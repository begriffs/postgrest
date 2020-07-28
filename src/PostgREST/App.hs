{-|
Module      : PostgREST.App
Description : PostgREST main application

This module is in charge of mapping HTTP requests to PostgreSQL queries.
Some of its functionality includes:

- Mapping HTTP request methods to proper SQL statements. For example, a GET request is translated to executing a SELECT query in a read-only TRANSACTION.
- Producing HTTP Headers according to RFCs.
- Content Negotiation
-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.App (
  postgrest
) where

import qualified Data.ByteString.Char8      as BS
import qualified Data.HashMap.Strict        as M
import qualified Data.List                  as L (union)
import qualified Data.Set                   as S
import qualified Hasql.Pool                 as P
import qualified Hasql.Transaction          as H
import qualified Hasql.Transaction          as HT
import qualified Hasql.Transaction.Sessions as HT

import Data.IORef             (IORef, readIORef)
import Data.Time.Clock        (UTCTime)
import Network.HTTP.Types.URI (renderSimpleQuery)

import Control.Applicative
import Data.Maybe
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai

import PostgREST.ApiRequest       (Action (..), ApiRequest (..),
                                   InvokeMethod (..), Target (..),
                                   mutuallyAgreeable, userApiRequest)
import PostgREST.Auth             (attemptJwtClaims, containsRole,
                                   jwtClaims)
import PostgREST.Config           (AppConfig (..))
import PostgREST.DbRequestBuilder (mutateRequest, readRequest,
                                   returningCols)
import PostgREST.DbStructure
import PostgREST.Error            (PgError (..), SimpleError (..),
                                   errorResponseFor, singularityError)
import PostgREST.Middleware
import PostgREST.OpenAPI
import PostgREST.Parsers          (pRequestColumns)
import PostgREST.QueryBuilder     (limitedQuery, mutateRequestToQuery,
                                   readRequestToCountQuery,
                                   readRequestToQuery,
                                   requestToCallProcQuery)
import PostgREST.RangeQuery       (allRange, contentRangeH,
                                   rangeStatusHeader)
import PostgREST.Statements       (callProcStatement,
                                   createExplainStatement,
                                   createReadStatement,
                                   createWriteStatement)
import PostgREST.Types
import Protolude                  hiding (Proxy, intercalate, toS)
import Protolude.Conv             (toS)

postgrest :: LogSetup -> IORef AppConfig -> IORef (Maybe DbStructure) -> P.Pool -> IO UTCTime -> IO () -> Application
postgrest logS refConf refDbStructure pool getTime connWorker =
  pgrstMiddleware logS $ \ req respond -> do
    time <- getTime
    body <- strictRequestBody req
    maybeDbStructure <- readIORef refDbStructure
    conf <- readIORef refConf
    case maybeDbStructure of
      Nothing -> respond . errorResponseFor $ ConnectionLostError
      Just dbStructure -> do
        response <- do
          let apiReq = userApiRequest (configSchemas conf) (configRootSpec conf) req body
              -- Need to parse ?columns early because findProc needs it to solve overloaded functions.
              apiReqCols = (,) <$> apiReq <*> (pRequestColumns . iColumns =<< apiReq)
          case apiReqCols of
            Left err -> return . errorResponseFor $ err
            Right (apiRequest, maybeCols) -> do
              -- The jwt must be checked before touching the db.
              attempt <- attemptJwtClaims (configJWKS conf) (configJwtAudience conf) (toS $ iJWT apiRequest) time (rightToMaybe $ configRoleClaimKey conf)
              case jwtClaims attempt of
                Left errJwt -> return . errorResponseFor $ errJwt
                Right claims -> do
                  let
                    authed = containsRole claims
                    cols = case (iPayload apiRequest, maybeCols) of
                      (Just ProcessedJSON{pjKeys}, _) -> pjKeys
                      (Just RawJSON{}, Just cls)      -> cls
                      _                               -> S.empty
                    proc = case iTarget apiRequest of
                      TargetProc qi _ -> findProc qi cols (iPreferParameters apiRequest == Just SingleObject) $ dbProcs dbStructure
                      _ -> Nothing
                    handleReq = runPgLocals conf claims (app dbStructure proc cols conf) apiRequest
                    txMode = transactionMode proc (iAction apiRequest)
                  dbResp <- P.use pool $ HT.transaction HT.ReadCommitted txMode handleReq
                  return $ either (errorResponseFor . PgError authed) identity dbResp
        -- Launch the connWorker when the connection is down. The postgrest function can respond successfully(with a stale schema cache) before the connWorker is done.
        when (responseStatus response == status503) connWorker
        respond response

transactionMode :: Maybe ProcDescription -> Action -> HT.Mode
transactionMode proc action =
  case action of
    ActionRead _         -> HT.Read
    ActionInfo           -> HT.Read
    ActionInspect _      -> HT.Read
    ActionInvoke InvGet  -> HT.Read
    ActionInvoke InvHead -> HT.Read
    ActionInvoke InvPost ->
      let v = maybe Volatile pdVolatility proc in
      if v == Stable || v == Immutable
         then HT.Read
         else HT.Write
    _ -> HT.Write

app :: DbStructure -> Maybe ProcDescription -> S.Set FieldName -> AppConfig -> ApiRequest -> H.Transaction Response
app dbStructure proc cols conf apiRequest =
  let rawContentTypes = (decodeContentType <$> configRawMediaTypes conf) `L.union` [ CTOctetStream, CTTextPlain ] in
  case responseContentTypeOrError (iAccepts apiRequest) rawContentTypes (iAction apiRequest) (iTarget apiRequest) of
    Left errorResponse -> return errorResponse
    Right contentType ->
      case (iAction apiRequest, iTarget apiRequest, iPayload apiRequest) of

        (ActionRead headersOnly, TargetIdent (QualifiedIdentifier tSchema tName), Nothing) ->
          case readSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (q, cq, bField, _) -> do
              let cQuery = if estimatedCount
                             then limitedQuery cq ((+ 1) <$> maxRows) -- LIMIT maxRows + 1 so we can determine below that maxRows was surpassed
                             else cq
                  stm = createReadStatement q cQuery (contentType == CTSingularJSON) shouldCount
                        (contentType == CTTextCSV) (contentType == CTGeoJSON) bField pgVer
                  explStm = createExplainStatement cq
              row <- H.statement () stm
              let (tableTotal, queryTotal, _ , body, gucHeaders, gucStatus) = row
                  gucs =  (,) <$> gucHeaders <*> gucStatus
              case gucs of
                Left err -> return $ errorResponseFor err
                Right (ghdrs, gstatus) -> do
                  total <- if | plannedCount   -> H.statement () explStm
                              | estimatedCount -> if tableTotal > (fromIntegral <$> maxRows)
                                                    then do estTotal <- H.statement () explStm
                                                            pure $ if estTotal > tableTotal then estTotal else tableTotal
                                                    else pure tableTotal
                              | otherwise      -> pure tableTotal
                  let (rangeStatus, contentRange) = rangeStatusHeader topLevelRange queryTotal total
                      status = fromMaybe rangeStatus gstatus
                      headers = addHeadersIfNotIncluded (catMaybes [
                                  Just $ toHeader contentType, Just contentRange,
                                  Just $ contentLocationH tName (iCanonicalQS apiRequest), profileH])
                                (unwrapGucHeader <$> ghdrs)
                      rBody = if headersOnly then mempty else toS body
                  return $
                    if contentType == CTSingularJSON && queryTotal /= 1
                      then errorResponseFor . singularityError $ queryTotal
                      else responseLBS status headers rBody

        (ActionCreate, TargetIdent (QualifiedIdentifier tSchema tName), Just pJson) ->
          case mutateSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let pkCols = tablePKCols dbStructure tSchema tName
                  stm = createWriteStatement sq mq
                    (contentType == CTSingularJSON) True
                    (contentType == CTTextCSV) (contentType == CTGeoJSON) (iPreferRepresentation apiRequest) pkCols pgVer
              row <- H.statement (toS $ pjRaw pJson) stm
              let (_, queryTotal, fields, body, gucHeaders, gucStatus) = row
                  gucs =  (,) <$> gucHeaders <*> gucStatus
              case gucs of
                Left err -> return $ errorResponseFor err
                Right (ghdrs, gstatus) -> do
                  let
                    (ctHeaders, rBody) = if iPreferRepresentation apiRequest == Full
                                          then ([Just $ toHeader contentType, profileH], toS body)
                                          else ([], mempty)
                    status = fromMaybe status201 gstatus
                    headers = addHeadersIfNotIncluded (catMaybes ([
                          if null fields
                            then Nothing
                            else Just $ locationH tName fields
                        , Just $ contentRangeH 1 0 $ if shouldCount then Just queryTotal else Nothing
                        , if null pkCols && isNothing (iOnConflict apiRequest)
                            then Nothing
                            else (\x -> ("Preference-Applied", encodeUtf8 (show x))) <$> iPreferResolution apiRequest
                        ] ++ ctHeaders)) (unwrapGucHeader <$> ghdrs)
                  if contentType == CTSingularJSON && queryTotal /= 1
                    then do
                      HT.condemn
                      return . errorResponseFor . singularityError $ queryTotal
                    else
                      return $ responseLBS status headers rBody

        (ActionUpdate, TargetIdent (QualifiedIdentifier tSchema tName), Just pJson) ->
          case mutateSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let stm = createWriteStatement sq mq
                    (contentType == CTSingularJSON) False (contentType == CTTextCSV) (contentType == CTGeoJSON)
                    (iPreferRepresentation apiRequest) [] pgVer
              row <- H.statement (toS $ pjRaw pJson) stm
              let (_, queryTotal, _, body, gucHeaders, gucStatus) = row
                  gucs =  (,) <$> gucHeaders <*> gucStatus
              case gucs of
                Left err -> return $ errorResponseFor err
                Right (ghdrs, gstatus) -> do
                  let
                    updateIsNoOp = S.null cols
                    defStatus | queryTotal == 0 && not updateIsNoOp      = status404
                              | iPreferRepresentation apiRequest == Full = status200
                              | otherwise                                = status204
                    status = fromMaybe defStatus gstatus
                    contentRangeHeader = contentRangeH 0 (queryTotal - 1) $ if shouldCount then Just queryTotal else Nothing
                    (ctHeaders, rBody) = if iPreferRepresentation apiRequest == Full
                                          then ([Just $ toHeader contentType, profileH], toS body)
                                          else ([], mempty)
                    headers = addHeadersIfNotIncluded (catMaybes ctHeaders ++ [contentRangeHeader]) (unwrapGucHeader <$> ghdrs)
                  if contentType == CTSingularJSON && queryTotal /= 1
                    then do
                      HT.condemn
                      return . errorResponseFor . singularityError $ queryTotal
                    else
                      return $ responseLBS status headers rBody

        (ActionSingleUpsert, TargetIdent (QualifiedIdentifier tSchema tName), Just pJson) ->
          case mutateSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (sq, mq) ->
              if topLevelRange /= allRange
                then return . errorResponseFor $ PutRangeNotAllowedError
              else do
                row <- H.statement (toS $ pjRaw pJson) $
                       createWriteStatement sq mq (contentType == CTSingularJSON) False
                                            (contentType == CTTextCSV) (contentType == CTGeoJSON) (iPreferRepresentation apiRequest) [] pgVer
                let (_, queryTotal, _, body, gucHeaders, gucStatus) = row
                    gucs =  (,) <$> gucHeaders <*> gucStatus
                case gucs of
                  Left err -> return $ errorResponseFor err
                  Right (ghdrs, gstatus) -> do
                    let headers = addHeadersIfNotIncluded (catMaybes [Just $ toHeader contentType, profileH]) (unwrapGucHeader <$> ghdrs)
                        (defStatus, rBody) = if iPreferRepresentation apiRequest == Full then (status200, toS body) else (status204, mempty)
                        status = fromMaybe defStatus gstatus
                    -- Makes sure the querystring pk matches the payload pk
                    -- e.g. PUT /items?id=eq.1 { "id" : 1, .. } is accepted, PUT /items?id=eq.14 { "id" : 2, .. } is rejected
                    -- If this condition is not satisfied then nothing is inserted, check the WHERE for INSERT in QueryBuilder.hs to see how it's done
                    if queryTotal /= 1
                      then do
                        HT.condemn
                        return . errorResponseFor $ PutMatchingPkError
                      else
                        return $ responseLBS status headers rBody

        (ActionDelete, TargetIdent (QualifiedIdentifier tSchema tName), Nothing) ->
          case mutateSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let stm = createWriteStatement sq mq
                    (contentType == CTSingularJSON) False
                    (contentType == CTTextCSV) (contentType == CTGeoJSON)
                    (iPreferRepresentation apiRequest) [] pgVer
              row <- H.statement mempty stm
              let (_, queryTotal, _, body, gucHeaders, gucStatus) = row
                  gucs =  (,) <$> gucHeaders <*> gucStatus
              case gucs of
                Left err -> return $ errorResponseFor err
                Right (ghdrs, gstatus) -> do
                  let
                    defStatus = if iPreferRepresentation apiRequest == Full then status200 else status204
                    status = fromMaybe defStatus gstatus
                    contentRangeHeader = contentRangeH 1 0 $ if shouldCount then Just queryTotal else Nothing
                    (ctHeaders, rBody) = if iPreferRepresentation apiRequest == Full
                                          then ([Just $ toHeader contentType, profileH], toS body)
                                          else ([], mempty)
                    headers = addHeadersIfNotIncluded (catMaybes ctHeaders ++ [contentRangeHeader]) (unwrapGucHeader <$> ghdrs)
                  if contentType == CTSingularJSON
                     && queryTotal /= 1
                    then do
                      HT.condemn
                      return . errorResponseFor . singularityError $ queryTotal
                    else
                      return $ responseLBS status headers rBody

        (ActionInfo, TargetIdent (QualifiedIdentifier tSchema tTable), Nothing) ->
          let mTable = find (\t -> tableName t == tTable && tableSchema t == tSchema) (dbTables dbStructure) in
          case mTable of
            Nothing -> return notFound
            Just table ->
              let allowH = (hAllow, if tableInsertable table then "GET,POST,PATCH,DELETE" else "GET")
                  allOrigins = ("Access-Control-Allow-Origin", "*") :: Header in
              return $ responseLBS status200 [allOrigins, allowH] mempty

        (ActionInvoke invMethod, TargetProc qi@(QualifiedIdentifier tSchema pName) _, Just pJson) ->
          let tName = fromMaybe pName $ procTableName =<< proc in
          case readSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (q, cq, bField, returning) -> do
              let
                preferParams = iPreferParameters apiRequest
                pq = requestToCallProcQuery qi (specifiedProcArgs cols proc) returnsScalar preferParams returning
                stm = callProcStatement returnsScalar pq q cq shouldCount (contentType == CTSingularJSON)
                        (contentType == CTTextCSV) (contentType == CTGeoJSON) (contentType `elem` rawContentTypes) (preferParams == Just MultipleObjects)
                        bField pgVer
              row <- H.statement (toS $ pjRaw pJson) stm
              let (tableTotal, queryTotal, body, gucHeaders, gucStatus) = row
                  gucs =  (,) <$> gucHeaders <*> gucStatus
              case gucs of
                Left err -> return $ errorResponseFor err
                Right (ghdrs, gstatus) -> do
                  let (rangeStatus, contentRange) = rangeStatusHeader topLevelRange queryTotal tableTotal
                      status = fromMaybe rangeStatus gstatus
                      headers = addHeadersIfNotIncluded
                        (catMaybes [Just $ toHeader contentType, Just contentRange, profileH])
                        (unwrapGucHeader <$> ghdrs)
                      rBody = if invMethod == InvHead then mempty else toS body
                  if contentType == CTSingularJSON && queryTotal /= 1
                    then do
                      HT.condemn
                      return . errorResponseFor . singularityError $ queryTotal
                    else
                      return $ responseLBS status headers rBody

        (ActionInspect headersOnly, TargetDefaultSpec tSchema, Nothing) -> do
          let host = configHost conf
              port = toInteger $ configPort conf
              proxy = pickProxy $ toS <$> configOpenAPIProxyUri conf
              uri Nothing = ("http", host, port, "/")
              uri (Just Proxy { proxyScheme = s, proxyHost = h, proxyPort = p, proxyPath = b }) = (s, h, p, b)
              uri' = uri proxy
              toTableInfo :: [Table] -> [(Table, [Column], [Text])]
              toTableInfo = map (\t -> let (s, tn) = (tableSchema t, tableName t) in (t, tableCols dbStructure s tn, tablePKCols dbStructure s tn))
              encodeApi ti sd procs = encodeOpenAPI (concat $ M.elems procs) (toTableInfo ti) uri' sd $ dbPrimaryKeys dbStructure

          body <- encodeApi <$>
            H.statement tSchema accessibleTables <*>
            H.statement tSchema schemaDescription <*>
            H.statement tSchema accessibleProcs
          return $ responseLBS status200 (catMaybes [Just $ toHeader CTOpenAPI, profileH]) (if headersOnly then mempty else toS body)

        _ -> return notFound

      where
        notFound = responseLBS status404 [] ""
        maxRows = configMaxRows conf
        exactCount = iPreferCount apiRequest == Just ExactCount
        estimatedCount = iPreferCount apiRequest == Just EstimatedCount
        plannedCount = iPreferCount apiRequest == Just PlannedCount
        shouldCount = exactCount || estimatedCount
        topLevelRange = iTopLevelRange apiRequest
        returnsScalar = maybe False procReturnsScalar proc
        pgVer = pgVersion dbStructure
        profileH = contentProfileH <$> iProfile apiRequest

        readSqlParts s t =
          let
            readReq = readRequest s t maxRows (dbRelations dbStructure) apiRequest
            returnings :: ReadRequest -> Either Response [FieldName]
            returnings rr = Right (returningCols rr [])
          in
          (,,,) <$>
          (readRequestToQuery <$> readReq) <*>
          (readRequestToCountQuery <$> readReq) <*>
          (binaryField contentType rawContentTypes returnsScalar =<< readReq) <*>
          (returnings =<< readReq)

        mutateSqlParts s t =
          let
            readReq = readRequest s t maxRows (dbRelations dbStructure) apiRequest
            mutReq = mutateRequest s t apiRequest cols (tablePKCols dbStructure s t) =<< readReq
          in
          (,) <$>
          (readRequestToQuery <$> readReq) <*>
          (mutateRequestToQuery <$> mutReq)

responseContentTypeOrError :: [ContentType] -> [ContentType] -> Action -> Target -> Either Response ContentType
responseContentTypeOrError accepts rawContentTypes action target = serves contentTypesForRequest accepts
  where
    contentTypesForRequest = case action of
      ActionRead _       ->  [CTApplicationJSON, CTSingularJSON, CTGeoJSON, CTTextCSV]
                             ++ rawContentTypes
      ActionCreate       ->  [CTApplicationJSON, CTSingularJSON, CTGeoJSON, CTTextCSV]
      ActionUpdate       ->  [CTApplicationJSON, CTSingularJSON, CTGeoJSON, CTTextCSV]
      ActionDelete       ->  [CTApplicationJSON, CTSingularJSON, CTGeoJSON, CTTextCSV]
      ActionInvoke _     ->  [CTApplicationJSON, CTSingularJSON, CTGeoJSON, CTTextCSV]
                             ++ rawContentTypes
                             ++ [CTOpenAPI | tpIsRootSpec target]
      ActionInspect _    ->  [CTOpenAPI, CTApplicationJSON]
      ActionInfo         ->  [CTTextCSV]
      ActionSingleUpsert ->  [CTApplicationJSON, CTSingularJSON, CTTextCSV, CTGeoJSON]
    serves sProduces cAccepts =
      case mutuallyAgreeable sProduces cAccepts of
        Nothing -> Left . errorResponseFor . ContentTypeError . map toMime $ cAccepts
        Just ct -> Right ct

{-
  | If raw(binary) output is requested, check that ContentType is one of the admitted rawContentTypes and that
  | `?select=...` contains only one field other than `*`
-}
binaryField :: ContentType -> [ContentType] -> Bool -> ReadRequest -> Either Response (Maybe FieldName)
binaryField ct rawContentTypes isScalarProc readReq
  | isScalarProc = Right Nothing
  | ct `elem` rawContentTypes =
      let fieldName = headMay fldNames in
      if length fldNames == 1 && fieldName /= Just "*"
        then Right fieldName
        else Left . errorResponseFor $ BinaryFieldError ct
  | otherwise = Right Nothing
  where
    fldNames = fstFieldNames readReq

locationH :: TableName -> [BS.ByteString] -> Header
locationH tName fields =
  let
    locationFields = renderSimpleQuery True $ splitKeyValue <$> fields
  in
    (hLocation, "/" <> toS tName <> locationFields)
  where
    splitKeyValue :: BS.ByteString -> (BS.ByteString, BS.ByteString)
    splitKeyValue kv =
      let (k, v) = BS.break (== '=') kv
      in (k, BS.tail v)

contentLocationH :: TableName -> ByteString -> Header
contentLocationH tName qString =
  ("Content-Location", "/" <> toS tName <> if BS.null qString then mempty else "?" <> toS qString)

contentProfileH :: Schema -> Header
contentProfileH schema =
   ("Content-Profile", toS schema)
