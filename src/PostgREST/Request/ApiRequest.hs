{-|
Module      : PostgREST.Request.ApiRequest
Description : PostgREST functions to translate HTTP request to a domain type called ApiRequest.
-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module PostgREST.Request.ApiRequest
  ( ApiRequest(..)
  , InvokeMethod(..)
  , ContentType(..)
  , Action(..)
  , Target(..)
  , PayloadJSON(..)
  , userApiRequest
  ) where

import qualified Data.Aeson           as JSON
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.Csv             as CSV
import qualified Data.HashMap.Strict  as M
import qualified Data.List            as L
import qualified Data.Set             as S
import qualified Data.Text            as T
import qualified Data.Vector          as V

import Control.Arrow             ((***))
import Data.Aeson.Types          (emptyArray, emptyObject)
import Data.List                 (last, lookup, partition, union)
import Data.List.NonEmpty        (head)
import Data.Maybe                (fromJust)
import Data.Ranged.Boundaries    (Boundary (..))
import Data.Ranged.Ranges        (Range (..), emptyRange,
                                  rangeIntersection)
import Network.HTTP.Base         (urlEncodeVars)
import Network.HTTP.Types.Header (hCookie)
import Network.HTTP.Types.URI    (parseQueryReplacePlus,
                                  parseSimpleQuery)
import Network.Wai               (Request (..))
import Network.Wai.Parse         (parseHttpAccept)
import Web.Cookie                (parseCookiesText)

import PostgREST.Config                  (AppConfig (..))
import PostgREST.ContentType             (ContentType (..))
import PostgREST.DbStructure             (DbStructure (..))
import PostgREST.DbStructure.Identifiers (FieldName,
                                          QualifiedIdentifier (..),
                                          Schema)
import PostgREST.DbStructure.Proc        (PgArg (..),
                                          ProcDescription (..),
                                          findProc)
import PostgREST.Error                   (ApiRequestError (..))
import PostgREST.Query.SqlFragment       (ftsOperators, operators)
import PostgREST.RangeQuery              (NonnegRange, allRange,
                                          rangeGeq, rangeLimit,
                                          rangeOffset, rangeRequested,
                                          restrictRange)
import PostgREST.Request.Parsers         (pRequestColumns)
import PostgREST.Request.Preferences     (PreferCount (..),
                                          PreferParameters (..),
                                          PreferRepresentation (..),
                                          PreferResolution (..),
                                          PreferTransaction (..))

import qualified PostgREST.ContentType as ContentType

import Protolude      hiding (head, toS)
import Protolude.Conv (toS)


type RequestBody = BL.ByteString

data PayloadJSON
  = ProcessedJSON -- ^ Cached attributes of a JSON payload
      { pjRaw  :: BL.ByteString
      -- ^ This is the raw ByteString that comes from the request body.  We
      -- cache this instead of an Aeson Value because it was detected that for
      -- large payloads the encoding had high memory usage, see
      -- https://github.com/PostgREST/postgrest/pull/1005 for more details
      , pjKeys :: S.Set Text
      -- ^ Keys of the object or if it's an array these keys are guaranteed to
      -- be the same across all its objects
      }
  | RawJSON { pjRaw  :: BL.ByteString }

data InvokeMethod = InvHead | InvGet | InvPost deriving Eq
-- | Types of things a user wants to do to tables/views/procs
data Action = ActionCreate       | ActionRead{isHead :: Bool}
            | ActionUpdate       | ActionDelete
            | ActionSingleUpsert | ActionInvoke InvokeMethod
            | ActionInfo         | ActionInspect{isHead :: Bool}
            deriving Eq
-- | The target db object of a user action
data Target = TargetIdent QualifiedIdentifier
            | TargetProc{tProc :: ProcDescription, tpIsRootSpec :: Bool}
            | TargetDefaultSpec{tdsSchema :: Schema} -- The default spec offered at root "/"
            | TargetUnknown

-- | RPC query param value `/rpc/func?v=<value>`, used for VARIADIC functions on form-urlencoded POST and GETs
-- | It can be fixed `?v=1` or repeated `?v=1&v=2&v=3.
data RpcParamValue = Fixed Text | Variadic [Text]
instance JSON.ToJSON RpcParamValue where
  toJSON (Fixed    v) = JSON.toJSON v
  toJSON (Variadic v) = JSON.toJSON v

toRpcParamValue :: ProcDescription -> (Text, Text) -> (Text, RpcParamValue)
toRpcParamValue proc (k, v) | argIsVariadic k = (k, Variadic [v])
                            | otherwise       = (k, Fixed v)
  where
    argIsVariadic arg = isJust $ find (\PgArg{pgaName, pgaVar} -> pgaName == arg && pgaVar) $ pdArgs proc

-- | Convert rpc params `/rpc/func?a=val1&b=val2` to json `{"a": "val1", "b": "val2"}
jsonRpcParams :: ProcDescription -> [(Text, Text)] -> PayloadJSON
jsonRpcParams proc prms =
  if not $ pdHasVariadic proc then -- if proc has no variadic arg, save steps and directly convert to json
    ProcessedJSON (JSON.encode $ M.fromList $ second JSON.toJSON <$> prms) (S.fromList $ fst <$> prms)
  else
    let paramsMap = M.fromListWith mergeParams $ toRpcParamValue proc <$> prms in
    ProcessedJSON (JSON.encode paramsMap) (S.fromList $ M.keys paramsMap)
  where
    mergeParams :: RpcParamValue -> RpcParamValue -> RpcParamValue
    mergeParams (Variadic a) (Variadic b) = Variadic $ b ++ a
    mergeParams v _                       = v -- repeated params for non-variadic arguments are not merged

{-|
  Describes what the user wants to do. This data type is a
  translation of the raw elements of an HTTP request into domain
  specific language.  There is no guarantee that the intent is
  sensible, it is up to a later stage of processing to determine
  if it is an action we are able to perform.
-}
data ApiRequest = ApiRequest {
    iAction               :: Action                           -- ^ Similar but not identical to HTTP verb, e.g. Create/Invoke both POST
  , iRange                :: M.HashMap ByteString NonnegRange -- ^ Requested range of rows within response
  , iTopLevelRange        :: NonnegRange                      -- ^ Requested range of rows from the top level
  , iTarget               :: Target                           -- ^ The target, be it calling a proc or accessing a table
  , iPayload              :: Maybe PayloadJSON                -- ^ Data sent by client and used for mutation actions
  , iPreferRepresentation :: PreferRepresentation             -- ^ If client wants created items echoed back
  , iPreferParameters     :: Maybe PreferParameters           -- ^ How to pass parameters to a stored procedure
  , iPreferCount          :: Maybe PreferCount                -- ^ Whether the client wants a result count
  , iPreferResolution     :: Maybe PreferResolution           -- ^ Whether the client wants to UPSERT or ignore records on PK conflict
  , iPreferTransaction    :: Maybe PreferTransaction          -- ^ Whether the clients wants to commit or rollback the transaction
  , iFilters              :: [(Text, Text)]                   -- ^ Filters on the result ("id", "eq.10")
  , iLogic                :: [(Text, Text)]                   -- ^ &and and &or parameters used for complex boolean logic
  , iSelect               :: Maybe Text                       -- ^ &select parameter used to shape the response
  , iOnConflict           :: Maybe Text                       -- ^ &on_conflict parameter used to upsert on specific unique keys
  , iColumns              :: S.Set FieldName                  -- ^ parsed colums from &columns parameter and payload
  , iOrder                :: [(Text, Text)]                   -- ^ &order parameters for each level
  , iCanonicalQS          :: ByteString                       -- ^ Alphabetized (canonical) request query string for response URLs
  , iHeaders              :: [(Text, Text)]                   -- ^ HTTP request headers
  , iCookies              :: [(Text, Text)]                   -- ^ Request Cookies
  , iPath                 :: ByteString                       -- ^ Raw request path
  , iMethod               :: ByteString                       -- ^ Raw request method
  , iProfile              :: Maybe Schema                     -- ^ The request profile for enabling use of multiple schemas. Follows the spec in hhttps://www.w3.org/TR/dx-prof-conneg/ttps://www.w3.org/TR/dx-prof-conneg/.
  , iSchema               :: Schema                           -- ^ The request schema. Can vary depending on iProfile.
  , iAcceptContentType    :: ContentType
  }

-- | Examines HTTP request and translates it into user intent.
userApiRequest :: AppConfig -> DbStructure -> Request -> RequestBody -> Either ApiRequestError ApiRequest
userApiRequest conf@AppConfig{..} dbStructure req reqBody
  | isJust profile && fromJust profile `notElem` configDbSchemas = Left $ UnacceptableSchema $ toList configDbSchemas
  | isTargetingProc && method `notElem` ["HEAD", "GET", "POST"] = Left ActionInappropriate
  | topLevelRange == emptyRange = Left InvalidRange
  | action == ActionSingleUpsert && topLevelRange /= allRange = Left PutRangeNotAllowedError
  | shouldParsePayload && isLeft payload = either (Left . InvalidBody . toS) witness payload
  | isLeft parsedColumns = either Left witness parsedColumns
  | otherwise = do
     acceptContentType <- findAcceptContentType conf action target accepts
     return ApiRequest {
      iAction = action
      , iTarget = target
      , iRange = ranges
      , iTopLevelRange = topLevelRange
      , iPayload = relevantPayload
      , iPreferRepresentation = representation
      , iPreferParameters  = if | hasPrefer (show SingleObject)     -> Just SingleObject
                                | hasPrefer (show MultipleObjects)  -> Just MultipleObjects
                                | otherwise                         -> Nothing
      , iPreferCount       = if | hasPrefer (show ExactCount)       -> Just ExactCount
                                | hasPrefer (show PlannedCount)     -> Just PlannedCount
                                | hasPrefer (show EstimatedCount)   -> Just EstimatedCount
                                | otherwise                         -> Nothing
      , iPreferResolution  = if | hasPrefer (show MergeDuplicates)  -> Just MergeDuplicates
                                | hasPrefer (show IgnoreDuplicates) -> Just IgnoreDuplicates
                                | otherwise                         -> Nothing
      , iPreferTransaction = if | hasPrefer (show Commit)           -> Just Commit
                                | hasPrefer (show Rollback)         -> Just Rollback
                                | otherwise                         -> Nothing
      , iFilters = filters
      , iLogic = [(toS k, toS $ fromJust v) | (k,v) <- qParams, isJust v, endingIn ["and", "or"] k ]
      , iSelect = toS <$> join (lookup "select" qParams)
      , iOnConflict = toS <$> join (lookup "on_conflict" qParams)
      , iColumns = payloadColumns
      , iOrder = [(toS k, toS $ fromJust v) | (k,v) <- qParams, isJust v, endingIn ["order"] k ]
      , iCanonicalQS = toS $ urlEncodeVars
        . L.sortOn fst
        . map (join (***) toS . second (fromMaybe BS.empty))
        $ qString
      , iHeaders = [ (toS $ CI.foldedCase k, toS v) | (k,v) <- hdrs, k /= hCookie]
      , iCookies = maybe [] parseCookiesText $ lookupHeader "Cookie"
      , iPath = rawPathInfo req
      , iMethod = method
      , iProfile = profile
      , iSchema = schema
      , iAcceptContentType = acceptContentType
      }
 where
  accepts = maybe [CTAny] (map ContentType.decodeContentType . parseHttpAccept) $ lookupHeader "accept"
  -- queryString with '+' converted to ' '(space)
  qString = parseQueryReplacePlus True $ rawQueryString req
  -- rpcQParams = Rpc query params e.g. /rpc/name?param1=val1, similar to filter but with no operator(eq, lt..)
  (filters, rpcQParams) =
    case action of
      ActionInvoke InvGet  -> partitionFlts
      ActionInvoke InvHead -> partitionFlts
      _                    -> (flts, [])
  partitionFlts = partition (liftM2 (||) (isEmbedPath . fst) (hasOperator . snd)) flts
  flts =
    [ (toS k, toS $ fromJust v) |
      (k,v) <- qParams, isJust v,
      k `notElem` ["select", "columns"],
      not (endingIn ["order", "limit", "offset", "and", "or"] k) ]
  hasOperator val = any (`T.isPrefixOf` val) $
                      ((<> ".") <$> "not":M.keys operators) ++
                      ((<> "(") <$> M.keys ftsOperators)
  isEmbedPath = T.isInfixOf "."
  isTargetingProc = case target of
    TargetProc _ _ -> True
    _              -> False
  isTargetingDefaultSpec = case target of
    TargetDefaultSpec _ -> True
    _                   -> False
  contentType = ContentType.decodeContentType . fromMaybe "application/json" $ lookupHeader "content-type"
  columns
    | action `elem` [ActionCreate, ActionUpdate, ActionInvoke InvPost] = toS <$> join (lookup "columns" qParams)
    | otherwise = Nothing
  parsedColumns = pRequestColumns columns
  payloadColumns =
    case (contentType, action) of
      (_, ActionInvoke InvGet)  -> S.fromList $ fst <$> rpcQParams
      (_, ActionInvoke InvHead) -> S.fromList $ fst <$> rpcQParams
      (CTUrlEncoded, _)         -> S.fromList $ map (toS . fst) $ parseSimpleQuery $ toS reqBody
      _ -> case (relevantPayload, fromRight Nothing parsedColumns) of
        (Just ProcessedJSON{pjKeys}, _) -> pjKeys
        (Just RawJSON{}, Just cls)      -> cls
        _                               -> S.empty
  payload = case contentType of
    CTApplicationJSON ->
      if isJust columns
        then Right $ RawJSON reqBody
        else note "All object keys must match" . payloadAttributes reqBody
               =<< if BL.null reqBody && isTargetingProc
                     then Right emptyObject
                     else JSON.eitherDecode reqBody
    CTTextCSV -> do
      json <- csvToJson <$> CSV.decodeByName reqBody
      note "All lines must have same number of fields" $ payloadAttributes (JSON.encode json) json
    CTUrlEncoded ->
      let urlEncodedBody = parseSimpleQuery $ toS reqBody in
      case target of
        TargetProc{tProc} ->
          Right $ jsonRpcParams tProc $ (toS *** toS) <$> urlEncodedBody
        _ ->
          let paramsMap = M.fromList $ (toS *** JSON.String . toS) <$> urlEncodedBody in
          Right $ ProcessedJSON (JSON.encode paramsMap) $ S.fromList (M.keys paramsMap)
    ct ->
      Left $ toS $ "Content-Type not acceptable: " <> ContentType.toMime ct
  topLevelRange = fromMaybe allRange $ M.lookup "limit" ranges -- if no limit is specified, get all the request rows
  action =
    case method of
      -- The HEAD method is identical to GET except that the server MUST NOT return a message-body in the response
      -- From https://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.4
      "HEAD"     | isTargetingDefaultSpec -> ActionInspect{isHead=True}
                 | isTargetingProc        -> ActionInvoke InvHead
                 | otherwise              -> ActionRead{isHead=True}
      "GET"      | isTargetingDefaultSpec -> ActionInspect{isHead=False}
                 | isTargetingProc        -> ActionInvoke InvGet
                 | otherwise              -> ActionRead{isHead=False}
      "POST"    -> if isTargetingProc
                    then ActionInvoke InvPost
                    else ActionCreate
      "PATCH"   -> ActionUpdate
      "PUT"     -> ActionSingleUpsert
      "DELETE"  -> ActionDelete
      "OPTIONS" -> ActionInfo
      _         -> ActionInspect{isHead=False}

  defaultSchema = head configDbSchemas
  profile
    | length configDbSchemas <= 1 -- only enable content negotiation by profile when there are multiple schemas specified in the config
      = Nothing
    | otherwise = case action of
        -- POST/PATCH/PUT/DELETE don't use the same header as per the spec
        ActionCreate         -> contentProfile
        ActionUpdate         -> contentProfile
        ActionSingleUpsert   -> contentProfile
        ActionDelete         -> contentProfile
        ActionInvoke InvPost -> contentProfile
        _                    -> acceptProfile
    where
      contentProfile = Just $ maybe defaultSchema toS $ lookupHeader "Content-Profile"
      acceptProfile = Just $ maybe defaultSchema toS $ lookupHeader "Accept-Profile"
  schema = fromMaybe defaultSchema profile
  target =
    let
      callFindProc proc = findProc (QualifiedIdentifier schema proc) payloadColumns (hasPrefer (show SingleObject)) $ dbProcs dbStructure
    in
    case path of
      []             -> case configDbRootSpec of
                        Just pName -> TargetProc (callFindProc pName) True
                        Nothing    -> TargetDefaultSpec schema
      [table]        -> TargetIdent $ QualifiedIdentifier schema table
      ["rpc", pName] -> TargetProc (callFindProc pName) False
      _              -> TargetUnknown

  shouldParsePayload = action `elem` [ActionCreate, ActionUpdate, ActionSingleUpsert, ActionInvoke InvPost]
  relevantPayload = case (target, action) of
    -- Though ActionInvoke GET/HEAD doesn't really have a payload, we use the payload variable as a way
    -- to store the query string arguments to the function.
    (TargetProc{tProc}, ActionInvoke InvGet)  -> Just $ jsonRpcParams tProc rpcQParams
    (TargetProc{tProc}, ActionInvoke InvHead) -> Just $ jsonRpcParams tProc rpcQParams
    _ | shouldParsePayload                    -> rightToMaybe payload
      | otherwise                             -> Nothing
  path            = pathInfo req
  method          = requestMethod req
  hdrs            = requestHeaders req
  qParams         = [(toS k, v)|(k,v) <- qString]
  lookupHeader    = flip lookup hdrs
  hasPrefer :: Text -> Bool
  hasPrefer val   = any (\(h,v) -> h == "Prefer" && val `elem` split v) hdrs
    where
        split :: BS.ByteString -> [Text]
        split = map T.strip . T.split (==',') . toS
  representation
    | hasPrefer (show Full)        = Full
    | hasPrefer (show None)        = None
    | hasPrefer (show HeadersOnly) = HeadersOnly
    | otherwise                    = None
  endingIn:: [Text] -> Text -> Bool
  endingIn xx key = lastWord `elem` xx
    where lastWord = last $ T.split (=='.') key

  headerRange = rangeRequested hdrs
  replaceLast x s = T.intercalate "." $ L.init (T.split (=='.') s) ++ [x]
  limitParams :: M.HashMap ByteString NonnegRange
  limitParams  = M.fromList [(toS (replaceLast "limit" k), restrictRange (readMaybe =<< (toS <$> v)) allRange) | (k,v) <- qParams, isJust v, endingIn ["limit"] k]
  offsetParams :: M.HashMap ByteString NonnegRange
  offsetParams = M.fromList [(toS (replaceLast "limit" k), maybe allRange rangeGeq (readMaybe =<< (toS <$> v))) | (k,v) <- qParams, isJust v, endingIn ["offset"] k]

  urlRange = M.unionWith f limitParams offsetParams
    where
      f rl ro = Range (BoundaryBelow o) (BoundaryAbove $ o + l - 1)
        where
          l = fromMaybe 0 $ rangeLimit rl
          o = rangeOffset ro
  ranges = M.insert "limit" (rangeIntersection headerRange (fromMaybe allRange (M.lookup "limit" urlRange))) urlRange

{-|
  Find the best match from a list of content types accepted by the
  client in order of decreasing preference and a list of types
  producible by the server.  If there is no match but the client
  accepts */* then return the top server pick.
-}
mutuallyAgreeable :: [ContentType] -> [ContentType] -> Maybe ContentType
mutuallyAgreeable sProduces cAccepts =
  let exact = listToMaybe $ L.intersect cAccepts sProduces in
  if isNothing exact && CTAny `elem` cAccepts
     then listToMaybe sProduces
     else exact

type CsvData = V.Vector (M.HashMap Text BL.ByteString)

{-|
  Converts CSV like
  a,b
  1,hi
  2,bye

  into a JSON array like
  [ {"a": "1", "b": "hi"}, {"a": 2, "b": "bye"} ]

  The reason for its odd signature is so that it can compose
  directly with CSV.decodeByName
-}
csvToJson :: (CSV.Header, CsvData) -> JSON.Value
csvToJson (_, vals) =
  JSON.Array $ V.map rowToJsonObj vals
 where
  rowToJsonObj = JSON.Object .
    M.map (\str ->
        if str == "NULL"
          then JSON.Null
          else JSON.String $ toS str
      )

payloadAttributes :: RequestBody -> JSON.Value -> Maybe PayloadJSON
payloadAttributes raw json =
  -- Test that Array contains only Objects having the same keys
  case json of
    JSON.Array arr ->
      case arr V.!? 0 of
        Just (JSON.Object o) ->
          let canonicalKeys = S.fromList $ M.keys o
              areKeysUniform = all (\case
                JSON.Object x -> S.fromList (M.keys x) == canonicalKeys
                _ -> False) arr in
          if areKeysUniform
            then Just $ ProcessedJSON raw canonicalKeys
            else Nothing
        Just _ -> Nothing
        Nothing -> Just emptyPJArray

    JSON.Object o -> Just $ ProcessedJSON raw (S.fromList $ M.keys o)

    -- truncate everything else to an empty array.
    _ -> Just emptyPJArray
  where
    emptyPJArray = ProcessedJSON (JSON.encode emptyArray) S.empty

findAcceptContentType :: AppConfig -> Action -> Target -> [ContentType] -> Either ApiRequestError ContentType
findAcceptContentType conf action target accepts =
  case mutuallyAgreeable (requestContentTypes conf action target) accepts of
    Just ct ->
      Right ct
    Nothing ->
      Left . ContentTypeError $ map ContentType.toMime accepts

requestContentTypes :: AppConfig -> Action -> Target -> [ContentType]
requestContentTypes conf action target =
  case action of
    ActionRead _    -> defaultContentTypes ++ rawContentTypes conf
    ActionInvoke _  -> invokeContentTypes
    ActionInspect _ -> [CTOpenAPI, CTApplicationJSON]
    ActionInfo      -> [CTTextCSV]
    _               -> defaultContentTypes
  where
    invokeContentTypes =
      defaultContentTypes
        ++ rawContentTypes conf
        ++ [CTOpenAPI | tpIsRootSpec target]
    defaultContentTypes =
      [CTApplicationJSON, CTSingularJSON, CTTextCSV]

rawContentTypes :: AppConfig -> [ContentType]
rawContentTypes AppConfig{..} =
  (ContentType.decodeContentType <$> configRawMediaTypes) `union` [CTOctetStream, CTTextPlain]
