{-# LANGUAGE RecordWildCards #-}
module PostgREST.Query
  ( createQuery
  , deleteQuery
  , invokeQuery
  , openApiQuery
  , readQuery
  , singleUpsertQuery
  , txMode
  , updateQuery
  , DbHandler
  , ReadQueryResult(..)
  , MutateQueryResult(..)
  , InvokeQueryResult(..)
  , OpenApiQueryResult
  ) where

import Control.Monad.Except (liftEither)

import qualified Data.ByteString.Char8      as BS8
import qualified Hasql.Transaction          as SQL
import qualified Hasql.Transaction.Sessions as SQL
import qualified Network.HTTP.Types.Status  as HTTP

import qualified PostgREST.DbStructure              as DbStructure
import qualified PostgREST.DbStructure.Proc         as Proc
import qualified PostgREST.Error                    as Error
import qualified PostgREST.Query.QueryBuilder       as QueryBuilder
import qualified PostgREST.Query.Statements         as Statements
import qualified PostgREST.Request.ApiRequest       as ApiRequest
import qualified PostgREST.Request.DbRequestBuilder as ReqBuilder

import PostgREST.Config                  (AppConfig (..))
import PostgREST.ContentType             (ContentType (..))
import PostgREST.DbStructure             (tablePKCols)
import PostgREST.DbStructure.Identifiers (QualifiedIdentifier (..),
                                          Schema)
import PostgREST.DbStructure.Proc        (ProcDescription (..),
                                          ProcVolatility (..))
import PostgREST.DbStructure.Table       (Table)
import PostgREST.Error                   (Error)
import PostgREST.GucHeader               (GucHeader)
import PostgREST.Request                 (InvokeRequestInfo (..),
                                          MutateRequestInfo (..),
                                          ReadRequestInfo (..))
import PostgREST.Request.ApiRequest      (Action (..),
                                          ApiRequest (..),
                                          InvokeMethod (..),
                                          Target (..))
import PostgREST.Request.Preferences     (PreferCount (..),
                                          PreferParameters (..))

import Protolude hiding (Handler)


type DbHandler = ExceptT Error SQL.Transaction

data ReadQueryResult = ReadQueryResult
  { rqRequest    :: ReadRequestInfo
  , rqQueryTotal :: Int64
  , rqBody       :: BS8.ByteString
  , rqTableTotal :: Maybe Int64
  , rqGucHeaders :: [GucHeader]
  , rqGucStatus  :: Maybe HTTP.Status
  }

data MutateQueryResult = MutateQueryResult
  { resRequest    :: MutateRequestInfo
  , resQueryTotal :: Int64
  , resFields     :: [ByteString]
  , resBody       :: ByteString
  , resGucStatus  :: Maybe HTTP.Status
  , resGucHeaders :: [GucHeader]
  }

data InvokeQueryResult = InvokeQueryResult
  { iqRequest    :: InvokeRequestInfo
  , iqTableTotal :: Maybe Int64
  , iqQueryTotal :: Int64
  , iqBody       :: BS8.ByteString
  , iqGucHeaders :: [GucHeader]
  , iqGucStatus  :: Maybe HTTP.Status
  }

type OpenApiQueryResult = ([Table], Maybe Text, Proc.ProcsMap)


readQuery :: ReadRequestInfo -> DbHandler ReadQueryResult
readQuery reqInfo@ReadRequestInfo{..} = do
  (tableTotal, queryTotal, _ , body, gucHeaders, gucStatus) <-
    lift . SQL.statement mempty $
      Statements.createReadStatement
        (QueryBuilder.readRequestToQuery rrReadRequest)
        (if iPreferCount == Just EstimatedCount then
           -- LIMIT maxRows + 1 so we can determine below that maxRows was surpassed
           QueryBuilder.limitedQuery countQuery ((+ 1) <$> configDbMaxRows)
         else
           countQuery
        )
        (iAcceptContentType == CTSingularJSON)
        (shouldCount iPreferCount)
        (iAcceptContentType == CTTextCSV)
        rrBinaryField
        rrPgVersion
      configDbPreparedStatements

  total <- readTotal tableTotal countQuery
  failNotSingular iAcceptContentType queryTotal
  liftEither $ ReadQueryResult reqInfo queryTotal body total <$> gucHeaders <*> gucStatus
  where
    ApiRequest{..} = rrApiRequest
    AppConfig{..} = rrConfig
    countQuery = QueryBuilder.readRequestToCountQuery rrReadRequest
    readTotal tableTotal countQry =
      case iPreferCount of
        Just PlannedCount ->
          explain
        Just EstimatedCount ->
          if tableTotal > (fromIntegral <$> configDbMaxRows) then
            max tableTotal <$> explain
          else
            return tableTotal
        _ ->
          return tableTotal
      where
        explain =
          lift . SQL.statement mempty $
            Statements.createExplainStatement countQry configDbPreparedStatements

createQuery :: MutateRequestInfo -> DbHandler MutateQueryResult
createQuery mutReq@MutateRequestInfo{..} = do
  result <- mutateQuery True pkCols mutReq
  failNotSingular (iAcceptContentType mrApiRequest) (resQueryTotal result)
  return result
  where
    pkCols = tablePKCols mrDbStructure qiSchema qiName
    QualifiedIdentifier{..} = mrIdentifier

updateQuery :: MutateRequestInfo -> DbHandler MutateQueryResult
updateQuery mutReq@MutateRequestInfo{..} = do
  result <- mutateQuery False mempty mutReq
  failNotSingular (iAcceptContentType mrApiRequest) (resQueryTotal result)
  return result

singleUpsertQuery :: MutateRequestInfo -> DbHandler MutateQueryResult
singleUpsertQuery mutReq = do
  result <- mutateQuery False mempty mutReq

  -- Makes sure the querystring pk matches the payload pk
  -- e.g. PUT /items?id=eq.1 { "id" : 1, .. } is accepted,
  -- PUT /items?id=eq.14 { "id" : 2, .. } is rejected.
  -- If this condition is not satisfied then nothing is inserted,
  -- check the WHERE for INSERT in QueryBuilder.hs to see how it's done
  when (resQueryTotal result /= 1) $ do
    lift SQL.condemn
    throwError Error.PutMatchingPkError

  return result

deleteQuery :: MutateRequestInfo -> DbHandler MutateQueryResult
deleteQuery mutReq@MutateRequestInfo{..} = do
  result <- mutateQuery False mempty mutReq
  failNotSingular (iAcceptContentType mrApiRequest) (resQueryTotal result)
  return result

mutateQuery :: Bool -> [Text] -> MutateRequestInfo -> DbHandler MutateQueryResult
mutateQuery isInsert pkCols mr@MutateRequestInfo{..} = do
  (_, queryTotal, fields, body, gucHeaders, gucStatus) <-
    lift . SQL.statement mempty $
      Statements.createWriteStatement
        (QueryBuilder.readRequestToQuery mrReadRequest)
        (QueryBuilder.mutateRequestToQuery mrMutateRequest)
        (iAcceptContentType mrApiRequest == CTSingularJSON)
        isInsert
        (iAcceptContentType mrApiRequest == CTTextCSV)
        (iPreferRepresentation mrApiRequest)
        pkCols
        mrPgVersion
        (configDbPreparedStatements mrConfig)

  liftEither $ MutateQueryResult mr queryTotal fields body <$> gucStatus <*> gucHeaders

invokeQuery :: InvokeRequestInfo -> DbHandler InvokeQueryResult
invokeQuery ir@InvokeRequestInfo{..} = do
  (tableTotal, queryTotal, body, gucHeaders, gucStatus) <-
    lift . SQL.statement mempty $
      Statements.callProcStatement
        (returnsScalar $ iTarget irApiRequest)
        (returnsSingle $ iTarget irApiRequest)
        (QueryBuilder.requestToCallProcQuery
          (QualifiedIdentifier (pdSchema irProc) (pdName irProc))
          (Proc.specifiedProcArgs (iColumns irApiRequest) irProc)
          (iPayload irApiRequest)
          (returnsScalar $ iTarget irApiRequest)
          (iPreferParameters irApiRequest)
          (ReqBuilder.returningCols irReadRequest [])
        )
        (QueryBuilder.readRequestToQuery irReadRequest)
        (QueryBuilder.readRequestToCountQuery irReadRequest)
        (shouldCount $ iPreferCount irApiRequest)
        (iAcceptContentType irApiRequest == CTSingularJSON)
        (iAcceptContentType irApiRequest == CTTextCSV)
        (iPreferParameters irApiRequest == Just MultipleObjects)
        irBinaryField
        irPgVersion
      (configDbPreparedStatements irConfig)

  failNotSingular (iAcceptContentType irApiRequest) queryTotal
  liftEither $ InvokeQueryResult ir tableTotal queryTotal body <$> gucHeaders <*> gucStatus
  where
    returnsSingle (ApiRequest.TargetProc target _) = Proc.procReturnsSingle target
    returnsSingle _                                = False

openApiQuery :: Schema -> AppConfig -> DbHandler OpenApiQueryResult
openApiQuery tSchema AppConfig{..} = do
  lift $ (,,)
    <$> SQL.statement tSchema (DbStructure.accessibleTables configDbPreparedStatements)
    <*> SQL.statement tSchema (DbStructure.schemaDescription configDbPreparedStatements)
    <*> SQL.statement tSchema (DbStructure.accessibleProcs configDbPreparedStatements)

txMode :: ApiRequest -> SQL.Mode
txMode ApiRequest{..} =
  case (iAction, iTarget) of
    (ActionRead _, _) ->
      SQL.Read
    (ActionInfo, _) ->
      SQL.Read
    (ActionInspect _, _) ->
      SQL.Read
    (ActionInvoke InvGet, _) ->
      SQL.Read
    (ActionInvoke InvHead, _) ->
      SQL.Read
    (ActionInvoke InvPost, TargetProc ProcDescription{pdVolatility=Stable} _) ->
      SQL.Read
    (ActionInvoke InvPost, TargetProc ProcDescription{pdVolatility=Immutable} _) ->
      SQL.Read
    _ ->
      SQL.Write

-- | Fail a response if a single JSON object was requested and not exactly one
-- was found.
failNotSingular :: ContentType -> Int64 -> DbHandler ()
failNotSingular contentType queryTotal =
  when (contentType == CTSingularJSON && queryTotal /= 1) $ do
    lift SQL.condemn
    throwError $ Error.singularityError queryTotal

shouldCount :: Maybe PreferCount -> Bool
shouldCount preferCount =
  preferCount == Just ExactCount || preferCount == Just EstimatedCount

returnsScalar :: ApiRequest.Target -> Bool
returnsScalar (TargetProc proc _) = Proc.procReturnsScalar proc
returnsScalar _                   = False
