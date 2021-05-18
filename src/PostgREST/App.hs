{-|
Module      : PostgREST.App
Description : PostgREST main application
-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.App
  ( SignalHandlerInstaller
  , SocketRunner
  , postgrest
  , run
  ) where

import Control.Monad.Except     (liftEither)
import Data.Either.Combinators  (mapLeft)
import Data.String              (IsString (..))
import Data.Time.Clock          (UTCTime)
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort,
                                 setServerName)
import System.Posix.Types       (FileMode)

import qualified Hasql.Pool                 as SQL
import qualified Hasql.Transaction.Sessions as SQL
import qualified Network.HTTP.Types.Status  as HTTP
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.Warp   as Warp

import qualified PostgREST.AppState   as AppState
import qualified PostgREST.Auth       as Auth
import qualified PostgREST.Error      as Error
import qualified PostgREST.Middleware as Middleware
import qualified PostgREST.Query      as Query
import qualified PostgREST.Request    as Request
import qualified PostgREST.Response   as Response

import PostgREST.AppState                (AppState)
import PostgREST.Config                  (AppConfig (..),
                                          LogLevel (..))
import PostgREST.DbStructure             (DbStructure (..),
                                          tablePKCols)
import PostgREST.DbStructure.Identifiers (QualifiedIdentifier (..))
import PostgREST.DbStructure.PgVersion   (PgVersion (..))
import PostgREST.DbStructure.Table       (Table (..))
import PostgREST.Error                   (Error)
import PostgREST.Query                   (DbHandler)
import PostgREST.Request                 (Request (..))
import PostgREST.Version                 (prettyVersion)
import PostgREST.Workers                 (connectionWorker, listener)

import Protolude      hiding (Handler, toS)
import Protolude.Conv (toS)


type Handler = ExceptT Error
type SignalHandlerInstaller = AppState -> IO()
type SocketRunner = Warp.Settings -> Wai.Application -> FileMode -> FilePath -> IO()

run :: SignalHandlerInstaller -> Maybe SocketRunner -> AppState -> IO ()
run installHandlers maybeRunWithSocket appState = do
  conf@AppConfig{..} <- AppState.getConfig appState
  connectionWorker appState -- Loads the initial DbStructure
  installHandlers appState
  -- reload schema cache + config on NOTIFY
  when configDbChannelEnabled $ listener appState

  let app = postgrest configLogLevel appState (connectionWorker appState)

  case configServerUnixSocket of
    Just socket ->
      -- run the postgrest application with user defined socket. Only for UNIX systems
      case maybeRunWithSocket of
        Just runWithSocket ->
          runWithSocket (serverSettings conf) app configServerUnixSocketMode socket
        Nothing ->
          panic "Cannot run with socket on non-unix plattforms."
    Nothing -> do
      putStrLn $ ("Listening on port " :: Text) <> show configServerPort
      Warp.runSettings (serverSettings conf) app

serverSettings :: AppConfig -> Warp.Settings
serverSettings AppConfig{..} =
  defaultSettings
    & setHost (fromString $ toS configServerHost)
    & setPort configServerPort
    & setServerName (toS $ "postgrest/" <> prettyVersion)

-- | PostgREST application
postgrest :: LogLevel -> AppState.AppState -> IO () -> Wai.Application
postgrest logLev appState connWorker =
  Middleware.pgrstMiddleware logLev $
    \waiRequest waiRespond -> do
      time <- AppState.getTime appState
      conf <- AppState.getConfig appState
      maybeDbStructure <- AppState.getDbStructure appState
      pgVer <- AppState.getPgVersion appState

      let
        eitherResponse :: IO (Either Error Wai.Response)
        eitherResponse =
          runExceptT $ postgrestResponse conf pgVer maybeDbStructure (AppState.getPool appState) time waiRequest

      response <- either Error.errorResponseFor identity <$> eitherResponse

      -- Launch the connWorker when the connection is down.  The postgrest
      -- function can respond successfully (with a stale schema cache) before
      -- the connWorker is done.
      when (Wai.responseStatus response == HTTP.status503) connWorker

      waiRespond response

postgrestResponse
  :: AppConfig
  -> PgVersion
  -> Maybe DbStructure
  -> SQL.Pool
  -> UTCTime
  -> Wai.Request
  -> Handler IO Wai.Response
postgrestResponse conf pgVer maybeDbStructure pool time waiRequest = do
  -- Fail early if no DbStructure is loaded
  dbStructure <- maybe (throwError Error.ConnectionLostError) pure maybeDbStructure
  -- The JWT must be checked before touching the db
  jwtClaims <- Auth.jwtClaims conf waiRequest time
  waiBody <- lift $ Wai.strictRequestBody waiRequest
  request <- liftEither $ Request.parse conf pgVer dbStructure waiRequest waiBody
  runDbHandler pool (Query.txMode $ Request.apiReq request) jwtClaims .
    Middleware.optionalRollback conf (Request.apiReq request) $ do
      Middleware.setPgLocals conf jwtClaims (Request.apiReq request)
      handleRequest request

runDbHandler :: SQL.Pool -> SQL.Mode -> Auth.JWTClaims -> DbHandler a -> Handler IO a
runDbHandler pool mode jwtClaims handler = do
  dbResp <-
    lift . SQL.use pool . SQL.transaction SQL.ReadCommitted mode $ runExceptT handler
  resp <-
    liftEither . mapLeft Error.PgErr $
      mapLeft (Error.PgError $ Auth.containsRole jwtClaims) dbResp
  liftEither resp

handleRequest :: Request -> DbHandler Wai.Response
handleRequest req =
  case req of
    ReadRequest readRequestInfo ->
      Response.readResponse <$> Query.readQuery readRequestInfo
    CreateRequest mutateRequestInfo ->
      Response.createResponse <$> Query.createQuery mutateRequestInfo
    UpdateRequest mutateRequestInfo ->
      Response.updateResponse <$> Query.updateQuery mutateRequestInfo
    SingleUpsertRequest mutateRequestInfo ->
      Response.singleUpsertResponse <$> Query.singleUpsertQuery mutateRequestInfo
    DeleteRequest mutateRequestInfo ->
      Response.deleteResponse <$> Query.deleteQuery mutateRequestInfo
    InfoRequest dbStructure _ QualifiedIdentifier{..} ->
      case find tableMatches $ dbTables dbStructure of
        Just table -> return $ Response.infoResponse hasPK table
        Nothing    -> throwError Error.NotFound
      where
        tableMatches Table{..} = tableName == qiName && tableSchema == qiSchema
        hasPK = not $ null $ tablePKCols dbStructure qiSchema qiName
    InvokeRequest invokeRequestInfo  ->
      Response.invokeResponse <$> Query.invokeQuery invokeRequestInfo
    OpenApiRequest conf dbStructure apiRequest headersOnly tSchema ->
      Response.openApiResponse headersOnly conf dbStructure apiRequest
        <$> Query.openApiQuery tSchema conf
