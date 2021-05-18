{-|
Module      : PostgREST.Middleware
Description : Sets CORS policy. Also the PostgreSQL GUCs, role, search_path and pre-request function.
-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.Middleware
  ( setPgLocals
  , pgrstFormat
  , pgrstMiddleware
  , optionalRollback
  ) where

import qualified Data.Aeson                           as JSON
import qualified Data.ByteString.Char8                as BS
import qualified Data.CaseInsensitive                 as CI
import qualified Data.HashMap.Strict                  as M
import qualified Data.Text                            as T
import qualified Hasql.Decoders                       as HD
import qualified Hasql.DynamicStatements.Snippet      as H hiding
                                                           (sql)
import qualified Hasql.DynamicStatements.Statement    as H
import qualified Hasql.Transaction                    as H
import qualified Network.HTTP.Types.Header            as HTTP
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Logger                   as Wai
import qualified Network.Wai.Middleware.Cors          as Wai
import qualified Network.Wai.Middleware.Gzip          as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Network.Wai.Middleware.Static        as Wai

import Data.Function             (id)
import Data.List                 (lookup)
import Data.Scientific           (FPFormat (..), formatScientific,
                                  isInteger)
import Network.HTTP.Types.Status (Status, status400, status500,
                                  statusCode)
import System.IO.Unsafe          (unsafePerformIO)
import System.Log.FastLogger     (toLogStr)

import PostgREST.Config             (AppConfig (..), LogLevel (..))
import PostgREST.Error              (Error, errorResponseFor)
import PostgREST.GucHeader          (addHeadersIfNotIncluded)
import PostgREST.Query.SqlFragment  (intercalateSnippet,
                                     unknownLiteral)
import PostgREST.Request.ApiRequest (ApiRequest (..))

import PostgREST.Request.Preferences

import Protolude      hiding (head, toS)
import Protolude.Conv (toS)

-- | Set local(transaction scoped) GUCs for every request, plus the pre-request function
setPgLocals :: AppConfig -> M.HashMap Text JSON.Value -> ApiRequest
  -> ExceptT Error H.Transaction ()
setPgLocals conf claims req = do
  lift $ H.statement mempty $ H.dynamicallyParameterized
    ("select " <> intercalateSnippet ", " (searchPathSql : roleSql ++ claimsSql ++ [methodSql, pathSql] ++ headersSql ++ cookiesSql ++ appSettingsSql))
    HD.noResult (configDbPreparedStatements conf)
  lift $ traverse_ H.sql preReqSql
  where
    methodSql = setConfigLocal mempty ("request.method", toS $ iMethod req)
    pathSql = setConfigLocal mempty ("request.path", toS $ iPath req)
    headersSql = setConfigLocal "request.header." <$> iHeaders req
    cookiesSql = setConfigLocal "request.cookie." <$> iCookies req
    claimsWithRole =
      let anon = JSON.String . toS $ configDbAnonRole conf in -- role claim defaults to anon if not specified in jwt
      M.union claims (M.singleton "role" anon)
    claimsSql = setConfigLocal "request.jwt.claim." <$> [(c,unquoted v) | (c,v) <- M.toList claimsWithRole]
    roleSql = maybeToList $ (\x -> setConfigLocal mempty ("role", unquoted x)) <$> M.lookup "role" claimsWithRole
    appSettingsSql = setConfigLocal mempty <$> configAppSettings conf
    searchPathSql =
      let schemas = T.intercalate ", " (iSchema req : configDbExtraSearchPath conf) in
      setConfigLocal mempty ("search_path", schemas)
    preReqSql = (\f -> "select " <> toS f <> "();") <$> configDbPreRequest conf

    -- | Do a pg set_config(setting, value, true) call. This is equivalent to a SET LOCAL.
    setConfigLocal :: Text -> (Text, Text) -> H.Snippet
    setConfigLocal prefix (k, v) =
      "set_config(" <> unknownLiteral (prefix <> k) <> ", " <> unknownLiteral v <> ", true)"

-- | Log in apache format. Only requests that have a status greater than minStatus are logged.
-- | There's no way to filter logs in the apache format on wai-extra: https://hackage.haskell.org/package/wai-extra-3.0.29.2/docs/Network-Wai-Middleware-RequestLogger.html#t:OutputFormat.
-- | So here we copy wai-logger apacheLogStr function: https://github.com/kazu-yamamoto/logger/blob/a4f51b909a099c51af7a3f75cf16e19a06f9e257/wai-logger/Network/Wai/Logger/Apache.hs#L45
-- | TODO: Add the ability to filter apache logs on wai-extra and remove this function.
pgrstFormat :: Status -> Wai.OutputFormatter
pgrstFormat minStatus date req status responseSize =
  if status < minStatus
    then mempty
  else toLogStr (getSourceFromSocket req)
    <> " - - ["
    <> toLogStr date
    <> "] \""
    <> toLogStr (Wai.requestMethod req)
    <> " "
    <> toLogStr (Wai.rawPathInfo req <> Wai.rawQueryString req)
    <> " "
    <> toLogStr (show (Wai.httpVersion req)::Text)
    <> "\" "
    <> toLogStr (show (statusCode status)::Text)
    <> " "
    <> toLogStr (maybe "-" show responseSize::Text)
    <> " \""
    <> toLogStr (fromMaybe mempty $ Wai.requestHeaderReferer req)
    <> "\" \""
    <> toLogStr (fromMaybe mempty $ Wai.requestHeaderUserAgent req)
    <> "\"\n"
  where
    getSourceFromSocket = BS.pack . Wai.showSockAddr . Wai.remoteHost

pgrstMiddleware :: LogLevel -> Wai.Application -> Wai.Application
pgrstMiddleware logLevel =
    logger
  . Wai.gzip Wai.def
  . Wai.cors corsPolicy
  . Wai.staticPolicy (Wai.only [("favicon.ico", "static/favicon.ico")])
  where
    logger = case logLevel of
      LogCrit  -> id
      LogError -> unsafePerformIO $ Wai.mkRequestLogger Wai.def { Wai.outputFormat = Wai.CustomOutputFormat $ pgrstFormat status500}
      LogWarn  -> unsafePerformIO $ Wai.mkRequestLogger Wai.def { Wai.outputFormat = Wai.CustomOutputFormat $ pgrstFormat status400}
      LogInfo  -> Wai.logStdout

defaultCorsPolicy :: Wai.CorsResourcePolicy
defaultCorsPolicy =  Wai.CorsResourcePolicy Nothing
  ["GET", "POST", "PATCH", "PUT", "DELETE", "OPTIONS"] ["Authorization"] Nothing
  (Just $ 60*60*24) False False True

-- | CORS policy to be used in by Wai Cors middleware
corsPolicy :: Wai.Request -> Maybe Wai.CorsResourcePolicy
corsPolicy req = case lookup "origin" headers of
  Just origin -> Just defaultCorsPolicy {
      Wai.corsOrigins = Just ([origin], True)
    , Wai.corsRequestHeaders = "Authentication" : accHeaders
    , Wai.corsExposedHeaders = Just [
        "Content-Encoding", "Content-Location", "Content-Range", "Content-Type"
      , "Date", "Location", "Server", "Transfer-Encoding", "Range-Unit"
      ]
    }
  Nothing -> Nothing
  where
    headers = Wai.requestHeaders req
    accHeaders = case lookup "access-control-request-headers" headers of
      Just hdrs -> map (CI.mk . toS . T.strip . toS) $ BS.split ',' hdrs
      Nothing -> []

unquoted :: JSON.Value -> Text
unquoted (JSON.String t) = t
unquoted (JSON.Number n) =
  toS $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
unquoted (JSON.Bool b) = show b
unquoted v = toS $ JSON.encode v

-- | Set a transaction to eventually roll back if requested and set respective
-- headers on the response.
optionalRollback
  :: AppConfig
  -> ApiRequest
  -> ExceptT Error H.Transaction Wai.Response
  -> ExceptT Error H.Transaction Wai.Response
optionalRollback AppConfig{..} ApiRequest{..} transaction = do
  resp <- catchError transaction $ return . errorResponseFor
  when (shouldRollback || (configDbTxRollbackAll && not shouldCommit))
    (lift H.condemn)
  return $ Wai.mapResponseHeaders preferenceApplied resp
  where
    shouldCommit =
      configDbTxAllowOverride && iPreferTransaction == Just Commit
    shouldRollback =
      configDbTxAllowOverride && iPreferTransaction == Just Rollback
    preferenceApplied
      | shouldCommit =
          addHeadersIfNotIncluded
            [(HTTP.hPreferenceApplied, BS.pack (show Commit))]
      | shouldRollback =
          addHeadersIfNotIncluded
            [(HTTP.hPreferenceApplied, BS.pack (show Rollback))]
      | otherwise =
          identity
