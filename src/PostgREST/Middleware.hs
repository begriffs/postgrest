{-|
Module      : PostgREST.Middleware
Description : Sets CORS policy. Also the PostgreSQL GUCs, role, search_path and pre-request function.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.Middleware where

import qualified Hasql.Decoders                    as HD
import qualified Hasql.DynamicStatements.Statement as H
import           PostgREST.Private.Common

import qualified Data.Aeson                as JSON
import qualified Data.ByteString.Char8     as BS
import qualified Data.CaseInsensitive      as CI
import           Data.Function             (id)
import qualified Data.HashMap.Strict       as M
import           Data.List                 (lookup)
import           Data.Scientific           (FPFormat (..),
                                            formatScientific,
                                            isInteger)
import qualified Data.Text                 as T
import qualified Hasql.Transaction         as H
import           Network.HTTP.Types.Status (Status, status400,
                                            status500, statusCode)
import           Network.Wai.Logger        (showSockAddr)
import           System.Log.FastLogger     (toLogStr)

import Network.Wai
import Network.Wai.Middleware.Cors          (CorsResourcePolicy (..),
                                             cors)
import Network.Wai.Middleware.Gzip          (def, gzip)
import Network.Wai.Middleware.RequestLogger

import PostgREST.ApiRequest   (ApiRequest (..))
import PostgREST.Config       (AppConfig (..))
import PostgREST.QueryBuilder (setConfigLocal)
import PostgREST.Types        (LogLevel (..))
import Protolude              hiding (head, toS)
import Protolude.Conv         (toS)
import System.IO.Unsafe       (unsafePerformIO)

-- | Runs local(transaction scoped) GUCs for every request, plus the pre-request function
runPgLocals :: AppConfig   -> M.HashMap Text JSON.Value ->
               (ApiRequest -> H.Transaction Response) ->
               ApiRequest  -> H.Transaction Response
runPgLocals conf claims app req = do
  H.statement mempty $ H.dynamicallyParameterized
    ("select " <> intercalateSnippet ", " (searchPathSql : roleSql ++ claimsSql ++ [methodSql, pathSql] ++ headersSql ++ cookiesSql ++ appSettingsSql))
    HD.noResult (configDbPreparedStatements conf)
  traverse_ H.sql preReqSql
  app req
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

-- | Log in apache format. Only requests that have a status greater than minStatus are logged.
-- | There's no way to filter logs in the apache format on wai-extra: https://hackage.haskell.org/package/wai-extra-3.0.29.2/docs/Network-Wai-Middleware-RequestLogger.html#t:OutputFormat.
-- | So here we copy wai-logger apacheLogStr function: https://github.com/kazu-yamamoto/logger/blob/a4f51b909a099c51af7a3f75cf16e19a06f9e257/wai-logger/Network/Wai/Logger/Apache.hs#L45
-- | TODO: Add the ability to filter apache logs on wai-extra and remove this function.
pgrstFormat :: Status -> OutputFormatter
pgrstFormat minStatus date req status responseSize =
  if status < minStatus
    then mempty
  else toLogStr (getSourceFromSocket req)
    <> " - - ["
    <> toLogStr date
    <> "] \""
    <> toLogStr (requestMethod req)
    <> " "
    <> toLogStr (rawPathInfo req <> rawQueryString req)
    <> " "
    <> toLogStr (show (httpVersion req)::Text)
    <> "\" "
    <> toLogStr (show (statusCode status)::Text)
    <> " "
    <> toLogStr (maybe "-" show responseSize::Text)
    <> " \""
    <> toLogStr (fromMaybe mempty $ requestHeaderReferer req)
    <> "\" \""
    <> toLogStr (fromMaybe mempty $ requestHeaderUserAgent req)
    <> "\"\n"
  where
    getSourceFromSocket = BS.pack . showSockAddr . remoteHost

pgrstMiddleware :: LogLevel -> Application -> Application
pgrstMiddleware logLevel =
    logger
  . gzip def
  . cors corsPolicy
  where
    logger = case logLevel of
      LogCrit  -> id
      LogError -> unsafePerformIO $ mkRequestLogger def { outputFormat = CustomOutputFormat $ pgrstFormat status500}
      LogWarn  -> unsafePerformIO $ mkRequestLogger def { outputFormat = CustomOutputFormat $ pgrstFormat status400}
      LogInfo  -> logStdout

defaultCorsPolicy :: CorsResourcePolicy
defaultCorsPolicy =  CorsResourcePolicy Nothing
  ["GET", "POST", "PATCH", "PUT", "DELETE", "OPTIONS"] ["Authorization"] Nothing
  (Just $ 60*60*24) False False True

-- | CORS policy to be used in by Wai Cors middleware
corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy req = case lookup "origin" headers of
  Just origin -> Just defaultCorsPolicy {
      corsOrigins = Just ([origin], True)
    , corsRequestHeaders = "Authentication":accHeaders
    , corsExposedHeaders = Just [
        "Content-Encoding", "Content-Location", "Content-Range", "Content-Type"
      , "Date", "Location", "Server", "Transfer-Encoding", "Range-Unit"
      ]
    }
  Nothing -> Nothing
  where
    headers = requestHeaders req
    accHeaders = case lookup "access-control-request-headers" headers of
      Just hdrs -> map (CI.mk . toS . T.strip . toS) $ BS.split ',' hdrs
      Nothing -> []

unquoted :: JSON.Value -> Text
unquoted (JSON.String t) = t
unquoted (JSON.Number n) =
  toS $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
unquoted (JSON.Bool b) = show b
unquoted v = toS $ JSON.encode v
