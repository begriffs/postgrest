{-|
Module      : PostgREST.Middleware
Description : Sets CORS policy. Also the PostgreSQL GUCs, role, search_path and pre-request function.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.Middleware (runPgLocals, pgrstMiddleware) where

import qualified Hasql.Decoders                    as HD
import qualified Hasql.DynamicStatements.Statement as H
import           PostgREST.Private.Common

import qualified Data.Aeson                as JSON
import qualified Data.ByteString.Char8     as BS
import qualified Data.CaseInsensitive      as CI
import qualified Data.HashMap.Strict       as M
import           Data.List                 (lookup)
import           Data.Scientific           (FPFormat (..),
                                            formatScientific,
                                            isInteger)
import qualified Data.Text                 as T
import qualified Hasql.Transaction         as H
import           Network.HTTP.Types.Status (status400, status500)

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

    unquoted :: JSON.Value -> Text
    unquoted (JSON.String t) = t
    unquoted (JSON.Number n) =
      toS $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
    unquoted (JSON.Bool b) = show b
    unquoted v = toS $ JSON.encode v

pgrstMiddleware :: LogLevel -> Middleware
pgrstMiddleware logLevel =
    logger logLevel
  . gzip def
  . cors corsPolicy

logger :: LogLevel -> Middleware
logger logLevel app req sendResponse = app req $ \res ->
  let
    status = responseStatus res
    logRequest = logStdout app req sendResponse
  in
  case logLevel of
    LogInfo                        -> logRequest
    LogWarn  | status >= status400 -> logRequest
    LogError | status >= status500 -> logRequest
    _                              -> sendResponse res

-- | CORS policy to be used in by Wai Cors middleware
corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy req = case lookup "origin" headers of
  Just origin ->
    Just CorsResourcePolicy
    { corsOrigins = Just ([origin], True)
    , corsMethods = ["GET", "POST", "PATCH", "PUT", "DELETE", "OPTIONS"]
    , corsRequestHeaders = "Authorization":accHeaders
    , corsExposedHeaders = Just
      [ "Content-Encoding", "Content-Location", "Content-Range", "Content-Type"
      , "Date", "Location", "Server", "Transfer-Encoding", "Range-Unit"]
    , corsMaxAge = Just $ 60*60*24
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = True
    }
  Nothing -> Nothing
  where
    headers = requestHeaders req
    accHeaders = case lookup "access-control-request-headers" headers of
      Just hdrs -> map (CI.mk . toS . T.strip . toS) $ BS.split ',' hdrs
      Nothing -> []
