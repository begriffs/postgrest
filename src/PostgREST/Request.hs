{-# LANGUAGE RecordWildCards #-}
module PostgREST.Request
  ( Request(..)
  , ReadRequestInfo(..)
  , MutateRequestInfo(..)
  , InvokeRequestInfo(..)
  , apiReq
  , parse
  ) where

import Data.Either.Combinators (mapLeft)
import Data.List               (union)

import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai          as Wai

import qualified PostgREST.ContentType              as ContentType
import qualified PostgREST.DbStructure.Proc         as Proc
import qualified PostgREST.Error                    as Error
import qualified PostgREST.Request.ApiRequest       as ApiRequest
import qualified PostgREST.Request.DbRequestBuilder as ReqBuilder
import qualified PostgREST.Request.Types            as Types

import PostgREST.Config                  (AppConfig (..))
import PostgREST.ContentType             (ContentType (..))
import PostgREST.DbStructure             (DbStructure (..),
                                          tablePKCols)
import PostgREST.DbStructure.Identifiers (FieldName,
                                          QualifiedIdentifier (..),
                                          Schema)
import PostgREST.DbStructure.PgVersion   (PgVersion)
import PostgREST.DbStructure.Proc        (ProcDescription (..))
import PostgREST.Error                   (Error)
import PostgREST.Request.ApiRequest      (Action (..),
                                          ApiRequest (..),
                                          InvokeMethod (..),
                                          Target (..))

import Protolude


data Request
  = ReadRequest ReadRequestInfo
  | CreateRequest MutateRequestInfo
  | UpdateRequest MutateRequestInfo
  | SingleUpsertRequest MutateRequestInfo
  | DeleteRequest MutateRequestInfo
  | InfoRequest DbStructure ApiRequest QualifiedIdentifier
  | InvokeRequest InvokeRequestInfo
  | OpenApiRequest AppConfig DbStructure ApiRequest Bool Schema

data ReadRequestInfo = ReadRequestInfo
  { rrConfig      :: AppConfig
  , rrPgVersion   :: PgVersion
  , rrDbStructure :: DbStructure
  , rrApiRequest  :: ApiRequest
  , rrHeadersOnly :: Bool
  , rrIdentifier  :: QualifiedIdentifier
  , rrReadRequest :: Types.ReadRequest
  , rrBinaryField :: BinaryField
  }

data MutateRequestInfo = MutateRequestInfo
  { mrConfig        :: AppConfig
  , mrPgVersion     :: PgVersion
  , mrDbStructure   :: DbStructure
  , mrApiRequest    :: ApiRequest
  , mrIdentifier    :: QualifiedIdentifier
  , mrReadRequest   :: Types.ReadRequest
  , mrMutateRequest :: Types.MutateRequest
  }

data InvokeRequestInfo = InvokeRequestInfo
  { irConfig       :: AppConfig
  , irPgVersion    :: PgVersion
  , irDbStructure  :: DbStructure
  , irApiRequest   :: ApiRequest
  , irInvokeMethod :: InvokeMethod
  , irProc         :: ProcDescription
  , irReadRequest  :: Types.ReadRequest
  , irBinaryField  :: BinaryField
  }

type BinaryField = Maybe FieldName

parse :: AppConfig -> PgVersion -> DbStructure -> Wai.Request -> LBS.ByteString -> Either Error Request
parse conf pgVer dbStructure waiRequest waiBody = do
  apiRequest@ApiRequest{..} <-
    mapLeft Error.ApiRequestError $ ApiRequest.userApiRequest conf dbStructure waiRequest waiBody

  case (iAction, iTarget) of
    (ActionRead headersOnly, TargetIdent identifier) -> do
      readReq <- readRequest identifier conf dbStructure apiRequest
      bField <- binaryField conf iTarget iAcceptContentType readReq
      return . ReadRequest $ ReadRequestInfo conf pgVer dbStructure apiRequest headersOnly identifier readReq bField
    (ActionCreate, TargetIdent identifier) ->
      CreateRequest <$> mutateRequest conf pgVer dbStructure apiRequest identifier
    (ActionUpdate, TargetIdent identifier) ->
      UpdateRequest <$> mutateRequest conf pgVer dbStructure apiRequest identifier
    (ActionSingleUpsert, TargetIdent identifier) ->
      SingleUpsertRequest <$> mutateRequest conf pgVer dbStructure apiRequest identifier
    (ActionDelete, TargetIdent identifier) ->
      DeleteRequest <$> mutateRequest conf pgVer dbStructure apiRequest identifier
    (ActionInfo, TargetIdent identifier) ->
      return $ InfoRequest dbStructure apiRequest identifier
    (ActionInvoke invMethod, TargetProc proc _) -> do
      readReq <- readRequest identifier conf dbStructure apiRequest
      bField <- binaryField conf iTarget iAcceptContentType readReq
      return . InvokeRequest $
        InvokeRequestInfo conf pgVer dbStructure apiRequest invMethod proc readReq bField
      where
        identifier =
          QualifiedIdentifier (pdSchema proc)
            (fromMaybe (pdName proc) $ Proc.procTableName proc)
    (ActionInspect headersOnly, TargetDefaultSpec tSchema) -> do
      Right $ OpenApiRequest conf dbStructure apiRequest headersOnly tSchema
    _ ->
      Left Error.NotFound

-- | Get the raw ApiRequest from a request. This should be obsoloted by further
-- refactoring of this module.
apiReq :: Request -> ApiRequest
apiReq (ReadRequest ReadRequestInfo{..})           = rrApiRequest
apiReq (CreateRequest MutateRequestInfo{..})       = mrApiRequest
apiReq (UpdateRequest MutateRequestInfo{..})       = mrApiRequest
apiReq (SingleUpsertRequest MutateRequestInfo{..}) = mrApiRequest
apiReq (DeleteRequest MutateRequestInfo{..})       = mrApiRequest
apiReq (InfoRequest _ a _)                         = a
apiReq (InvokeRequest InvokeRequestInfo{..})       = irApiRequest
apiReq (OpenApiRequest _ _ a _ _)                  = a

mutateRequest :: AppConfig -> PgVersion -> DbStructure -> ApiRequest -> QualifiedIdentifier -> Either Error MutateRequestInfo
mutateRequest conf pgVer dbStructure apiRequest identifier@QualifiedIdentifier{..} = do
  readReq <- readRequest identifier conf dbStructure apiRequest
  mutReq <-
    ReqBuilder.mutateRequest qiSchema qiName apiRequest
      (tablePKCols dbStructure qiSchema qiName)
      readReq
  return $ MutateRequestInfo conf pgVer dbStructure apiRequest identifier readReq mutReq

readRequest :: QualifiedIdentifier -> AppConfig -> DbStructure -> ApiRequest -> Either Error Types.ReadRequest
readRequest QualifiedIdentifier{..} AppConfig{..} dbStructure =
  ReqBuilder.readRequest qiSchema qiName configDbMaxRows (dbRelationships dbStructure)

-- | If raw(binary) output is requested, check that ContentType is one of the
-- admitted rawContentTypes and that`?select=...` contains only one field other
-- than `*`
binaryField :: AppConfig -> ApiRequest.Target -> ContentType -> Types.ReadRequest -> Either Error (Maybe FieldName)
binaryField conf iTarget iAcceptContentType readReq
  | returnsScalar iTarget && iAcceptContentType `elem` rawContentTypes conf =
      return $ Just "pgrst_scalar"
  | iAcceptContentType `elem` rawContentTypes conf =
      let
        fldNames = Types.fstFieldNames readReq
        fieldName = headMay fldNames
      in
      if length fldNames == 1 && fieldName /= Just "*" then
        return fieldName
      else
        Left $ Error.BinaryFieldError iAcceptContentType
  | otherwise =
      return Nothing

rawContentTypes :: AppConfig -> [ContentType]
rawContentTypes AppConfig{..} =
  (ContentType.decodeContentType <$> configRawMediaTypes) `union` [CTOctetStream, CTTextPlain]

returnsScalar :: ApiRequest.Target -> Bool
returnsScalar (TargetProc proc _) = Proc.procReturnsScalar proc
returnsScalar _                   = False
