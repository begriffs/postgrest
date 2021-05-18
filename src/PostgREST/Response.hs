{-# LANGUAGE RecordWildCards #-}
module PostgREST.Response
  ( createResponse
  , deleteResponse
  , infoResponse
  , invokeResponse
  , openApiResponse
  , readResponse
  , singleUpsertResponse
  , updateResponse
  ) where

import qualified Data.ByteString.Char8     as BS8
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Set                  as Set
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.URI    as HTTP
import qualified Network.Wai               as Wai

import qualified PostgREST.ContentType as ContentType
import qualified PostgREST.OpenAPI     as OpenAPI
import qualified PostgREST.RangeQuery  as RangeQuery

import PostgREST.Config                  (AppConfig (..))
import PostgREST.ContentType             (ContentType (..))
import PostgREST.DbStructure             (DbStructure (..),
                                          tablePKCols)
import PostgREST.DbStructure.Identifiers (QualifiedIdentifier (..),
                                          Schema)
import PostgREST.DbStructure.Table       (Table (..))
import PostgREST.GucHeader               (GucHeader,
                                          addHeadersIfNotIncluded,
                                          unwrapGucHeader)
import PostgREST.Query                   (InvokeQueryResult (..),
                                          MutateQueryResult (..),
                                          OpenApiQueryResult,
                                          ReadQueryResult (..))
import PostgREST.Request                 (InvokeRequestInfo (..),
                                          MutateRequestInfo (..),
                                          ReadRequestInfo (..))
import PostgREST.Request.ApiRequest      (ApiRequest (..),
                                          InvokeMethod (..))
import PostgREST.Request.Preferences     (PreferCount (..),
                                          PreferRepresentation (..))

import Protolude      hiding (Handler, toS)
import Protolude.Conv (toS)


readResponse :: ReadQueryResult -> Wai.Response
readResponse ReadQueryResult{..} =
  gucResponse rqGucStatus rqGucHeaders status headers $
    if rrHeadersOnly then mempty else toS rqBody
  where
    ReadRequestInfo{..} = rqRequest
    ApiRequest{..} = rrApiRequest
    (status, contentRange) =
      RangeQuery.rangeStatusHeader iTopLevelRange rqQueryTotal rqTableTotal
    headers =
      [ contentRange
      , ( "Content-Location"
        , "/"
            <> toS (qiName rrIdentifier)
            <> if BS8.null iCanonicalQS then mempty else "?" <> toS iCanonicalQS
        )
      ]
      ++ contentTypeHeaders rrApiRequest

createResponse :: MutateQueryResult -> Wai.Response
createResponse MutateQueryResult{..} =
  if iPreferRepresentation == Full then
    response (headers ++ contentTypeHeaders mrApiRequest) (toS resBody)
  else
    response headers mempty
  where
    MutateRequestInfo{..} = resRequest
    ApiRequest{..} = mrApiRequest
    QualifiedIdentifier{..} = mrIdentifier
    pkCols = tablePKCols mrDbStructure qiSchema qiName
    response = gucResponse resGucStatus resGucHeaders HTTP.status201
    headers =
      catMaybes
        [ if null resFields then
            Nothing
          else
            Just
              ( HTTP.hLocation
              , "/"
                  <> toS qiName
                  <> HTTP.renderSimpleQuery True (splitKeyValue <$> resFields)
              )
        , Just . RangeQuery.contentRangeH 1 0 $
            if shouldCount iPreferCount then Just resQueryTotal else Nothing
        , if null pkCols && isNothing iOnConflict then
            Nothing
          else
            (\x -> ("Preference-Applied", BS8.pack $ show x)) <$> iPreferResolution
        ]

updateResponse :: MutateQueryResult -> Wai.Response
updateResponse MutateQueryResult{..} =
  if fullRepr then
    response (contentTypeHeaders mrApiRequest ++ [contentRangeHeader]) (toS resBody)
  else
    response [contentRangeHeader] mempty
  where
    MutateRequestInfo{..} = resRequest
    response = gucResponse resGucStatus resGucHeaders status
    fullRepr = iPreferRepresentation mrApiRequest == Full
    updateIsNoOp = Set.null $ iColumns mrApiRequest
    status
      | resQueryTotal == 0 && not updateIsNoOp = HTTP.status404
      | fullRepr = HTTP.status200
      | otherwise = HTTP.status204
    contentRangeHeader =
      RangeQuery.contentRangeH 0 (resQueryTotal - 1) $
        if shouldCount (iPreferCount mrApiRequest) then Just resQueryTotal else Nothing

singleUpsertResponse :: MutateQueryResult -> Wai.Response
singleUpsertResponse MutateQueryResult{..} =
  if iPreferRepresentation mrApiRequest == Full then
    response HTTP.status200 (contentTypeHeaders mrApiRequest) (toS resBody)
  else
    response HTTP.status204 (contentTypeHeaders mrApiRequest) mempty
  where
    MutateRequestInfo{..} = resRequest
    response = gucResponse resGucStatus resGucHeaders

deleteResponse :: MutateQueryResult -> Wai.Response
deleteResponse MutateQueryResult{..} =
  if iPreferRepresentation mrApiRequest == Full then
    response HTTP.status200
      (contentTypeHeaders mrApiRequest ++ [contentRangeHeader])
      (toS resBody)
  else
    response HTTP.status204 [contentRangeHeader] mempty
  where
    MutateRequestInfo{..} = resRequest
    response = gucResponse resGucStatus resGucHeaders
    contentRangeHeader =
      RangeQuery.contentRangeH 1 0 $
        if shouldCount (iPreferCount mrApiRequest) then Just resQueryTotal else Nothing

infoResponse :: Bool -> Table -> Wai.Response
infoResponse hasPrimaryKey table =
  Wai.responseLBS HTTP.status200 [allOrigins, allowH] mempty
  where
    allOrigins = ("Access-Control-Allow-Origin", "*")
    allowH =
      ( HTTP.hAllow
      , BS8.intercalate "," $
          ["OPTIONS,GET,HEAD"]
          ++ ["POST" | tableInsertable table]
          ++ ["PUT" | tableInsertable table && tableUpdatable table && hasPrimaryKey]
          ++ ["PATCH" | tableUpdatable table]
          ++ ["DELETE" | tableDeletable table]
      )

invokeResponse :: InvokeQueryResult -> Wai.Response
invokeResponse InvokeQueryResult{..} =
  gucResponse iqGucStatus iqGucHeaders status headers $
    if irInvokeMethod == InvHead then mempty else toS iqBody
  where
    InvokeRequestInfo{..} = iqRequest
    ApiRequest{..} = irApiRequest
    (status, contentRange) =
      RangeQuery.rangeStatusHeader iTopLevelRange iqQueryTotal iqTableTotal
    headers = contentTypeHeaders irApiRequest ++ [contentRange]

openApiResponse :: Bool -> AppConfig -> DbStructure -> ApiRequest -> OpenApiQueryResult -> Wai.Response
openApiResponse headersOnly conf dbStructure apiRequest result =
  Wai.responseLBS HTTP.status200
    (ContentType.toHeader CTOpenAPI : maybeToList (profileHeader $ iProfile apiRequest))
    (if headersOnly then mempty else toS body)
  where
    (accessibleTables, schemaDescription, accessibleProcs) = result
    body =
      OpenAPI.encode conf dbStructure accessibleTables schemaDescription accessibleProcs

-- | Response with headers and status overridden from GUCs.
gucResponse
  :: Maybe HTTP.Status
  -> [GucHeader]
  -> HTTP.Status
  -> [HTTP.Header]
  -> LBS.ByteString
  -> Wai.Response
gucResponse gucStatus gucHeaders status headers =
  Wai.responseLBS (fromMaybe status gucStatus) $
    addHeadersIfNotIncluded headers (map unwrapGucHeader gucHeaders)

shouldCount :: Maybe PreferCount -> Bool
shouldCount preferCount =
  preferCount == Just ExactCount || preferCount == Just EstimatedCount

contentTypeHeaders :: ApiRequest -> [HTTP.Header]
contentTypeHeaders ApiRequest{..} =
  ContentType.toHeader iAcceptContentType : maybeToList (profileHeader iProfile)

profileHeader :: Maybe Schema -> Maybe HTTP.Header
profileHeader iProfile =
  (,) "Content-Profile" <$> (toS <$> iProfile)

splitKeyValue :: ByteString -> (ByteString, ByteString)
splitKeyValue kv =
  (k, BS8.tail v)
  where
    (k, v) = BS8.break (== '=') kv
