module Feature.RpcPreRequestGucsSpec where

import qualified Data.ByteString.Lazy as BL (empty)

import Network.Wai      (Application)
import Network.Wai.Test (SResponse (simpleBody, simpleHeaders, simpleStatus))

import Network.HTTP.Types
import Test.Hspec          hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Text.Heredoc

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "GUC headers on all methods via pre-request" $ do
    it "succeeds setting the headers on POST" $
      request methodPost "/items" [] [json|[{"id": 11111}]|]
        `shouldRespondWith` ""
        { matchStatus = 201
        , matchHeaders = ["X-Custom-Header" <:> "mykey=myval"]
        }

    it "succeeds setting the headers on GET and HEAD" $ do
      request methodGet "/items?id=eq.11111" [("User-Agent", "MSIE 6.0")] mempty
        `shouldRespondWith` [json|[{"id": 11111}]|]
        {matchHeaders = [
            matchContentTypeJson,
            "Cache-Control" <:> "no-cache, no-store, must-revalidate"]}

      request methodHead "/items?id=eq.11111" [("User-Agent", "MSIE 7.0")] mempty
        `shouldRespondWith` ""
        {matchHeaders = ["Cache-Control" <:> "no-cache, no-store, must-revalidate"]}

      request methodHead "/projects" [("Accept", "text/csv")] mempty
        `shouldRespondWith` ""
        {matchHeaders = ["Content-Disposition" <:> "attachment; filename=projects.csv"]}

    it "succeeds setting the headers on PATCH" $
      request methodPatch "/items?id=eq.11111" [] [json|[{"id": 11111}]|]
        `shouldRespondWith` ""
        { matchStatus = 204
        , matchHeaders = ["X-Custom-Header" <:> "mykey=myval"]
        }

    it "succeeds setting the headers on PUT" $
      request methodPut "/items?id=eq.11111" [] [json|[{"id": 11111}]|]
        `shouldRespondWith` ""
        { matchStatus = 204
        , matchHeaders = ["X-Custom-Header" <:> "mykey=myval"]
        }

    it "succeeds setting the headers on DELETE" $
      request methodDelete "/items?id=eq.11111" [] mempty
        `shouldRespondWith` ""
        { matchStatus = 204
        , matchHeaders = ["X-Custom-Header" <:> "mykey=myval"]
        }
    it "can override the Content-Type header" $ do
      request methodHead "/clients?id=eq.1" [] mempty
        `shouldRespondWith` ""
        { matchStatus = 200
        , matchHeaders = ["Content-Type" <:> "application/custom+json"]
        }
      request methodHead "/rpc/getallprojects" [] mempty
        `shouldRespondWith` ""
        { matchStatus = 200
        , matchHeaders = ["Content-Type" <:> "application/custom+json"]
        }
