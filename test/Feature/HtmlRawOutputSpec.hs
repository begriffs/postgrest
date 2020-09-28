module Feature.HtmlRawOutputSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec         hiding (pendingWith)
import Test.Hspec.Wai
import Text.Heredoc

import Protolude  hiding (get)
import SpecHelper (acceptHdrs)

spec :: SpecWith ((), Application)
spec = describe "When raw-media-types is set to \"text/html\"" $
  it "can get raw output with Accept: text/html" $
    request methodGet "/rpc/welcome.html" (acceptHdrs "text/html") ""
      `shouldRespondWith`
      [str|
          |<html>
          |  <head>
          |    <title>PostgREST</title>
          |  </head>
          |  <body>
          |    <h1>Welcome to PostgREST</h1>
          |  </body>
          |</html>
          |]
      { matchStatus = 200
      , matchHeaders = ["Content-Type" <:> "text/html"]
      }
