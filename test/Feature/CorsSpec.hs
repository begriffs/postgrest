module Feature.CorsSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai

import Protolude

spec :: SpecWith ((), Application)
spec =
  describe "CORS" $ do
    it "replies naively and permissively to preflight request" $
      request methodOptions "/"
          [ ("Accept", "*/*")
          , ("Origin", "http://example.com")
          , ("Access-Control-Request-Method", "POST")
          , ("Access-Control-Request-Headers", "Foo,Bar") ]
          ""
        `shouldRespondWith`
          ""
          { matchHeaders = [ "Access-Control-Allow-Origin" <:> "http://example.com"
                           , "Access-Control-Allow-Credentials" <:> "true"
                           , "Access-Control-Allow-Methods" <:> "GET, POST, PATCH, PUT, DELETE, OPTIONS, HEAD"
                           , "Access-Control-Allow-Headers" <:> "Authorization, Foo, Bar, Accept, Accept-Language, Content-Language"
                           , "Access-Control-Max-Age" <:> "86400" ]
          }

    it "exposes necesssary response headers to regular request" $
      request methodGet "/items"
          [("Origin", "http://example.com")]
          ""
        `shouldRespondWith`
          ResponseMatcher
          { matchStatus = 200
          , matchBody = MatchBody (\_ _ -> Nothing) -- match any body
          , matchHeaders = [ "Access-Control-Expose-Headers" <:>
                             "Content-Encoding, Content-Location, Content-Range, Content-Type, \
                             \Date, Location, Server, Transfer-Encoding, Range-Unit"]
          }

    it "allows INFO body through even with CORS request headers present to postflight request" $
      request methodOptions "/items"
          [ ("Host", "localhost:3000")
          , ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:32.0) Gecko/20100101 Firefox/32.0")
          , ("Origin", "http://localhost:8000")
          , ("Accept", "text/csv, */*; q=0.01")
          , ("Accept-Language", "en-US,en;q=0.5")
          , ("Accept-Encoding", "gzip, deflate")
          , ("Referer", "http://localhost:8000/")
          , ("Connection", "keep-alive") ]
          ""
        `shouldRespondWith`
          ""
          { matchHeaders = [ "Access-Control-Allow-Origin" <:> "*" ] }
