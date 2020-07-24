module Feature.PostGISSpec where

import Network.Wai      (Application)
import Network.Wai.Test (SResponse (simpleHeaders))

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "PostGIS features" $
  context "GeoJSON output" $ do
    it "works for a table that has a geometry column" $
      request methodGet "/shops"
        [("Accept", "application/geo+json")] "" `shouldRespondWith`
        [json| {
          "type" : "Featurecollection",
          "features" : [
            {"type": "Feature", "geometry": {"type":"Point","coordinates":[-71.10044,42.373695]}, "properties": {"id": 1, "address": "1369 Cambridge St"}}
          , {"type": "Feature", "geometry": {"type":"Point","coordinates":[-71.10543,42.366432]}, "properties": {"id": 2, "address": "757 Massachusetts Ave"}}
          , {"type": "Feature", "geometry": {"type":"Point","coordinates":[-71.081924,42.36437]}, "properties": {"id": 3, "address": "605 W Kendall St"}}
          ]} |]
        { matchHeaders = ["Content-Type" <:> "application/geo+json; charset=utf-8"] }

    it "fails for a table that doesn't have a geometry column" $
      request methodGet "/projects"
        [("Accept", "application/geo+json")] "" `shouldRespondWith`
        [json| {"hint":null,"details":null,"code":"22023","message":"geometry column is missing"} |]
        { matchStatus  = 400
        , matchHeaders = [matchContentTypeJson]
        }

    it "gives an empty features array on no rows" $
      request methodGet "/shops?id=gt.3"
        [("Accept", "application/geo+json")] "" `shouldRespondWith`
        [json| {
          "type" : "Featurecollection",
          "features" : []} |]
        { matchHeaders = ["Content-Type" <:> "application/geo+json; charset=utf-8"] }

    it "must include the geometry column when using ?select" $
      request methodGet "/shops?select=id,shop_geom&id=eq.1"
        [("Accept", "application/geo+json")] "" `shouldRespondWith`
        [json| {
          "type" : "Featurecollection",
          "features" : [
            {"type": "Feature", "geometry": {"type":"Point","coordinates":[-71.10044,42.373695]}, "properties": {"id": 1}}
          ] }|]
        { matchHeaders = ["Content-Type" <:> "application/geo+json; charset=utf-8"] }

    it "works with resource embedding" $
      request methodGet "/shops?select=*,shop_beacons(*)&id=eq.1"
        [("Accept", "application/geo+json")] "" `shouldRespondWith`
        [json|{
          "type" : "Featurecollection",
          "features" : [
            {"type": "Feature",
             "geometry": {"type":"Point","coordinates":[-71.10044,42.373695]},
             "properties": {
              "id": 1, "address": "1369 Cambridge St",
              "shop_beacons": [
                {"id":1,"name":"Beacon-1","coords":{"type":"Point","coordinates":[-71.10044,42.373695]},"shop_id":1},
                {"id":2,"name":"Beacon-2","coords":{"type":"Point","coordinates":[-71.10044,42.373695]},"shop_id":1}
              ]
              }
            }
          ]
        }|]
        { matchHeaders = ["Content-Type" <:> "application/geo+json; charset=utf-8"] }

    it "works with RPC" $
      request methodGet "/rpc/get_shop?id=1"
        [("Accept", "application/geo+json")] "" `shouldRespondWith`
        [json|{
          "type" : "Featurecollection",
          "features" : [
            {"type": "Feature", "geometry": {"type":"Point","coordinates":[-71.10044,42.373695]}, "properties": {"id": 1, "address": "1369 Cambridge St"} }
          ]
        }|]
        { matchHeaders = ["Content-Type" <:> "application/geo+json; charset=utf-8"] }

    it "works with Prefer: return=representation after POST" $
      request methodPost "/shops"
        [("Accept", "application/geo+json"), ("Prefer", "return=representation")] [json|
          {"id": 4, "address": "1354 Massachusetts Ave", "shop_geom": "SRID=4326;POINT(-71.11834 42.373238)"}
        |] `shouldRespondWith`
        [json|{
          "type": "Featurecollection",
          "features": [
            {
              "type": "Feature",
              "geometry": { "coordinates": [ -71.11834, 42.373238 ], "type": "Point" },
              "properties": { "address": "1354 Massachusetts Ave", "id": 4 }
            }
          ]
        }|]
        { matchStatus  = 201
        , matchHeaders = ["Content-Type" <:> "application/geo+json; charset=utf-8"]
        }
