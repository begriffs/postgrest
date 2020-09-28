module Feature.JsonOperatorSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import PostgREST.Types (PgVersion, pgVersion112, pgVersion121,
                        pgVersion95)
import Protolude       hiding (get)
import SpecHelper

spec :: PgVersion -> SpecWith ((), Application)
spec actualPgVersion = describe "json and jsonb operators" $ do
  context "Shaping response with select parameter" $ do
    it "obtains a json subfield one level with casting" $
      get "/complex_items?id=eq.1&select=settings->>foo::json" `shouldRespondWith`
        [json| [{"foo":{"int":1,"bar":"baz"}}] |] -- the value of foo here is of type "text"
        { matchHeaders = [matchContentTypeJson] }

    it "renames json subfield one level with casting" $
      get "/complex_items?id=eq.1&select=myFoo:settings->>foo::json" `shouldRespondWith`
        [json| [{"myFoo":{"int":1,"bar":"baz"}}] |] -- the value of foo here is of type "text"
        { matchHeaders = [matchContentTypeJson] }

    it "fails on bad casting (data of the wrong format)" $
      get "/complex_items?select=settings->foo->>bar::integer"
        `shouldRespondWith` (
        if actualPgVersion >= pgVersion121 then
        [json| {"hint":null,"details":null,"code":"22P02","message":"invalid input syntax for type integer: \"baz\""} |]
        else
        [json| {"hint":null,"details":null,"code":"22P02","message":"invalid input syntax for integer: \"baz\""} |]
                            )
        { matchStatus  = 400 , matchHeaders = [] }

    it "obtains a json subfield two levels (string)" $
      get "/complex_items?id=eq.1&select=settings->foo->>bar" `shouldRespondWith`
        [json| [{"bar":"baz"}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "renames json subfield two levels (string)" $
      get "/complex_items?id=eq.1&select=myBar:settings->foo->>bar" `shouldRespondWith`
        [json| [{"myBar":"baz"}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "obtains a json subfield two levels with casting (int)" $
      get "/complex_items?id=eq.1&select=settings->foo->>int::integer" `shouldRespondWith`
        [json| [{"int":1}] |] -- the value in the db is an int, but here we expect a string for now
        { matchHeaders = [matchContentTypeJson] }

    it "renames json subfield two levels with casting (int)" $
      get "/complex_items?id=eq.1&select=myInt:settings->foo->>int::integer" `shouldRespondWith`
        [json| [{"myInt":1}] |] -- the value in the db is an int, but here we expect a string for now
        { matchHeaders = [matchContentTypeJson] }

    -- TODO the status code for the error is 404, this is because 42883 represents undefined function
    -- this works fine for /rpc/unexistent requests, but for this case a 500 seems more appropriate
    it "fails when a double arrow ->> is followed with a single arrow ->" $ do
      get "/json_arr?select=data->>c->1"
        `shouldRespondWith` (
        if actualPgVersion >= pgVersion112 then
        [json|
          {"hint":"No operator matches the given name and argument types. You might need to add explicit type casts.",
           "details":null,"code":"42883","message":"operator does not exist: text -> integer"} |]
           else
        [json|
          {"hint":"No operator matches the given name and argument type(s). You might need to add explicit type casts.",
           "details":null,"code":"42883","message":"operator does not exist: text -> integer"} |]
                            )
        { matchStatus  = 404 , matchHeaders = [] }
      get "/json_arr?select=data->>c->b"
        `shouldRespondWith` (
        if actualPgVersion >= pgVersion112 then
        [json|
          {"hint":"No operator matches the given name and argument types. You might need to add explicit type casts.",
           "details":null,"code":"42883","message":"operator does not exist: text -> unknown"} |]
           else
        [json|
          {"hint":"No operator matches the given name and argument type(s). You might need to add explicit type casts.",
           "details":null,"code":"42883","message":"operator does not exist: text -> unknown"} |]
                            )
        { matchStatus  = 404 , matchHeaders = [] }

    context "with array index" $ do
      it "can get array of ints and alias/cast it" $ do
        get "/json_arr?select=data->>0::int&id=in.(1,2)" `shouldRespondWith`
          [json| [{"data":1}, {"data":4}] |]
          { matchHeaders = [matchContentTypeJson] }
        get "/json_arr?select=idx0:data->>0::int,idx1:data->>1::int&id=in.(1,2)" `shouldRespondWith`
          [json| [{"idx0":1,"idx1":2}, {"idx0":4,"idx1":5}] |]
          { matchHeaders = [matchContentTypeJson] }

      it "can get nested array of ints" $ do
        get "/json_arr?select=data->0->>1::int&id=in.(3,4)" `shouldRespondWith`
          [json| [{"data":8}, {"data":7}] |]
          { matchHeaders = [matchContentTypeJson] }
        get "/json_arr?select=data->0->0->>1::int&id=in.(3,4)" `shouldRespondWith`
          [json| [{"data":null}, {"data":6}] |]
          { matchHeaders = [matchContentTypeJson] }

      it "can get array of objects" $ do
        get "/json_arr?select=data->0->>a&id=in.(5,6)" `shouldRespondWith`
          [json| [{"a":"A"}, {"a":"[1,2,3]"}] |]
          { matchHeaders = [matchContentTypeJson] }
        get "/json_arr?select=data->0->a->>2&id=in.(5,6)" `shouldRespondWith`
          [json| [{"a":null}, {"a":"3"}] |]
          { matchHeaders = [matchContentTypeJson] }

      it "can get array in object keys" $ do
        get "/json_arr?select=data->c->>0::json&id=in.(7,8)" `shouldRespondWith`
          [json| [{"c":1}, {"c":{"d": [4,5,6,7,8]}}] |]
          { matchHeaders = [matchContentTypeJson] }
        get "/json_arr?select=data->c->0->d->>4::int&id=in.(7,8)" `shouldRespondWith`
          [json| [{"d":null}, {"d":8}] |]
          { matchHeaders = [matchContentTypeJson] }

      it "only treats well formed numbers as indexes" $
        get "/json_arr?select=data->0->0xy1->1->23-xy-45->1->xy-6->>0::int&id=eq.9" `shouldRespondWith`
          [json| [{"xy-6":3}] |]
          { matchHeaders = [matchContentTypeJson] }

    context "finishing json path with single arrow ->" $ do
      it "works when finishing with a key" $ do
        get "/json_arr?select=data->c&id=in.(7,8)" `shouldRespondWith`
          [json| [{"c":[1,2,3]}, {"c":[{"d": [4,5,6,7,8]}]}] |]
          { matchHeaders = [matchContentTypeJson] }
        get "/json_arr?select=data->0->a&id=in.(5,6)" `shouldRespondWith`
          [json| [{"a":"A"}, {"a":[1,2,3]}] |]
          { matchHeaders = [matchContentTypeJson] }

      it "works when finishing with an index" $ do
        get "/json_arr?select=data->0->a&id=in.(5,6)" `shouldRespondWith`
          [json| [{"a":"A"}, {"a":[1,2,3]}] |]
          { matchHeaders = [matchContentTypeJson] }
        get "/json_arr?select=data->c->0->d&id=eq.8" `shouldRespondWith`
          [json| [{"d":[4,5,6,7,8]}] |]
          { matchHeaders = [matchContentTypeJson] }

  context "filtering response" $ do
    it "can filter by properties inside json column" $ do
      get "/json?data->foo->>bar=eq.baz" `shouldRespondWith`
        [json| [{"data": {"id": 1, "foo": {"bar": "baz"}}}] |]
        { matchHeaders = [matchContentTypeJson] }
      get "/json?data->foo->>bar=eq.fake" `shouldRespondWith`
        [json| [] |]
        { matchHeaders = [matchContentTypeJson] }

    it "can filter by properties inside json column using not" $
      get "/json?data->foo->>bar=not.eq.baz" `shouldRespondWith`
        [json| [] |]
        { matchHeaders = [matchContentTypeJson] }

    it "can filter by properties inside json column using ->>" $
      get "/json?data->>id=eq.1" `shouldRespondWith`
        [json| [{"data": {"id": 1, "foo": {"bar": "baz"}}}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "can be filtered with and/or" $
      get "/grandchild_entities?or=(jsonb_col->a->>b.eq.foo, jsonb_col->>b.eq.bar)&select=id" `shouldRespondWith`
        [json|[{id: 4}, {id: 5}]|] { matchStatus = 200, matchHeaders = [matchContentTypeJson] }

    it "can filter by array indexes" $ do
      get "/json_arr?select=data&data->>0=eq.1" `shouldRespondWith`
        [json| [{"data":[1, 2, 3]}] |]
        { matchHeaders = [matchContentTypeJson] }
      get "/json_arr?select=data&data->1->>2=eq.13" `shouldRespondWith`
        [json| [{"data":[[9, 8, 7], [11, 12, 13]]}] |]
        { matchHeaders = [matchContentTypeJson] }
      get "/json_arr?select=data&data->1->>b=eq.B" `shouldRespondWith`
        [json| [{"data":[{"a": "A"}, {"b": "B"}]}] |]
        { matchHeaders = [matchContentTypeJson] }
      get "/json_arr?select=data&data->1->b->>1=eq.5" `shouldRespondWith`
        [json| [{"data":[{"a": [1,2,3]}, {"b": [4,5]}]}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "can filter jsonb" $ do
      get "/jsonb_test?data=eq.{\"e\":1}" `shouldRespondWith`
        [json| [{"id":4,"data":{"e": 1}}] |]
        { matchHeaders = [matchContentTypeJson] }
      get "/jsonb_test?data->a=eq.{\"b\":2}" `shouldRespondWith`
        [json| [{"id":1,"data":{"a": {"b": 2}}}] |]
        { matchHeaders = [matchContentTypeJson] }
      get "/jsonb_test?data->c=eq.[1,2,3]" `shouldRespondWith`
        [json| [{"id":2,"data":{"c": [1, 2, 3]}}] |]
        { matchHeaders = [matchContentTypeJson] }
      get "/jsonb_test?data->0=eq.{\"d\":\"test\"}" `shouldRespondWith`
        [json| [{"id":3,"data":[{"d": "test"}]}] |]
        { matchHeaders = [matchContentTypeJson] }

  context "ordering response" $ do
    it "orders by a json column property asc" $
      get "/json?order=data->>id.asc" `shouldRespondWith`
        [json| [{"data": {"id": 0}}, {"data": {"id": 1, "foo": {"bar": "baz"}}}, {"data": {"id": 3}}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "orders by a json column with two level property nulls first" $
      get "/json?order=data->foo->>bar.nullsfirst" `shouldRespondWith`
        [json| [{"data": {"id": 3}}, {"data": {"id": 0}}, {"data": {"id": 1, "foo": {"bar": "baz"}}}] |]
        { matchHeaders = [matchContentTypeJson] }

  context "Patching record, in a nonempty table" $
    it "can set a json column to escaped value" $ do
      _ <- post "/json" [json| { data: {"escaped":"bar"} } |]
      request methodPatch "/json?data->>escaped=eq.bar"
                   [("Prefer", "return=representation")]
                   [json| { "data": { "escaped":" \"bar" } } |]
        `shouldRespondWith` [json| [{ "data": { "escaped":" \"bar" } }] |]
        { matchStatus  = 200 , matchHeaders = [] }

  when (actualPgVersion >= pgVersion95) $
    context "json array negative index" $ do
      it "can select with negative indexes" $ do
        get "/json_arr?select=data->>-1::int&id=in.(1,2)" `shouldRespondWith`
          [json| [{"data":3}, {"data":6}] |]
          { matchHeaders = [matchContentTypeJson] }
        get "/json_arr?select=data->0->>-2::int&id=in.(3,4)" `shouldRespondWith`
          [json| [{"data":8}, {"data":7}] |]
          { matchHeaders = [matchContentTypeJson] }
        get "/json_arr?select=data->-2->>a&id=in.(5,6)" `shouldRespondWith`
          [json| [{"a":"A"}, {"a":"[1,2,3]"}] |]
          { matchHeaders = [matchContentTypeJson] }

      it "can filter with negative indexes" $ do
        get "/json_arr?select=data&data->>-3=eq.1" `shouldRespondWith`
          [json| [{"data":[1, 2, 3]}] |]
          { matchHeaders = [matchContentTypeJson] }
        get "/json_arr?select=data&data->-1->>-3=eq.11" `shouldRespondWith`
          [json| [{"data":[[9, 8, 7], [11, 12, 13]]}] |]
          { matchHeaders = [matchContentTypeJson] }
        get "/json_arr?select=data&data->-1->>b=eq.B" `shouldRespondWith`
          [json| [{"data":[{"a": "A"}, {"b": "B"}]}] |]
          { matchHeaders = [matchContentTypeJson] }
        get "/json_arr?select=data&data->-1->b->>-1=eq.5" `shouldRespondWith`
          [json| [{"data":[{"a": [1,2,3]}, {"b": [4,5]}]}] |]
          { matchHeaders = [matchContentTypeJson] }

      it "should fail on badly formed negatives" $ do
        get "/json_arr?select=data->>-78xy" `shouldRespondWith`
          [json|
            {"details": "unexpected 'x' expecting digit, \"->\", \"::\" or end of input",
             "message": "\"failed to parse select parameter (data->>-78xy)\" (line 1, column 11)"} |]
          { matchStatus = 400, matchHeaders = [matchContentTypeJson] }
        get "/json_arr?select=data->>--34" `shouldRespondWith`
          [json|
            {"details": "unexpected \"-\" expecting digit",
             "message": "\"failed to parse select parameter (data->>--34)\" (line 1, column 9)"} |]
          { matchStatus = 400, matchHeaders = [matchContentTypeJson] }
        get "/json_arr?select=data->>-xy-4" `shouldRespondWith`
          [json|
            {"details":"unexpected \"x\" expecting digit",
             "message":"\"failed to parse select parameter (data->>-xy-4)\" (line 1, column 9)"} |]
          { matchStatus = 400, matchHeaders = [matchContentTypeJson] }
