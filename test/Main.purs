module Test.Main where

import Prelude

import Control.Monad.Aff (attempt)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except (runExcept)
import Control.Monad.Free (Free)
import Data.Either (Either(..), isLeft, isRight)
import Data.Foreign (Foreign, MultipleErrors)
import Data.Foreign.Class (class Decode, class Encode, decode)
import Data.Foreign.Generic (defaultOptions, encodeJSON, genericDecode, genericEncode)
import Data.Foreign.Generic.Types (Options)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import IsomorphicFetch (FETCH, delete, get, json, patch, patch', post, post', put, put', text)
import Test.Unit (TestF, failure, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

-- TODO: It would be more than nice to use `nock` or similar for mocking / stubbing API.
-- Using a "real" API, such as `jsonplaceholder.typecode.com`, should be avoided for testing.
-- Anyway, this ^ will be a task for later :)

main :: forall eff. Eff ( console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR, fetch :: FETCH | eff) Unit
main = runTest do
  testDelete
  testGet
  testPatch
  testPost
  testPut
  testTxt
  testJson

testGet :: forall eff. Free (TestF (console :: CONSOLE, fetch :: FETCH | eff)) Unit
testGet =
  suite "GET" do
    test "results in a successfull response" do
      responseE <- attempt $ get "http://jsonplaceholder.typicode.com/posts/1"
      Assert.equal (isRight responseE) true
    test "results in an error" do
      responseE <- attempt $ get "http://xxx"
      Assert.equal (isLeft responseE) true

testPost :: forall eff. Free (TestF (console :: CONSOLE, fetch :: FETCH | eff)) Unit
testPost =
  suite "POST" do
    test "results in a successfull response" do
      responseE <- attempt $ post "http://jsonplaceholder.typicode.com/posts"
      Assert.equal (isRight responseE) true
    test "with body results in a successfull response" do
      responseE <- attempt $ post' "http://jsonplaceholder.typicode.com/posts" (encodeJSON simplePost)
      Assert.equal (isRight responseE) true

testPut :: forall eff. Free (TestF (console :: CONSOLE, fetch :: FETCH | eff)) Unit
testPut =
  suite "PUT" do
    test "results in a successfull response" do
      responseE <- attempt $ put "http://jsonplaceholder.typicode.com/posts"
      Assert.equal (isRight responseE) true
    test "with body results in a successfull response" do
      responseE <- attempt $ put' "http://jsonplaceholder.typicode.com/posts" (encodeJSON simplePost)
      Assert.equal (isRight responseE) true

testPatch :: forall eff. Free (TestF (console :: CONSOLE, fetch :: FETCH | eff)) Unit
testPatch =
  suite "PATCH" do
    test "results in a successfull response" do
      responseE <- attempt $ patch "http://jsonplaceholder.typicode.com/posts/1"
      Assert.equal (isRight responseE) true
    test "with body results in a successfull response" do
      responseE <- attempt $ patch' "http://jsonplaceholder.typicode.com/posts/1" "{title: \"foo\"})"
      Assert.equal (isRight responseE) true

testDelete :: forall eff. Free (TestF (console :: CONSOLE, fetch :: FETCH | eff)) Unit
testDelete =
  suite "DELETE" do
    test "results in a successfull response" do
      responseE <- attempt $ delete "http://jsonplaceholder.typicode.com/posts/1"
      Assert.equal (isRight responseE) true

testTxt :: forall eff. Free (TestF (console :: CONSOLE, fetch :: FETCH | eff)) Unit
testTxt =
  suite "text" do
    test "returns text of a response" do
      response <- get "http://jsonplaceholder.typicode.com/posts/1"
      text' <- text response
      let expected = "{\n  \"userId\": 1,\n  \"id\": 1,\n  \"title\": \"sunt aut facere repellat provident occaecati excepturi optio reprehenderit\",\n  \"body\": \"quia et suscipit\\nsuscipit recusandae consequuntur expedita et cum\\nreprehenderit molestiae ut ut quas totam\\nnostrum rerum est autem sunt rem eveniet architecto\"\n}"
      Assert.equal text' expected

testJson :: forall eff. Free (TestF (console :: CONSOLE, fetch :: FETCH | eff)) Unit
testJson =
  suite "json" do
    test "returns json of a response" do
      response <- get "http://jsonplaceholder.typicode.com/posts/1"
      json' <- json response
      case decodePost json' of
        Left err ->
          failure $ "json decode error" <> show err
        Right post ->
          Assert.equal post defaultPost

------------------------------------------------------
-- Mock data / test helpers
------------------------------------------------------

decodePost :: Foreign -> Either MultipleErrors Post
decodePost d = runExcept $ decode d

jsonGenericOptions :: Options
jsonGenericOptions = defaultOptions { unwrapSingleConstructors = true }

newtype Post = Post
  { userId :: Int
  , id :: Int
  , title :: String
  , body :: String
  }

derive instance gPost :: Generic Post _
instance showPost :: Show Post where
  show = genericShow
instance eqPost :: Eq Post where
  eq = genericEq
instance dcPost :: Decode Post where
  decode = genericDecode jsonGenericOptions
instance ecPost :: Encode Post where
  encode = genericEncode jsonGenericOptions

defaultPost :: Post
defaultPost = Post
  { userId: 1
  , id: 1
  , title: "sunt aut facere repellat provident occaecati excepturi optio reprehenderit"
  , body: "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto"
 }

simplePost :: Post
simplePost = Post
  { userId: 1
  , id: 1
  , title: "foo"
  , body: "bar"
  }
