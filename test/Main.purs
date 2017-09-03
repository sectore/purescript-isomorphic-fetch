module Test.Main where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (MultipleErrors)
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Foreign.Generic.Types (Options)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import IsomorphicFetch (FETCH, get, json, text)

main :: forall e. Eff (console :: CONSOLE, fetch :: FETCH, exception :: EXCEPTION | e) Unit
main = void $ launchAff $ do
  print "GET"
  response <- get "http://jsonplaceholder.typicode.com/posts/1"
  -- print "text: "
  -- textPost <- text response
  -- print textPost
  print "json: "
  json' <- json response
  let decoded = (runExcept $ decode json') :: Either MultipleErrors Post
  _ <- case decoded of
          Left err ->
            print $ "Error while decoding Post: " <> show err
          Right post ->
            print $ show post
  print "finished"
  where
    print a = liftEff $ log a


jsonDecodeOptions :: Options
jsonDecodeOptions = defaultOptions { unwrapSingleConstructors = true }

newtype Post = Post
  { userId :: Int
  , id :: Int
  , title :: String
  , body :: String
  }
derive instance gPost :: Generic Post _
instance showPost :: Show Post where
  show = genericShow
instance encodePost :: Decode Post where
  decode = genericDecode jsonDecodeOptions
