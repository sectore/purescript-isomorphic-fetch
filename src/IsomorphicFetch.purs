module IsomorphicFetch
  ( FETCH
  , URI
  , get
  , json
  , text
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Promise (toAffE, Promise)
import Data.Foreign (Foreign)
import Data.URI.URI (print) as URI
import Network.HTTP.Types.Exchange (Request(..), Response, get) as HTTP

type URI = String

foreign import data FETCH :: Effect

type FetchEff eff =  (fetch:: FETCH, console :: CONSOLE | eff)

newtype FetchRequest = FetchRequest
  { uri :: String
  , method :: String
  , body :: String
  }

toFetchRequest :: HTTP.Request -> FetchRequest
toFetchRequest (HTTP.Request r) =
  FetchRequest { uri: URI.print r.uri
                , method: show r.method
                , body: ""
                }

foreign import fetchImpl :: forall eff. FetchRequest -> Eff (FetchEff eff) (Promise HTTP.Response)

fetch :: forall eff. HTTP.Request -> Aff (FetchEff eff) HTTP.Response
fetch = toAffE <<< fetchImpl <<< toFetchRequest

get :: forall eff. URI -> Aff (FetchEff eff) HTTP.Response
get uri = fetch =<< HTTP.get uri

-- TODO: Add post, delete, put etc.

foreign import jsonImpl :: forall eff. HTTP.Response -> Eff (FetchEff eff) (Promise Foreign)

json :: forall eff. HTTP.Response -> Aff (FetchEff eff) Foreign
json = toAffE <<< jsonImpl

foreign import textImpl :: forall eff. HTTP.Response -> Eff (FetchEff eff) (Promise String)

text :: forall eff. HTTP.Response -> Aff (FetchEff eff) String
text = toAffE <<< textImpl
