module IsomorphicFetch
  ( FETCH
  , FetchEff
  , URIString
  , delete
  , get
  , json
  , post
  , post'
  , patch
  , patch'
  , put
  , put'
  , text
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Promise (toAffE, Promise)
import Data.Foreign (Foreign, toForeign)
import Data.Foreign.NullOrUndefined (undefined)
import Data.Maybe (maybe)
import Data.URI.URI (print) as URI
import Network.HTTP.Types.Exchange (Request(..), Response, delete, get, patch
  , patchWithBody, post, postWithBody, put, putWithBody) as HTTP

-- | Type alias of an URI as string
type URIString = String

-- | FETCH effect
foreign import data FETCH :: Effect

type FetchEff eff =
  ( fetch:: FETCH
  -- , console :: CONSOLE
  | eff
  )

newtype FetchRequest = FetchRequest
  { uri :: String
  , method :: String
  , body :: Foreign -- String or undefined
  }

toFetchRequest :: HTTP.Request -> FetchRequest
toFetchRequest (HTTP.Request r) =
  FetchRequest { uri: URI.print r.uri
                , method: show r.method
                , body: maybe undefined toForeign r.body
                }

foreign import fetchImpl :: forall eff. FetchRequest -> Eff (FetchEff eff) (Promise HTTP.Response)

fetch :: forall eff. HTTP.Request -> Aff (FetchEff eff) HTTP.Response
fetch = toAffE <<< fetchImpl <<< toFetchRequest

get :: forall eff. URIString -> Aff (FetchEff eff) HTTP.Response
get uri = fetch =<< HTTP.get uri

post :: forall eff. URIString -> Aff (FetchEff eff) HTTP.Response
post uri = fetch =<< HTTP.post uri

post' :: forall eff. URIString -> String -> Aff (FetchEff eff) HTTP.Response
post' uri body = fetch =<< HTTP.postWithBody uri body

put :: forall eff. URIString -> Aff (FetchEff eff) HTTP.Response
put uri = fetch =<< HTTP.put uri

put' :: forall eff. URIString -> String -> Aff (FetchEff eff) HTTP.Response
put' uri body = fetch =<< HTTP.putWithBody uri body

patch :: forall eff. URIString -> Aff (FetchEff eff) HTTP.Response
patch uri = fetch =<< HTTP.patch uri

patch' :: forall eff. URIString -> String -> Aff (FetchEff eff) HTTP.Response
patch' uri body = fetch =<< HTTP.patchWithBody uri body

delete :: forall eff. URIString -> Aff (FetchEff eff) HTTP.Response
delete uri = fetch =<< HTTP.delete uri

foreign import jsonImpl :: forall eff. HTTP.Response -> Eff (FetchEff eff) (Promise Foreign)

json :: forall eff. HTTP.Response -> Aff (FetchEff eff) Foreign
json = toAffE <<< jsonImpl

foreign import textImpl :: forall eff. HTTP.Response -> Eff (FetchEff eff) (Promise String)

text :: forall eff. HTTP.Response -> Aff (FetchEff eff) String
text = toAffE <<< textImpl
