module Belka.Request
where

import Belka.Prelude hiding (option, Option)
import qualified Network.CURL730 as A
import qualified Potoki.Produce as C
import qualified Potoki.Core.Produce as C
import qualified Potoki.Core.Fetch as I
import qualified Potoki.IO as D
import qualified JSONBytesBuilder.Builder as E
import qualified JSONBytesBuilder.ByteString.Builder as G
import qualified Data.ByteString as L
import qualified Data.ByteString.Builder as F
import qualified Data.ByteString.Char8 as R
import qualified Data.Text as H
import qualified Iri.Data as J
import qualified Iri.Rendering.ByteString as K
import qualified Ptr.Poking as M
import qualified Ptr.ByteString as O
import qualified Belka.Ptr.Poking as P


data Request =
  Request ![A.CURLoption] !(IO ([A.CURLoption], IO ()))

request :: Redirects -> Timeout -> Method -> Iri -> [Header] -> Body -> Request
request (Redirects redirectsOptions) (Timeout timeoutOptions) (Method methodOption) iri headers (Body bodyIO) =
  Request pureOptions bodyIO
  where
    pureOptions =
      redirectsOptions <>
      timeoutOptions <>
      [iriOption, headersOption, methodOption]
      where
        iriOption =
          A.CURLOPT_URL string
          where
            string =
              R.unpack (K.uri iri)
        headersOption =
          A.CURLOPT_HTTPHEADER stringList
          where
            stringList =
              fmap (\ (Header x) -> x) headers


newtype Redirects =
  Redirects [A.CURLoption]

noRedirects :: Redirects
noRedirects =
  Redirects [A.CURLOPT_FOLLOWLOCATION False]

redirects :: Int -> Redirects
redirects amount =
  Redirects [A.CURLOPT_FOLLOWLOCATION True, A.CURLOPT_MAXREDIRS (fromIntegral amount)]


newtype Timeout =
  Timeout [A.CURLoption]

noTimeout :: Timeout
noTimeout =
  Timeout []

{-| Set timeout in millis -}
millisTimeout :: Int -> Timeout
millisTimeout millis =
  Timeout [A.CURLOPT_TIMEOUT_MS (fromIntegral millis)]


newtype Method =
  Method A.CURLoption

method :: ByteString -> Method
method method =
  Method (A.CURLOPT_CUSTOMREQUEST (R.unpack method))

headMethod :: Method
headMethod =
  method "head"

getMethod :: Method
getMethod =
  method "get"

postMethod :: Method
postMethod =
  method "post"

putMethod :: Method
putMethod =
  method "put"

deleteMethod :: Method
deleteMethod =
  method "delete"


newtype Header =
  Header String

header :: ByteString -> ByteString -> Header
header name value =
  Header (R.unpack name <> ": " <> R.unpack value)

acceptHeader :: ByteString -> Header
acceptHeader value =
  header "accept" value

jsonAcceptHeader :: Header
jsonAcceptHeader =
  acceptHeader "application/json"

htmlAcceptHeader :: Header
htmlAcceptHeader =
  acceptHeader "text/html"

acceptLanguageHeader :: ByteString -> Header
acceptLanguageHeader =
  header "accept-language"

contentTypeHeader :: ByteString -> Header
contentTypeHeader value =
  header "content-type" value

jsonContentTypeHeader :: Header
jsonContentTypeHeader =
  contentTypeHeader "application/json"

basicAuthHeader :: Text -> Text -> Header
basicAuthHeader user password =
  header "authorization" (O.poking (P.basicAuth user password))

userAgentHeader :: ByteString -> Header
userAgentHeader =
  header "user-agent"


newtype Body =
  Body (IO ([A.CURLoption], IO ()))

produceBody :: C.Produce ByteString -> Body
produceBody (C.Produce produceIO) =
  Body $ do
    (I.Fetch fetchIO, releaseIO) <- produceIO
    unconsumedRef <- newIORef mempty
    let
      readFunction maxSize =
        do
          unconsumed <- readIORef unconsumedRef
          if L.null unconsumed
            then 
              fix $ \ loop ->
              join $
              fetchIO
                (return (A.CURL_READFUNC_OK mempty))
                (\ bytes ->
                  if L.null bytes
                    then loop
                    else do
                      let (left, right) = L.splitAt maxSize bytes
                      writeIORef unconsumedRef right
                      return (A.CURL_READFUNC_OK bytes))
            else
              do
                let (left, right) = L.splitAt maxSize unconsumed
                writeIORef unconsumedRef right
                return (A.CURL_READFUNC_OK unconsumed)
    return ([A.CURLOPT_READFUNCTION (Just readFunction)], releaseIO)
