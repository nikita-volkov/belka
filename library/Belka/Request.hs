module Belka.Request
where

import Belka.Prelude
import qualified Network.HTTP.Client as A
import qualified Data.CaseInsensitive as B
import qualified Potoki.Produce as C
import qualified Potoki.Core.Produce as C
import qualified Potoki.Core.Fetch as I
import qualified Potoki.IO as D
import qualified JSONBytesBuilder.Builder as E
import qualified JSONBytesBuilder.ByteString.Builder as G
import qualified Data.ByteString as L
import qualified Data.ByteString.Builder as F
import qualified Data.Text as H
import qualified Iri.Data as J
import qualified Iri.Rendering.Ptr.Poking.Ascii as K
import qualified Ptr.Poking as M
import qualified Ptr.ByteString as O
import qualified Belka.Poking as P


newtype Request =
  Request (A.Request -> IO (A.Request, IO ()))

instance Semigroup Request where
  (<>) (Request leftIO) (Request rightIO) =
    Request $ \ !hcRequest ->
    do
      (leftRequest, leftCleanUp) <- leftIO hcRequest
      (rightRequest, rightCleanUp) <- rightIO leftRequest
      return (rightRequest, leftCleanUp >> rightCleanUp)

instance Monoid Request where
  mempty =
    Request (\ hcRequest -> return (hcRequest, return ()))
  mappend =
    (<>)

endo :: (A.Request -> A.Request) -> Request
endo endo =
  Request $ \ hcRequest -> return (endo hcRequest, return ())

setHeader :: ByteString -> ByteString -> Request
setHeader name value =
  endo (\ x -> x {A.requestHeaders = newHeaders (A.requestHeaders x)})
  where
    newHeaders oldHeaders =
      (B.mk name, value) : oldHeaders

setAcceptHeader :: ByteString -> Request
setAcceptHeader value =
  setHeader "accept" value

setContentTypeHeader :: ByteString -> Request
setContentTypeHeader value =
  setHeader "content-type" value

setBasicAuthHeader :: Text -> Text -> Request
setBasicAuthHeader user password =
  setHeader "authorization" (O.poking (P.basicAuth user password))

setAcceptHeaderToJson :: Request
setAcceptHeaderToJson =
  setAcceptHeader "application/json"

setContentTypeHeaderToJson :: Request
setContentTypeHeaderToJson =
  setContentTypeHeader "application/json"

setIri :: Iri -> Request
setIri (J.Iri scheme authority host port path query fragment) =
  setPathsAndStuff <> setHeaders
  where
    setPathsAndStuff =
      endo $ \ request ->
        request {
          A.secure = secure,
          A.host = preparedHost,
          A.port = preparedPort,
          A.path = preparedPath,
          A.queryString = preparedQuery
        }
      where
        secure =
          scheme == J.Scheme "https"
        preparedHost =
          O.poking (K.host host)
        preparedPort =
          case port of
            J.PresentPort value -> fromIntegral value
            J.MissingPort -> if secure then 443 else 80
        preparedPath =
          O.poking (M.asciiChar '/' <> K.path path)
        preparedQuery =
          O.poking $
          case K.query query of
            query -> if M.null query then mempty else M.asciiChar '?' <> query
    setHeaders =
      case authority of
        J.PresentAuthority (J.User user) password -> case password of
          J.PresentPassword password -> setBasicAuthHeader user password
          J.MissingPassword -> setBasicAuthHeader user ""
        J.MissingAuthority -> mempty

setMethod :: ByteString -> Request
setMethod method =
  endo (\ x -> x {A.method = method})

setMethodToGet :: Request
setMethodToGet =
  setMethod "get"

setMethodToPost :: Request
setMethodToPost =
  setMethod "post"

setMethodToDelete :: Request
setMethodToDelete =
  setMethod "delete"

setMethodToHead :: Request
setMethodToHead =
  setMethod "head"

setBody :: ByteString -> Request
setBody bytes =
  endo $ \ request ->
  request { A.requestBody = A.RequestBodyBS bytes }

produceBody :: C.Produce ByteString -> Request
produceBody (C.Produce produceIO) =
  Request $ \ hcRequest ->
  do
    (fetch, cleanUp) <- produceIO
    return
      ((,)
        (hcRequest { A.requestBody = A.RequestBodyStreamChunked (givesPopper fetch) })
        cleanUp)
  where
    givesPopper (I.Fetch fetchIO) takesPopper =
      takesPopper (fetchIO mempty id)

buildBody :: F.Builder -> Request
buildBody builder =
  endo $ \ request ->
  request { A.requestBody = A.RequestBodyLBS (F.toLazyByteString builder) }

buildJsonBody :: E.Literal -> Request
buildJsonBody builder =
  buildBody (G.jsonLiteral builder) <> setContentTypeHeaderToJson
