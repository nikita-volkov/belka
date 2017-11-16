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
import qualified Data.ByteString.Builder as F
import qualified Data.Text as H


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

setAcceptHeaderToJson :: Request
setAcceptHeaderToJson =
  setAcceptHeader "application/json"

setContentTypeHeaderToJson :: Request
setContentTypeHeaderToJson =
  setContentTypeHeader "application/json"

setUrl :: Text -> Maybe Request
setUrl url =
  do
    parsedRequest <- A.parseRequest (H.unpack url)
    return $ endo $ \ request ->
      request {
        A.secure = A.secure parsedRequest,
        A.host = A.host parsedRequest,
        A.port = A.port parsedRequest,
        A.path = A.path parsedRequest,
        A.queryString = A.queryString parsedRequest
      }

setUrlUnsafe :: Text -> Request
setUrlUnsafe url =
  fromMaybe (error ("Invalid URL: " <> show url)) (setUrl url)

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
