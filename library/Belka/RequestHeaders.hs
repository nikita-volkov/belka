module Belka.RequestHeaders
where

import Belka.Prelude
import qualified Data.HashMap.Strict as B
import qualified Data.ByteString as L
import qualified Ptr.ByteString as O
import qualified Belka.Ptr.Poking as P


{-| Composable specification of headers -}
newtype RequestHeaders =
  RequestHeaders (Endo (HashMap ByteString ByteString))
  deriving (Semigroup, Monoid)

header :: ByteString -> ByteString -> RequestHeaders
header name value =
  properHeader (lowerCaseBytesInIso8859_1 name) value

properHeader :: ByteString -> ByteString -> RequestHeaders
properHeader name value =
  RequestHeaders (Endo (B.insert name value))

accept :: ByteString -> RequestHeaders
accept value =
  properHeader "accept" value

jsonAccept :: RequestHeaders
jsonAccept =
  accept "application/json"

htmlAccept :: RequestHeaders
htmlAccept =
  accept "text/html"

acceptLanguage :: ByteString -> RequestHeaders
acceptLanguage =
  properHeader "accept-language"

contentType :: ByteString -> RequestHeaders
contentType value =
  properHeader "content-type" value

jsonContentType :: RequestHeaders
jsonContentType =
  contentType "application/json"

basicAuth :: Text -> Text -> RequestHeaders
basicAuth user password =
  properHeader "authorization" (O.poking (P.basicAuth user password))

userAgent :: ByteString -> RequestHeaders
userAgent =
  properHeader "user-agent"
