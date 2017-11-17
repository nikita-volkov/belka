module Belka.Poking
where

import Belka.Prelude
import Ptr.Poking
import qualified Iri.Data as J
import qualified Iri.Rendering.Ptr.Poking.Ascii as K
import qualified Ptr.ByteString as O
import qualified Data.ByteString.Base64 as A
import qualified Data.Text.Encoding as B


host :: J.Host -> Poking
host =
  K.host

path :: J.Path -> Poking
path path =
  asciiChar '/' <> K.path path

queryString :: J.Query -> Poking
queryString query =
  asciiChar '?' <> K.query query

basicAuth :: Text -> Text -> Poking
basicAuth user password =
  bytes "Basic " <> bytes (A.encode bodyBytes)
  where
    bodyBytes =
      if password == ""
        then B.encodeUtf8 user
        else O.poking (bytes (B.encodeUtf8 user) <> asciiChar ':' <> bytes (B.encodeUtf8 password))
