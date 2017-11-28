module Belka.ParseBody
where

import Belka.Prelude
import qualified Belka.ParseHeaders as E
import qualified Belka.Potoki.Consume as H
import qualified Potoki.Consume as F
import qualified Aeson.ValueParser as G
import qualified Data.Aeson as I


newtype ParseBody a =
  ParseBody (Compose (F.Consume ByteString) (Either Text) a)
  deriving (Functor, Applicative)

consume :: F.Consume ByteString (Either Text body) -> ParseBody body
consume consume =
  ParseBody (Compose consume)

json :: ParseBody I.Value
json =
  consume (H.json)

parseJson :: G.Value json -> ParseBody json
parseJson jsonParser =
  consume (H.parseJson jsonParser)

{-| Useful for debugging -}
print :: ParseBody ()
print =
  consume (fmap Right F.printBytes)
