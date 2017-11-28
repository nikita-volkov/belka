module Belka.Potoki.Consume
where

import Belka.Prelude
import Potoki.Consume
import qualified Aeson.ValueParser as A
import qualified Data.Aeson.Parser as B
import qualified Data.Aeson as C


json :: Consume ByteString (Either Text C.Value)
json =
  parseBytes B.json'

lazyJson :: Consume ByteString (Either Text C.Value)
lazyJson =
  parseBytes B.json

parseJson :: A.Value parsed -> Consume ByteString (Either Text parsed)
parseJson parser =
  fmap (>>= A.run parser) lazyJson
