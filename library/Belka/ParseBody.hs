module Belka.ParseBody
where

import Belka.Prelude
-- import qualified Network.CURL730 as A
import qualified Belka.Potoki.Consume as H
import qualified Potoki.Consume as F
-- import qualified Potoki.Core.Consume as F
import qualified Potoki.IO as E
import qualified Aeson.ValueParser as G
import qualified Data.Aeson as I
import qualified Data.ByteString as B


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

--------------------------------------------------------------------------------

-- newtype ParseBody a =
--   ParseBody (Compose IO (Compose ((,) A.CURL_write_callback) (Compose IO (Either Text))) a)
--   deriving (Functor, Applicative)

--------------------------------------------------------------------------------

-- newtype ParseBody a =
--   ParseBody (IO (A.CURL_write_callback, IO (Either Text a)))
--   deriving (Functor)

-- consume :: F.Consume ByteString (Either Text consumed) -> ParseBody consumed
-- consume consume =
--   -- E.consume $ \ nil just ->
--   ParseBody $ do
--     activeVar <- newTVarIO True
--     chan <- newEmptyTMVarIO
--     return (writeCallback chan activeVar, resultIO chan activeVar)
--   where
--     writeCallback chan activeVar chunk =
--       do
        

