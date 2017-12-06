module Belka.ParseHead
where

import Belka.Prelude
import qualified Network.CURL730 as A
import qualified Belka.ParseHeaders as E
import qualified Belka.Potoki.Consume as H
import qualified Potoki.Consume as F


newtype ParseHead a =
  ParseHead (ExceptT Text (ReaderT GetInfo IO) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadError Text)

newtype GetInfo =
  GetInfo (forall a. A.CURLinfo a -> IO a)

status :: ParseHead Int
status =
  ParseHead (ExceptT (ReaderT io))
  where
    io (GetInfo getInfoIO) =
      fmap (bimap textError fromIntegral) (try (getInfoIO A.CURLINFO_RESPONSE_CODE))
      where
        textError (e :: A.CURLE) =
          "Failed getting the status: " <> showText e
