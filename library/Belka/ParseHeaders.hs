module Belka.ParseHeaders
where

import Belka.Prelude
import qualified Data.CaseInsensitive as B
import qualified Data.HashMap.Strict as D
import qualified Network.HTTP.Client as A
import qualified Network.HTTP.Types as C


newtype ParseHeaders a =
  ParseHeaders (ReaderT (HashMap ByteString ByteString) (Except Text) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadError Text)

header :: ByteString -> ParseHeaders ByteString
header name =
  ParseHeaders (ReaderT (except . maybe (Left ("Header not found: " <> showText name)) Right . D.lookup name))
