module Belka.ParseHeaders
where

import Belka.Prelude
import qualified Data.CaseInsensitive as B
import qualified Data.HashMap.Strict as D
import qualified Network.HTTP.Client as A
import qualified Network.HTTP.Types as C
import qualified Belka.Ptr.Parse as E
import qualified Ptr.ByteString as F


newtype ParseHeaders a =
  ParseHeaders (ReaderT (HashMap ByteString ByteString) (Except Text) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadError Text)

header :: ByteString -> ParseHeaders ByteString
header name =
  ParseHeaders (ReaderT (except . maybe (Left ("Header not found: " <> showText name)) Right . D.lookup name))

contentType :: ParseHeaders (ByteString, HashMap ByteString ByteString)
contentType =
  do
    value <- header "content-type"
    F.parse value (fmap return E.contentTypeHeader) (\ _ -> throwError "Not enough data") throwError

charset :: ParseHeaders ByteString
charset =
  do
    (_, detailsTable) <- contentType
    maybe (throwError "No Charset specified in the Content-Type header") return (D.lookup "charset" detailsTable)
