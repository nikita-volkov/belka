module Belka.ParseHead
where

import Belka.Prelude
import qualified Data.CaseInsensitive as B
import qualified Data.HashMap.Strict as D
import qualified Network.HTTP.Client as A
import qualified Network.HTTP.Types as C
import qualified Belka.ParseHeaders as E
import qualified Belka.Potoki.Consume as H
import qualified Potoki.Consume as F
import qualified Aeson.ValueParser as G


newtype ParseHead a =
  ParseHead (ExceptT Text (ReaderT (A.Response A.BodyReader) IO) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadError Text)

liftIOFn :: (A.Response A.BodyReader -> IO (Either Text a)) -> ParseHead a
liftIOFn fn =
  ParseHead (ExceptT (ReaderT fn))

liftPureFn :: (A.Response A.BodyReader -> Either Text a) -> ParseHead a
liftPureFn fn =
  liftIOFn (pure . fn)

liftTotalFn :: (A.Response A.BodyReader -> a) -> ParseHead a
liftTotalFn fn =
  liftPureFn (Right . fn)

getStatus :: ParseHead Int
getStatus =
  liftTotalFn (C.statusCode . A.responseStatus)

parseHeaders :: E.ParseHeaders headers -> ParseHead headers
parseHeaders (E.ParseHeaders (ReaderT hashMapFn)) =
  liftPureFn (runExcept . hashMapFn . D.fromList . map (first B.foldedCase) . A.responseHeaders)
