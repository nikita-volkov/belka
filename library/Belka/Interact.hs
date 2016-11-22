module Belka.Interact
where

import Belka.Prelude
import qualified Network.HTTP.Client as A
import qualified Belka.Request as B
import qualified Belka.ParseHead as C
import qualified Belka.ParseBody as D
import qualified Potoki.IO as E
import qualified Data.ByteString as F


newtype Interact a =
  Interact (ExceptT A.HttpException (ExceptT Text (ReaderT A.Manager IO)) a)
  deriving (Functor, Applicative, Monad, MonadIO)

request :: B.Request -> C.ParseHead (D.ParseBody body) -> Interact body
request (B.Request (Endo httpRequest)) (C.ParseHead (ExceptT (ReaderT parseResponseHeadIO))) =
  Interact $ ExceptT $ ExceptT $ ReaderT $ \ manager ->
  handle (\ e -> return (Right (Left e))) $
  A.withResponse (httpRequest A.defaultRequest) manager $ \ response ->
  do
    parsedHead <- parseResponseHeadIO response
    case parsedHead of
      Left parsingError -> return (Left parsingError)
      Right (D.ParseBody (Compose consumeBody)) ->
        E.consume
          (let fetchChunk = A.responseBody response
            in \ stop emit -> do
              chunk <- fetchChunk
              if F.null chunk
                then stop
                else emit chunk)
          (fmap (either Left (Right . Right)) consumeBody)
