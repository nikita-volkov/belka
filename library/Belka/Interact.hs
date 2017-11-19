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
  Interact (ExceptT IOException (ExceptT Text (ReaderT A.Manager IO)) a)
  deriving (Functor, Applicative, Monad, MonadIO)

request :: B.Request -> C.ParseHead (D.ParseBody body) -> Interact body
request (B.Request requestIO) (C.ParseHead (ExceptT (ReaderT parseResponseHeadIO))) =
  Interact $ ExceptT $ ExceptT $ ReaderT $ \ manager ->
  handle (return . httpExceptionMapping) $
  do
    (hcRequest, requestCleanUp) <- requestIO A.defaultRequest
    result <-
      A.withResponse hcRequest manager $ \ response -> do
        parsedHead <- parseResponseHeadIO response
        case parsedHead of
          Left parsingError -> return (Left parsingError)
          Right (D.ParseBody (Compose consumeBody)) ->
            E.consume
              (let fetchChunk = A.responseBody response
                in \ end element -> do
                  chunk <- fetchChunk
                  return $ if F.null chunk
                    then end
                    else element chunk)
              (fmap (either (Left . mappend "Body parsing: ") (Right . Right)) consumeBody)
    requestCleanUp
    return result
  where
    httpExceptionMapping =
      \ case
        A.HttpExceptionRequest _ (A.ConnectionFailure someException) | Just ioException <- fromException someException ->
          Right (Left ioException)
        A.HttpExceptionRequest _ (A.InternalException someException) | Just ioException <- fromException someException ->
          Right (Left ioException)
        A.HttpExceptionRequest _ contents ->
          Left (fromString (showString "HTTP exception: " (show contents)))
        A.InvalidUrlException url message ->
          Left (fromString (showString "Invalid URL: " (showString url (showString ": " message))))
