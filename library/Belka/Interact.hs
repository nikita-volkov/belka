module Belka.Interact
where

import Belka.Prelude
import qualified Network.CURL730 as A
import qualified Belka.Request as B
import qualified Belka.ParseHead as C
import qualified Belka.ParseBody as D
-- import qualified Belka.TransportError as G
import qualified Potoki.IO as E
import qualified Data.ByteString as F
import qualified Data.Pool as P



-- newtype Interact a =
--   Interact (ExceptT G.TransportError (ExceptT Text (ReaderT A.Manager IO)) a)
--   deriving (Functor, Applicative, Monad, MonadIO)

-- -- request :: B.Request -> C.ParseHead (D.ParseBody response) -> Interact response
-- -- request (B.Request requestIO) (C.ParseHead (ExceptT (ReaderT parseResponseHeadIO))) =
-- --   Interact $ ExceptT $ ExceptT $ ReaderT $ \ manager ->
-- --   handle (return . Right . Left . G.httpException) $
-- --   do
-- --     (hcRequest, requestCleanUp) <- requestIO A.defaultRequest
-- --     result <-
-- --       A.withResponse hcRequest manager $ \ response -> do
-- --         parsedHead <- parseResponseHeadIO response
-- --         case parsedHead of
-- --           Left parsingError -> return (Left parsingError)
-- --           Right (D.ParseBody (Compose consumeBody)) ->
-- --             E.consume
-- --               (let fetchChunk = A.responseBody response
-- --                 in \ end element -> do
-- --                   chunk <- fetchChunk
-- --                   return $ if F.null chunk
-- --                     then end
-- --                     else element chunk)
-- --               (fmap (either (Left . mappend "Body parsing: ") (Right . Right)) consumeBody)
-- --     requestCleanUp
-- --     return result

newtype Interact a =
  Interact (ExceptT Text (ReaderT A.CURL IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

request :: B.Request -> C.ParseHead (D.ParseBody response) -> Interact response
request (B.Request pureOptions requestBodyIO) (C.ParseHead (ExceptT (ReaderT parseResponseHeadIO))) =
  do
    activeVar <- newTVarIO True
    chan <- newEmptyTMVarIO
    
  where

