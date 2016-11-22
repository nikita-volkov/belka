module Belka.IO
where

import Belka.Prelude hiding (interact)
import qualified Network.HTTP.Client as A
import qualified Network.HTTP.Client.TLS as B
import qualified Belka.Interact as D
import qualified Potoki.IO as E


interact :: A.Manager -> D.Interact a -> IO (Either Text (Either A.HttpException a))
interact manager (D.Interact interactStack) =
  runReaderT (runExceptT (runExceptT interactStack)) manager

interactUsingNewManager :: D.Interact a -> IO (Either Text (Either A.HttpException a))
interactUsingNewManager interact_ =
  do
    manager <- B.newTlsManager
    interact manager interact_

interactUsingGlobalManager :: D.Interact a -> IO (Either Text (Either A.HttpException a))
interactUsingGlobalManager interact_ =
  do
    manager <- B.getGlobalManager
    interact manager interact_
