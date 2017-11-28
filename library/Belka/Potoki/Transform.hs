module Belka.Potoki.Transform
where

import Belka.Prelude
import Potoki.Transform
import qualified Network.HTTP.Client as A
import qualified Network.HTTP.Client.TLS as A
import qualified Belka.Request as B
import qualified Belka.ParseHead as C
import qualified Belka.ParseBody as D
import qualified Belka.TransportError as G
import qualified Belka.IO as H
import qualified Belka.Interact as I


{-|
Given an HTTP manager, a specification of how to parse the response,
execute the requests, producing the parsed responses.
-}
request :: A.Manager -> C.ParseHead (D.ParseBody response) -> Transform B.Request (Either Text (Either G.TransportError response))
request manager parse =
  arr (\ request -> H.interact manager (I.request request parse)) >>>
  executeIO

{-|
Perform 'request' using a new manager.
-}
requestUsingNewManager :: C.ParseHead (D.ParseBody response) -> Transform B.Request (Either Text (Either G.TransportError response))
requestUsingNewManager parse =
  ioTransform $ do
    manager <- A.newTlsManager
    return (request manager parse)

{-|
Perform 'request' using a global manager.
-}
requestUsingGlobalManager :: C.ParseHead (D.ParseBody response) -> Transform B.Request (Either Text (Either G.TransportError response))
requestUsingGlobalManager parse =
  ioTransform $ do
    manager <- A.getGlobalManager
    return (request manager parse)
