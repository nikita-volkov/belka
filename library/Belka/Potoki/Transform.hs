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
Given a specification of how to parse the response,
execute the requests, producing the parsed responses.
-}
request :: C.ParseHead (D.ParseBody response) -> Transform B.Request (Either Text (Either G.TransportError response))
request parse =
  arr (\ request -> A.newTlsManager >>= \ manager -> H.interact manager (I.request request parse) <* A.closeManager manager) >>>
  executeIO

{-|
Given an HTTP manager, a specification of how to parse the response,
execute the requests, producing the parsed responses.
-}
requestUsingManager :: A.Manager -> C.ParseHead (D.ParseBody response) -> Transform B.Request (Either Text (Either G.TransportError response))
requestUsingManager manager parse =
  arr (\ request -> H.interact manager (I.request request parse)) >>>
  executeIO

{-|
Perform 'requestUsingManager' using a new manager.
-}
requestUsingNewManager :: C.ParseHead (D.ParseBody response) -> Transform B.Request (Either Text (Either G.TransportError response))
requestUsingNewManager parse =
  ioTransform $ do
    manager <- A.newTlsManagerWith settings
    return (requestUsingManager manager parse)
  where
    settings =
      A.tlsManagerSettings

{-|
Perform 'requestUsingManager' using a global manager.
-}
requestUsingGlobalManager :: C.ParseHead (D.ParseBody response) -> Transform B.Request (Either Text (Either G.TransportError response))
requestUsingGlobalManager parse =
  ioTransform $ do
    manager <- A.getGlobalManager
    return (requestUsingManager manager parse)
