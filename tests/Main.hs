module Main
where

import Prelude hiding (assert, choose, (.&.), Success, Failure, Fixed)
import Bug
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances
import qualified Belka.IO as A
import qualified Belka.Interact as B
import qualified Belka.Request as C
import qualified Belka.ParseBody as D
import qualified Belka.Potoki.Transform as I
import qualified Iri.QuasiQuoter as E
import qualified Potoki.Produce as F
import qualified Potoki.IO as G
import qualified Potoki.Consume as H


main =
  defaultMain $
  testGroup "All tests" $
  [
    scrape
    ,
    erroneusResponse
  ]

erroneusResponse =
  testCase "Erroneous response" $ do
    result <-
      fmap
        (either Left (either (Left . fromString . show) Right))
        (A.interactUsingGlobalManager
          (B.request
            (C.setIri [E.httpUri|http://localhost:993|])
            (pure (fmap (const ()) D.json))))
    assertBool "" (isLeft result)

scrape =
  testCase "Scrape" $ do
    result <- G.produceAndConsume produce consume
    let (Right (Right bodies)) = fmap sequence (sequence result)
    assertEqual "" [""] bodies
  where
    produce =
      F.list [request]
      where
        request =
          C.setIri [E.httpUri|https://best-trailer.ru/player/html5/zrollnotgpmd.php|] <>
          C.setAcceptHeaderToHtml <>
          C.setUserAgentHeader "Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 YaBrowser/17.9.1.785 (beta) Yowser/2.5 Safari/537.36" <>
          C.setAcceptLanguageHeader ""
    consume =
      H.transform transform H.list
      where
        transform =
          I.requestUsingNewManager (pure (D.consume consume))
          where
            consume =
              fmap Right H.concat
