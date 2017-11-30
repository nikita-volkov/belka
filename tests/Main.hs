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
import qualified Belka.ParseHead as J
import qualified Belka.ParseHeaders as K
import qualified Belka.Potoki.Transform as I
import qualified Iri.QuasiQuoter as E
import qualified Iri.Parsing.Text as M
import qualified Potoki.Produce as F
import qualified Potoki.IO as G
import qualified Potoki.Consume as H
import qualified Potoki.Transform as N
import qualified Data.Text as L


main =
  defaultMain $
  testGroup "All tests" $
  [
    bombingWithRequests
    ,
    scrape
    ,
    erroneusResponse
    ,
    charset
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
    assertBool "" ((not . null) bodies)
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

charset =
  testCase "Charset" $ do
    result <- G.produceAndConsume produce consume
    let (Right (Right bodies)) = fmap sequence (sequence result)
    assertEqual "" ["utf-8"] bodies
  where
    produce =
      F.list [request]
      where
        request =
          C.setIri [E.httpUri|http://google.com|] <>
          C.setAcceptHeaderToHtml <>
          C.setUserAgentHeader "Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 YaBrowser/17.9.1.785 (beta) Yowser/2.5 Safari/537.36" <>
          C.setAcceptLanguageHeader ""
    consume =
      H.transform transform H.list
      where
        transform =
          I.requestUsingNewManager (J.parseHeaders K.charset >>= return . pure)

bombingWithRequests =
  testCase "Bombing with requests" $ do
    results <- run
    assertBool (show results) (isRight results)
  where
    getIris =
      do
        result <- traverse M.httpIri . L.split (== '\n') <$> L.readFile "samples/iris"
        either (fail . show) return result
    getRequests =
      do
        iris <- getIris
        return $ do
          iri <- iris
          return $ C.setIri iri
    run =
      do
        requests <- getRequests
        results <- G.produceAndConsume (F.list requests) (H.transform transform H.list)
        either (fail . showString "Parsing error: " . show) (return . sequence) (sequence results)
      where
        transform =
          N.concurrently 30 $
          I.requestUsingNewManager (J.parseHeaders K.contentType >>= return . pure)
