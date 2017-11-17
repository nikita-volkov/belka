module Main
where

import Prelude hiding (assert, choose, (.&.), Success, Failure, Fixed)
import Bug
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Instances
import qualified Belka.IO as A
import qualified Belka.Interact as B
import qualified Belka.Request as C
import qualified Belka.ParseBody as D
import qualified Iri.Parsing.ByteString as E


main =
  defaultMain $
  testGroup "All tests" $
  [
    testCase "Erroneous response" $ do
      result <-
        fmap
          (either Left (either (Left . fromString . show) Right))
          (A.interactUsingGlobalManager
            (B.request
              (C.setIri (either ($bug "") id (E.url "http://user:password@localhost:993")))
              (pure (fmap (const ()) D.json))))
      assertBool "" (isLeft result)
  ]
