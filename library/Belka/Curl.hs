module Belka.Curl
where

import Belka.Prelude
import qualified Network.CURL730 as A
import qualified Data.Pool as B


-- {-# NOINLINE pool #-}
-- pool :: B.Pool A.CURL
-- pool =
--   unsafePerformIO $ do
--     B.createPool A.curl_easy_init A.curl_easy_cleanup 10 7 100
