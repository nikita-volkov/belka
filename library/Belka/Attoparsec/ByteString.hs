{-|
See
<https://tools.ietf.org/html/rfc2046 MIME RFC>.
-}
module Belka.Attoparsec.ByteString
where

import Belka.Prelude hiding (fail)
import Data.Attoparsec.ByteString
import qualified Belka.BytePredicates as A
import qualified Belka.MonadPlus as B
import qualified Data.HashMap.Strict as C
import qualified Ptr.Poking as D
import qualified Ptr.ByteString as E


contentTypeHeader :: Parser (ByteString, HashMap ByteString ByteString)
contentTypeHeader =
  do
    mimeType <- takeWhile1 A.mimeType
    parameters <- B.foldl (\ table (k, v) -> C.insert k v table) mempty parameter
    return (mimeType, parameters)
  where
    parameter =
      do
        skipWhile A.space
        semicolon
        skipWhile A.space
        attribute <- token
        equality
        value <- token <|> quotedToken
        return (attribute, value)

quotedToken :: Parser ByteString
quotedToken =
  quote *> (lowerCaseBytesInIso8859_1 . E.poking <$> B.foldl mappend mempty segment) <* quote
  where
    segment =
      (D.bytes <$> takeWhile1 A.quotedTokenUnescaped) <|>
      (D.word8 34 <$ (backslash *> quote))

token :: Parser ByteString
token =
  lowerCaseBytesInIso8859_1 <$> takeWhile1 A.token

backslash :: Parser Word8
backslash =
  word8 92

quote :: Parser Word8
quote =
  word8 34

equality :: Parser Word8
equality =
  word8 61

semicolon :: Parser Word8
semicolon =
  word8 59
