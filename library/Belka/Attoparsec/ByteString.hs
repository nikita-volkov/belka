module Belka.Attoparsec.ByteString
where

import Belka.Prelude hiding (fail)
import Data.Attoparsec.ByteString
import qualified Belka.BytePredicates as A
import qualified Belka.MonadPlus as B
import qualified Data.HashMap.Strict as C


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
        value <- token
        return (attribute, value)

token :: Parser ByteString
token =
  lowerCaseBytesInIso8859_1 <$> takeWhile1 A.token

equality :: Parser Word8
equality =
  word8 61

semicolon :: Parser Word8
semicolon =
  word8 59
