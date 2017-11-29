module Belka.Ptr.Parse
where

import Belka.Prelude hiding (fail)
import Ptr.Parse
import qualified Belka.BytePredicates as A
import qualified Belka.MonadPlus as B
import qualified Data.HashMap.Strict as C


contentTypeHeader :: Parse (ByteString, HashMap ByteString ByteString)
contentTypeHeader =
  do
    mimeType <- bytesWhile A.mimeType
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

token :: Parse ByteString
token =
  bytesWhile A.token

equality :: Parse ()
equality =
  do
    byte <- word8
    if byte == 61
      then return ()
      else fail "Not an equality sign"

semicolon :: Parse ()
semicolon =
  do
    byte <- word8
    if byte == 59
      then return ()
      else fail "Not a semicolon"
