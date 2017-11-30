module Belka.BytePredicates
where

import Belka.Prelude hiding ((|||), (&&&), inRange, Predicate)
import qualified Data.Vector as A
import qualified Data.Char as B


type Predicate =
  Word8 -> Bool

{-# NOINLINE cached #-}
cached :: Predicate -> Predicate
cached predicate =
  case A.generate 256 (predicate . fromIntegral) of
    vector -> A.unsafeIndex vector . fromIntegral

{-# NOINLINE cachedAscii #-}
cachedAscii :: Predicate -> Predicate
cachedAscii predicate =
  case A.generate 128 (predicate . fromIntegral) of
    vector -> A.unsafeIndex vector . fromIntegral

oneOfChars :: [Char] -> Predicate
oneOfChars string i =
  elem i (fmap (fromIntegral . ord) string)

infixr 2 |||
(|||) :: Predicate -> Predicate -> Predicate
(|||) left right i =
  left i || right i

infixr 3 &&&
(&&&) :: Predicate -> Predicate -> Predicate
(&&&) left right i =
  left i && right i

inRange :: Word8 -> Word8 -> Predicate
inRange min max i =
  i >= min && i <= max

inCharRange :: Char -> Char -> Predicate
inCharRange min max =
  inRange (fromIntegral (ord min)) (fromIntegral (ord max))

charPredicate :: (Char -> Bool) -> Predicate
charPredicate p =
  p . chr . fromIntegral

{-| 7-bit -}
septimal :: Predicate
septimal i =
  i < 0x80

nonSeptimal :: Predicate
nonSeptimal i =
  i >= 0x80

asciiAlphanumeric :: Predicate
asciiAlphanumeric =
  inCharRange 'a' 'z' |||
  inCharRange 'A' 'Z' |||
  inCharRange '0' '9'

mimeType :: Predicate
mimeType =
  cachedAscii $
  asciiAlphanumeric ||| oneOfChars "-/+*"

space :: Predicate
space =
  charPredicate B.isSpace

semicolon :: Predicate
semicolon =
  charPredicate (== ';')

token :: Predicate
token =
  mimeType

quotedTokenUnescaped :: Predicate
quotedTokenUnescaped =
  mimeType
