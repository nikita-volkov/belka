module Belka.TransportError
where

import Belka.Prelude
import qualified Network.CURL730 as A


data TransportError =
  TimeoutTransportError |
  ConnectionTransportError IOErrorType |
  UnclassifiedTransportError A.CURLE
  deriving (Show)

someException :: TransportError -> SomeException -> TransportError
someException alternative someException =
  case fromException someException of
    Just ioException -> ConnectionTransportError (ioeGetErrorType ioException)
    Nothing -> alternative

httpException :: A.HttpException -> TransportError
httpException httpException =
  case httpException of
    A.HttpExceptionRequest _ content ->
      case content of
        A.ResponseTimeout ->
          TimeoutTransportError
        A.ConnectionTimeout ->
          TimeoutTransportError
        A.ConnectionFailure x ->
          someException (UnclassifiedTransportError httpException) x
        A.InternalException x ->
          someException (UnclassifiedTransportError httpException) x
        _ ->
          UnclassifiedTransportError httpException
    _ ->
      UnclassifiedTransportError httpException
