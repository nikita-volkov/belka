module Belka.TransportError
where

import Belka.Prelude
import qualified Network.HTTP.Client as A


data TransportError =
  TimeoutTransportError |
  ConnectionError IOErrorType |
  UnclassifiedError A.HttpException
  deriving (Show)

someException :: TransportError -> SomeException -> TransportError
someException alternative someException =
  case fromException someException of
    Just ioException -> ConnectionError (ioeGetErrorType ioException)
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
          someException (UnclassifiedError httpException) x
        A.InternalException x ->
          someException (UnclassifiedError httpException) x
        _ ->
          UnclassifiedError httpException
    _ ->
      UnclassifiedError httpException
