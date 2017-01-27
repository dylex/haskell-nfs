-- |Exceptions for RPC calls

{-# LANGUAGE ExistentialQuantification #-}
module Network.ONCRPC.Exception
  ( RPCException(..)
  , rpcExceptionToException
  , rpcExceptionFromException
  ) where

import           Control.Exception (Exception(..), SomeException)
import           Data.Typeable (Typeable, cast)

data RPCException = forall e . Exception e => RPCException e
  deriving (Typeable)

instance Show RPCException where
  showsPrec p (RPCException e) = showsPrec p e

instance Exception RPCException

rpcExceptionToException :: Exception e => e -> SomeException
rpcExceptionToException = toException . RPCException

rpcExceptionFromException :: Exception e => SomeException -> Maybe e
rpcExceptionFromException x = do
  RPCException a <- fromException x
  cast a
