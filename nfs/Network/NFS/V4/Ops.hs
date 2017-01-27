{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Network.NFS.V4.Ops
  ( Op(..)
  , GETFH4args(..)
  , LOOKUPP4args(..)
  , PUTPUBFH4args(..)
  , PUTROOTFH4args(..)
  , READLINK4args(..)
  , RESTOREFH4args(..)
  , SAVEFH4args(..)
  , ILLEGAL4args(..)
  , SECINFO_NO_NAME4res(..)

  , Ops
  , opArgs
  , opHandler
  , op
  , op_
  , (>*<)
  ) where

import           Control.Applicative (liftA2)
import           Control.Exception (throw)
import           Data.Functor (void)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Vector as V
import qualified Network.ONCRPC as RPC

import qualified Network.NFS.V4.Prot as NFS
import           Network.NFS.V4.Exception
import           Network.NFS.V4.Ops.TH

class (RPC.XDR a, RPC.XDR r) => Op a r | r -> a, a -> r where
  opNum :: a -> NFS.Nfs_opnum4
  toOpArg :: a -> NFS.Nfs_argop4
  fromOpRes :: NFS.Nfs_resop4 -> Maybe r

thOps
  -- void args:
  [ NFS.OP_GETFH
  , NFS.OP_LOOKUPP
  , NFS.OP_PUTPUBFH
  , NFS.OP_PUTROOTFH
  , NFS.OP_READLINK
  , NFS.OP_RESTOREFH
  , NFS.OP_SAVEFH
  , NFS.OP_ILLEGAL
  ]
  -- exclude:
  [ NFS.OP_SECINFO_NO_NAME
  ]

newtype SECINFO_NO_NAME4res = SECINFO_NO_NAME4res{ sECINFO_NO_NAME4res :: NFS.SECINFO4res }
instance RPC.XDR SECINFO_NO_NAME4res where
  xdrType _ = "SECINFO_NO_NAME4res"
  xdrPut (SECINFO_NO_NAME4res s) = RPC.xdrPut s
  xdrGet = SECINFO_NO_NAME4res <$> RPC.xdrGet

instance Op NFS.Secinfo_style4 SECINFO_NO_NAME4res where
  opNum _ = NFS.OP_SECINFO_NO_NAME
  toOpArg = NFS.Nfs_argop4'OP_SECINFO_NO_NAME
  fromOpRes (NFS.Nfs_resop4'OP_SECINFO_NO_NAME r) = Just $ SECINFO_NO_NAME4res r
  fromOpRes _ = Nothing


-- |A set of compound operations and their handler.
-- These can be created with 'Op' and combined using the 'Applicative' instance.
-- Since you often want to fold results left-to-right, 'Control.Applicative.<**>' can be helpful.
data Ops a = Ops
  { opArgs :: V.Vector NFS.Nfs_argop4
  , opHandler :: V.Vector NFS.Nfs_resop4 -> a
  }

instance Functor Ops where
  fmap f (Ops a h) = Ops a (f . h)

-- |Operations are performed in order, left-to-right.
instance Applicative Ops where
  pure x = Ops V.empty (const x)
  Ops fa fh <*> Ops xa xh = Ops (fa <> xa) $ \r ->
    fh (V.take fn r) $ xh (V.drop fn r)
    where fn = V.length fa

-- |A single operation
op :: Op a r => a -> Ops r
op a = Ops (V.singleton $ toOpArg a)
  $ fromMaybe (throw $ NFSException (Just $ opNum a) Nothing) . fromOpRes . V.head

-- |A single operation, ignoring the result value: @'void' . 'op'@
op_ :: Op a r => a -> Ops ()
op_ = void . op

-- |The monoidal pair operator, for convenience: @'liftA2' '(,)'@
(>*<) :: Applicative f => f a -> f b -> f (a, b)
(>*<) = liftA2 (,)
infixl 4 >*<
