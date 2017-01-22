-- |Types for RPC protocol and procedure.

{-# LANGUAGE RecordWildCards #-}
module Network.ONCRPC.Types
  ( XID
  , ProgNum
  , VersNum
  , ProcNum
  , Procedure(..)
  ) where

import           Data.Word (Word32)

type XID = Word32
type ProgNum = Word32
type VersNum = Word32
type ProcNum = Word32

-- |Description of a specific procedure, parameterized by argument and result types.
data Procedure a r = Procedure
  { procedureProg :: !ProgNum
  , procedureVers :: !VersNum
  , procedureProc :: !ProcNum
  }
  deriving (Eq, Show)
