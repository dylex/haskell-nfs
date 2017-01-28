-- | XDR: External Data Representation as described in RFC4506
--
-- This module should be imported qualified, e.g., as @XDR@.

module Network.ONCRPC.XDR
  ( module Network.ONCRPC.XDR.Types
  , module Network.ONCRPC.XDR.Array
  , module Network.ONCRPC.XDR.Serial
  ) where

import Network.ONCRPC.XDR.Types
import Network.ONCRPC.XDR.Array
import Network.ONCRPC.XDR.Serial
