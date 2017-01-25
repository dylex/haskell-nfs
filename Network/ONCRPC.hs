-- | ONC (Sun) RPC: Remote Procedure Call Protocol Version 2 as described in RFC5531
--
-- This module should be imported qualified, e.g., as @RPC@.

module Network.ONCRPC
  ( module Network.ONCRPC.XDR
  , module Network.ONCRPC.Auth
  , module Network.ONCRPC.Message
  , module Network.ONCRPC.Exception
  , module Network.ONCRPC.Client
  ) where

import Network.ONCRPC.XDR
import Network.ONCRPC.Auth hiding (opacifyAuth, unopacifyAuth)
import Network.ONCRPC.Message (Call(..), Reply(..), ReplyException)
import Network.ONCRPC.Exception (RPCException)
import Network.ONCRPC.Client
