module Network.NFS.V4.State
  ( anonymousStateid
  , bypassStateid
  , currentStateid
  , invalidStateid 
  ) where

import           Network.ONCRPC.XDR.Array (padLengthArray)

import qualified Network.NFS.V4.Prot as NFS

anonymousStateid, bypassStateid, currentStateid, invalidStateid :: NFS.Stateid4
anonymousStateid = NFS.Stateid4 0        (padLengthArray mempty 0)
bypassStateid    = NFS.Stateid4 maxBound (padLengthArray mempty maxBound)
currentStateid   = NFS.Stateid4 1        (padLengthArray mempty 0)
invalidStateid   = NFS.Stateid4 maxBound (padLengthArray mempty 0)
