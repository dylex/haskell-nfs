{-# LANGUAGE TupleSections #-}
module Network.WebDAV.NFS.Property
  ( PropertySet
  , standardProperties
  , propFindSet
  , fileInfoProperty
  ) where

import qualified Data.Set as Set
import qualified Network.NFS.V4 as NFS

import           Network.WebDAV.DAV
import           Network.WebDAV.NFS.Types

type PropertySet = Set.Set PropertyType

standardProperties :: PropertySet
standardProperties = Set.fromList
  [ CreationDate     Proxy
  , GetContentLength Proxy
  , GetETag          Proxy
  , GetLastModified  Proxy
  , ResourceType     Proxy
  ]

propFindSet :: PropFind -> Maybe PropertySet
propFindSet PropName = Nothing
propFindSet (PropFind a l) = Just $
  (if a then Set.union standardProperties else id)
  $ Set.fromList l

($.) :: PropertyContent c => (c a -> Property c) -> (FileInfo -> a) -> FileInfo -> Maybe (Property c)
c $. f = Just . c . return . f
infix 1 $.

fileInfoProperty :: PropertyContent c => Property t -> FileInfo -> Maybe (Property c)
fileInfoProperty CreationDate{} = fmap (CreationDate . return) . fileCTime
fileInfoProperty GetContentLength{} = GetContentLength $. fileSize
fileInfoProperty GetETag{} = GetETag $. fileETag
fileInfoProperty GetLastModified{} = GetLastModified $. fileMTime
fileInfoProperty ResourceType{} = ResourceType $. (, []) . (Just NFS.NF4DIR ==) . fileType
fileInfoProperty _ = const Nothing
