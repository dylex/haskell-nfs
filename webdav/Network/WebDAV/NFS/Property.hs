{-# LANGUAGE TupleSections #-}
module Network.WebDAV.NFS.Property
  ( standardProperties
  , propFindSet
  , fileInfoProperty
  , getProperty
  ) where

import qualified Data.Set as Set
import qualified Network.NFS.V4 as NFS

import           Network.WebDAV.DAV
import           Network.WebDAV.NFS.File

type PropertySet = Set.Set PropertyType

standardProperties :: PropertySet
standardProperties = Set.fromList
  [ propertyType CreationDate
  , propertyType GetContentLength
  , propertyType GetETag
  , propertyType GetLastModified
  , propertyType ResourceType
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
fileInfoProperty CreationDate{} = CreationDate $. fileCTime
fileInfoProperty GetContentLength{} = GetContentLength $. fileSize
fileInfoProperty GetETag{} = GetETag $. fileETag
fileInfoProperty GetLastModified{} = GetLastModified $. fileMTime
fileInfoProperty ResourceType{} = ResourceType $. (, []) . (==) NFS.NF4DIR . fileType
fileInfoProperty _ = const Nothing

getProperty :: PropertyContent c => Property t -> FileInfo -> Maybe (Property c)
getProperty = fileInfoProperty
