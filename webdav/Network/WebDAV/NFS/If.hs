{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.WebDAV.NFS.If
  ( checkIfHeaders
  ) where

import           Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Char8 as APC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import           Network.URI (URI, parseURIReference)
import           Waimwork.HTTP (parseHTTPDate, ETag, etagParser, parseETags, matchETag)

import           Network.WebDAV.NFS.Types
import           Network.WebDAV.NFS.Request

type IfHeader = [IfList]
data IfList = IfList
  { _ifResourceTag :: Maybe URI
  , _ifConditions :: [IfCondition]
  }
data IfCondition
  = IfStateToken{ _ifNot :: Bool, _ifStateToken :: URI }
  | IfEntityTag { _ifNot :: Bool, _ifEntityTag :: ETag }

parseURI :: AP.Parser URI
parseURI = maybe (fail "invalid URI") return . parseURIReference . BSC.unpack
  =<< APC.char '<' *> AP.takeWhile (AP.inClass "!#-;=?-~") <* APC.char '>'

parseIfCondition :: AP.Parser IfCondition
parseIfCondition = do
  neg <- AP.option False (True <$ AP.string "Not")
  APC.skipSpace
  (IfStateToken neg <$> parseURI
    <|> IfEntityTag neg <$> (APC.char '[' *> etagParser <* APC.char ']'))
    <* APC.skipSpace

parseIfList :: AP.Parser [IfCondition]
parseIfList = APC.char '(' *> APC.skipSpace *> AP.many1 parseIfCondition <* APC.char ')' <* APC.skipSpace

parseTaggedList :: Maybe URI -> AP.Parser [IfList]
parseTaggedList tag = do
  tag' <- AP.option tag $ Just <$> parseURI
  APC.skipSpace
  (:) . IfList tag' <$> parseIfList <*> AP.option [] (parseTaggedList tag')

parseIfHeader :: AP.Parser IfHeader
parseIfHeader = APC.skipSpace *> parseTaggedList Nothing <* AP.endOfInput

parseIf :: BS.ByteString -> Either String IfHeader
parseIf = AP.parseOnly parseIfHeader

checkIfHeaders :: Context -> Maybe HTTP.Status
checkIfHeaders ctx@Context{ contextFile = FileInfo{..} }
  | not (isnomat || ismod) = Just HTTP.notModified304
  | not (ismat || isnomod) = Just HTTP.preconditionFailed412
  | otherwise = Nothing
  where
  ismat   = all (matchETag fileETag) ifmat
  isnomat = all (not . matchETag fileETag) ifnomat
  ismod   = all (fileMTime >) ifmod
  isnomod = all (fileMTime <=) ifnomod
  ifmat   = parseETags    <$> header HTTP.hIfMatch
  ifnomat = parseETags    <$> header HTTP.hIfNoneMatch
  ifmod   = parseHTTPDate =<< header HTTP.hIfModifiedSince
  ifnomod = parseHTTPDate =<< header HTTP.hIfUnmodifiedSince
  _ifhead  = either (const Nothing) Just . parseIf =<< header "If"
  header = requestHeader ctx
