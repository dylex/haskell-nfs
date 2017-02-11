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
import           Waimwork.HTTP (parseHTTPDate, ETag, etagParser, parseETags, matchETag, strongETagEq)

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

parseIf :: BS.ByteString -> Either String IfHeader
parseIf = AP.parseOnly parseIfHeader where

  parseIfHeader :: AP.Parser IfHeader
  parseIfHeader = APC.skipSpace *> parseTaggedList Nothing <* AP.endOfInput

  parseTaggedList :: Maybe URI -> AP.Parser [IfList]
  parseTaggedList tag = do
    tag' <- AP.option tag $ Just <$> parseURI
    APC.skipSpace
    (:) . IfList tag' <$> parseIfList <*> AP.option [] (parseTaggedList tag')

  parseIfList :: AP.Parser [IfCondition]
  parseIfList = APC.char '(' *> APC.skipSpace *> AP.many1 parseIfCondition <* APC.char ')' <* APC.skipSpace

  parseIfCondition :: AP.Parser IfCondition
  parseIfCondition = do
    neg <- AP.option False (True <$ AP.string "Not")
    APC.skipSpace
    (IfStateToken neg <$> parseURI
      <|> IfEntityTag neg <$> (APC.char '[' *> etagParser <* APC.char ']'))
      <* APC.skipSpace

testIf :: Context -> IfHeader -> Bool
testIf Context{ contextFile = FileInfo{..} } = any testIfList where

  testIfList (IfList (Just _) _) = False -- TODO: other resource
  testIfList (IfList Nothing l) = all testIfCondition l

  testIfCondition (IfStateToken n _) = n -- TODO: token
  testIfCondition (IfEntityTag n e) = n /= (e == fileETag)

checkIfHeaders :: Context -> Maybe HTTP.Status
checkIfHeaders ctx@Context{ contextFile = FileInfo{..} }
  | not (isnomat || ismod) = Just HTTP.notModified304
  | not (ismat || isnomod) = Just HTTP.preconditionFailed412
  | not ishead             = Just HTTP.preconditionFailed412
  | otherwise = Nothing
  where
  ismat   = all (matchETag (strongETagEq fileETag)) ifmat
  isnomat = all (not . matchETag (strongETagEq fileETag)) ifnomat
  ismod   = all (fileMTime >) ifmod
  isnomod = all (fileMTime <=) ifnomod
  ishead  = all (testIf ctx) ifhead
  ifmat   = parseETags    <$> header HTTP.hIfMatch
  ifnomat = parseETags    <$> header HTTP.hIfNoneMatch
  ifmod   = parseHTTPDate =<< header HTTP.hIfModifiedSince
  ifnomod = parseHTTPDate =<< header HTTP.hIfUnmodifiedSince
  ifhead  = either (const Nothing) Just . parseIf =<< header "If"
  header = requestHeader ctx
