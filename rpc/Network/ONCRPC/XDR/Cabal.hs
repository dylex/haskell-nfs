-- |Cabal utilities for XDR processing.
{-# LANGUAGE CPP #-}
module Network.ONCRPC.XDR.Cabal
  ( ppRPCGenSuffixHandler
  ) where

import           Data.Char (toLower)
import           Data.List (intercalate, isPrefixOf)
import           Data.Maybe (fromMaybe, mapMaybe)
import           Distribution.PackageDescription (BuildInfo(customFieldsBI))
import           Distribution.Verbosity (Verbosity)
import           Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import           Distribution.Simple.PreProcess (PreProcessor(..), PPSuffixHandler)
import           Distribution.Simple.Utils (info)
#if MIN_VERSION_Cabal(2,0,0)
import           Distribution.Types.ComponentLocalBuildInfo (ComponentLocalBuildInfo)
#endif
import           System.FilePath ((</>), dropExtension, splitDirectories)

import           Network.ONCRPC.XDR.Generate

runRPCGen :: [(String, String)] -> (FilePath, FilePath) -> (FilePath, FilePath) -> Verbosity -> IO ()
runRPCGen custom (indir, infile) (outdir, outfile) verb = do
  info verb $ "hdrpcgen " ++ inpath ++ " with " ++ show opts
  writeFile outpath
    =<< generateFromFile opts inpath
  where
  opts = GenerateOptions
    { generateModuleName = modname
    , generateReidentOptions = ReidentOptions
      { reidentUpperPrefix = fromMaybe "" $ opt "upper-prefix"
      , reidentLowerPrefix = fromMaybe "" $ opt "lower-prefix"
      , reidentJoinField = joinopt "field"
      , reidentJoinProcedure = joinopt "procedure"
      }
    }
  joinopt t = case (maybe False boolish $ opt $ t ++ "s-unique", opt $ "join-" ++ t) of
    (False, j) -> Just $ fromMaybe "'" j
    (True, Nothing) -> Nothing
    (True, Just _) -> error $ "x-rpcgen join and unique options are mutually exclusive"
  boolish s = map toLower s `isPrefixOf` "true"
  opt f = lookup f custom
  inpath = indir </> infile
  outpath = outdir </> outfile
  modname = intercalate "." $ splitDirectories $ dropExtension infile

ppRPCGenCustomField :: (String, String) -> Maybe (String, String)
ppRPCGenCustomField ('x':'-':'r':'p':'c':'g':'e':'n':'-':f,v) = Just (f,v)
ppRPCGenCustomField _ = Nothing

ppRPCGen :: BuildInfo -> LocalBuildInfo
#if MIN_VERSION_Cabal(2,0,0)
  -> ComponentLocalBuildInfo
#endif
  -> PreProcessor
ppRPCGen bi _ 
#if MIN_VERSION_Cabal(2,0,0)
  _
#endif
  = PreProcessor
  { platformIndependent = True
  , runPreProcessor = runRPCGen $ mapMaybe ppRPCGenCustomField $ customFieldsBI bi
  }

-- |Pre-processor for hsrpcgen.
-- You can use it by setting @'Distributin.Simple.UserHooks' { 'Distributin.Simple.hookedPrepProcessors' = ['ppRPCGenSuffixHandler'] }@.
-- Note that this will override the default alex @.x@ file handler.
-- You can also specify custom cabal fields corresponding to 'ReidentOptions' and command-line flags prefixed with @x-rpcgen-@: @{upper,lower}-prefix@, @join-{field,procedure}@, and @{field,procedure}s-unique}@.
ppRPCGenSuffixHandler :: PPSuffixHandler
ppRPCGenSuffixHandler = ("x", ppRPCGen)
