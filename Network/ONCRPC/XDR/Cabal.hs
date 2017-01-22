-- |Cabal utilities for XDR processing.
module Network.ONCRPC.XDR.Cabal
  ( ppRPCGenSuffixHandler
  ) where

import           Data.List (intercalate)
import           Distribution.PackageDescription (BuildInfo)
import           Distribution.Verbosity (Verbosity)
import           Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import           Distribution.Simple.PreProcess (PreProcessor(..), PPSuffixHandler)
import           Distribution.Simple.Utils (info)
import           System.FilePath ((</>), dropExtension, splitDirectories)

import Network.ONCRPC.XDR.Generate

runRPCGen :: (FilePath, FilePath) -> (FilePath, FilePath) -> Verbosity -> IO ()
runRPCGen (indir, infile) (outdir, outfile) verb = do
  info verb $ "hdrpcgen -m " ++ modname ++ " " ++ inpath
  writeFile outpath
    =<< generateFromFile (GenerateOptions modname defaultReidentOptions) inpath
  where
  inpath = indir </> infile
  outpath = outdir </> outfile
  modname = intercalate "." $ splitDirectories $ dropExtension infile

ppRPCGen :: BuildInfo -> LocalBuildInfo -> PreProcessor
ppRPCGen _ _ = PreProcessor
  { platformIndependent = True
  , runPreProcessor = runRPCGen
  }

-- |Pre-processor for hsrpcgen.
-- You can use it by setting @'UserHooks' { 'hookedPrepProcessors' = ['ppRPCGenSuffixHandler'] }@.
-- Note that this will override the default alex @.x@ file handler.
ppRPCGenSuffixHandler :: PPSuffixHandler
ppRPCGenSuffixHandler = ("x", ppRPCGen)
