module Paths_forest (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,0,2], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/hpacheco/.cabal/bin"
libdir     = "/Users/hpacheco/.cabal/lib/x86_64-osx-ghc-7.6.3/forest-1.0.2"
datadir    = "/Users/hpacheco/.cabal/share/x86_64-osx-ghc-7.6.3/forest-1.0.2"
libexecdir = "/Users/hpacheco/.cabal/libexec"
sysconfdir = "/Users/hpacheco/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "forest_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "forest_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "forest_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "forest_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "forest_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
