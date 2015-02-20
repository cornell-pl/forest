module Paths_ctrie (
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
version = Version {versionBranch = [0,1,0,3], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/hpacheco/Library/Haskell/bin"
libdir     = "/Users/hpacheco/Library/Haskell/ghc-7.8.3-x86_64/lib/ctrie-0.1.0.3"
datadir    = "/Users/hpacheco/Library/Haskell/share/ghc-7.8.3-x86_64/ctrie-0.1.0.3"
libexecdir = "/Users/hpacheco/Library/Haskell/libexec"
sysconfdir = "/Users/hpacheco/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ctrie_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ctrie_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ctrie_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ctrie_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ctrie_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
