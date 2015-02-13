{-# LANGUAGE EmptyDataDecls, DoAndIfThenElse, OverlappingInstances, TypeOperators, FunctionalDependencies, UndecidableInstances, ConstraintKinds,ScopedTypeVariables, TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, DataKinds, TypeFamilies, Rank2Types, GADTs, ViewPatterns, DeriveDataTypeable #-}

-- A module for filesystem-specific operations

module Language.Forest.FS.FSRep where

import System.Mem.WeakKey
import Language.Forest.FS.FSDelta
import Control.Monad
import Data.Maybe
import Data.List.Split
import Data.Typeable
import System.Directory
import System.IO.Unsafe
import qualified Data.List as List
import Data.IORef
import System.FilePath.Posix
import System.Posix.IO
import System.IO
import Language.Forest.IO.Shell
import Language.Forest.IO.Utils
import Control.Monad.Lazy
import System.FilePath.Glob
import Language.Pads.Padsc hiding (numErrors)

import Language.Pads.MetaData hiding (numErrors)
import Language.Pads.CoreBaseTypes
import Language.Forest.Errors
import Data.Data hiding (gmapT)
import System.Posix.Types
import Data.DeriveTH
import Data.Derive.Data
import Foreign.C.Types
import System.FilePath.Canonical
import System.Posix.Files
import System.Posix.User
import System.Process
import System.Posix.Time
import Control.Exception as Exception
import Data.Unique
import Data.Int
import Data.Word
import Data.Time.Clock

import Language.Haskell.TH.Syntax hiding (Loc(..))

import Debug.Trace

debug str = trace str

---------------------------------------------------------------

{-# NOINLINE forestCfg #-}
forestCfg :: IORef (ForestCfg fs)
forestCfg = unsafePerformIO $ newIORef (error "no initial Forest cfg")

-- the kind of possible filesystem instantiations
data FS = PureFS | TxFS | LazyFS | NILFS | TxVarFS | TxICFS deriving Typeable
deriving instance Typeable PureFS
deriving instance Typeable TxFS
deriving instance Typeable LazyFS
deriving instance Typeable NILFS
deriving instance Typeable TxVarFS
deriving instance Typeable TxICFS

deriving instance Typeable ForestM

-- | Class that implements filesystem-specific operations
class (Typeable fs,MonadLazy (ForestM fs),Eq (FSTree fs),Show (FSTree fs)) => FSRep (fs :: FS) where
	
	-- | The forest monad
	data ForestM fs a :: *
	
	-- some configuration parameters, such as the home forest directory, etc
	data ForestCfg fs :: *
	
	-- | returns the forest directory used to store auxiliary data
	-- if undefined just consider this the tmp directory of your OS
	getForestDirectory :: ForestM fs FilePath
	getForestDirectory = forestIO getTempForestDirectory
	
	-- runs a Forest computation (includes the FS monitor, incremental computation, etc)
	runForest :: ForestCfg fs -> ForestM fs a -> IO a
	
	-- | The Forest monad has to support IO
	-- we don't want to make the Forest monad an instance of @MonadIO@ to prevent users from using it
	forestIO :: IO a -> ForestM fs a
	
	-- A snapshot of a filesystem, possibly at a given moment in time if the FS support versioning
	data FSTree fs :: *
	
	-- makes a fresh up-to-date snapshot of the filesystem
	latestTree :: ForestM fs (FSTree fs)
	
	-- for neat handling of compressed archives (i.e., without temporary filenames making their way to in-memory data representations) we use the AVFS virtual filesystem.
	-- So @fs@ instances that support compressed Forest specs should provide a virtualization @FSTree@ mode
	virtualTree :: FSTree fs -> ForestM fs (FSTree fs)
	
	-- registers the deletion of a path from the filesystem
	deletePath :: FilePath -> ForestM fs ()
	-- registers a write to the contents of a file in the filesystem; receives an on-disk location from which the content can be retrieved
	writeFile :: FilePath -> OnDisk -> ForestM fs ()
	-- registers the creation of a new empty directory in the filesystem
	writeDir :: FilePath -> ForestM fs ()
	-- registers a creation of a symbolic link in the filesystem
	writeLink :: FilePath -> FilePath -> ForestM fs ()
	-- registers a write to the metadata of a path in the filesystem; receives an on-disk location from which the metadata can be retrieved
	writePathMD :: FilePath -> OnDisk -> ForestM fs ()
	-- generates a fresh forest temporary path
	tempPath :: ForestM fs FilePath

	-- *for IC we need the following operations to be deterministic, i.e., it should give the same answer at any time for the same @FilePath@ and @FSTree@
	-- in a filesystem without versioning this may simply not be true, e.g., if a directory has been deleted as we check for its existance before and after.

	-- translates a tree filepath to an on-disk filepath. this is used before any read from the FS
	pathInTree :: FilePath -> FSTree fs -> ForestM fs OnDisk
	-- translates an on-disk filepath to a tree filepath
	-- optional: if the FS does not support this functionality, this can be undefined provided a specialized instance for @canonalizePathWithTree@
	pathFromTree :: OnDisk -> FSTree fs -> ForestM fs FilePath
	-- appends a relative path to an original tree path
	stepPathInTree :: FSTree fs -> FilePath -> FilePath -> ForestM fs FilePath
	
	getDirectoryContentsInTree :: FilePath -> FSTree fs -> ForestM fs [FilePath]
	getDirectoryContentsInTree path tree = forestIO . getDirectoryContents =<< pathInTree path tree
	doesDirectoryExistInTree :: FilePath -> FSTree fs -> ForestM fs Bool
	doesDirectoryExistInTree path tree = forestIO . doesDirectoryExist =<< pathInTree path tree
	doesFileExistInTree :: FilePath -> FSTree fs -> ForestM fs Bool
	doesFileExistInTree path tree = forestIO . doesFileExist =<< pathInTree path tree
	doesExistInTree :: FilePath -> FSTree fs -> ForestM fs Bool
	doesExistInTree path tree = pathInTree path tree >>= \diskpath -> do
		isDir <- forestIO $ doesDirectoryExist diskpath
		if isDir then return True else forestIO $ doesFileExist diskpath
	-- returns a canonical version of a filepath according for a given tree
	canonalizePathWithTree :: FilePath -> FSTree fs -> ForestM fs FilePath
	canonalizePathWithTree path tree = flip pathFromTree tree =<< forestIO . canonalizePath =<< pathInTree path tree

	-- it may fail in case it is not possible to compute a difference between the two trees
	diffFS :: FSTree fs -> FSTree fs -> FilePath -> ForestM fs (Maybe FSTreeDeltaNodeMay)

-- canonalizes a filepath, but leaving the filename uncanonized
canonalizeDirectoryInTree :: FSRep fs => FilePath -> FSTree fs -> ForestM fs FilePath
canonalizeDirectoryInTree path tree = do
	let (root,file) = splitFileName path
	canroot <- canonalizePathWithTree root tree
	return $ canroot </> file

----------


class (FSRep fs,Show a) => Matching (fs :: FS) a where
	getMatchingFilesInTree :: FilePath -> a -> FSTree fs -> ForestM fs [FilePath]
	defaultMatch :: Proxy fs -> a -> ForestM fs [FilePath]

instance FSRep fs => Matching fs [FilePath] where
	getMatchingFilesInTree _ files _ = return files
	defaultMatch fs files = return files

instance FSRep fs => Matching fs FilePath where
	getMatchingFilesInTree _ file _ = return [file]
	defaultMatch fs file = return [file]

instance FSRep fs => Matching fs RE where
	getMatchingFilesInTree = getMatchingFilesREInTree
	defaultMatch fs re = return []

instance FSRep fs => Matching fs GL where
	getMatchingFilesInTree = getMatchingFilesGlobInTree
	defaultMatch fs gl = return []

getMatchingFilesREInTree :: FSRep fs => FilePath -> RE -> FSTree fs -> ForestM fs [FilePath]
getMatchingFilesREInTree path re tree = do 
	files <- getDirectoryContentsInTree path tree
	let matches = (filterByRegex re files)
	return matches

getMatchingFilesGlobInTree :: FSRep fs => FilePath -> GL -> FSTree fs -> ForestM fs [FilePath]
getMatchingFilesGlobInTree path (GL glob) tree = do 
	files <- getDirectoryContentsInTree path tree
	let gl = compile glob
	let matches = (Prelude.filter (match gl) files)
	return matches

getMatchingFilesInTreeM :: (FSRep fs,Matching fs a) => FilePath -> ForestM fs a -> FSTree fs -> ForestM fs [FilePath]
getMatchingFilesInTreeM path matchingM tree = do
	matching <- matchingM
	getMatchingFilesInTree path matching tree
