{-# LANGUAGE EmptyDataDecls, DoAndIfThenElse, OverlappingInstances, TypeOperators, FunctionalDependencies, UndecidableInstances, ConstraintKinds,ScopedTypeVariables, TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, DataKinds, TypeFamilies, Rank2Types, GADTs, ViewPatterns, DeriveDataTypeable #-}

-- A module for filesystem-specific operations

module Language.Forest.FS.FSRep where

import Prelude hiding (const,read,mod)
import System.Mem.WeakRef
import Control.Monad
--import Control.Monad.IO.Class
--import Control.Lens hiding (runIdentity,inside)
import Data.Maybe
import Data.WithClass.MData
--import Data.Functor.Identity
import Data.List.Split
import Data.Typeable
import System.Directory
import System.IO.Unsafe
import qualified Data.List as List
import Data.IORef
import System.FilePath.Posix
import System.Posix.IO
import System.IO
import Language.Forest.Shell

import Language.Pads.MetaData hiding (numErrors)
import Language.Pads.CoreBaseTypes
import Language.Forest.Errors
import Data.Data hiding (gmapT)
import System.Posix.Types
import Data.WithClass.Derive.MData
import Data.DeriveTH
import Data.Derive.Data
import Foreign.C.Types
import System.FilePath.Canonical
import System.Posix.Files
import System.Posix.User
import System.Process
import System.Posix.Time
import Control.Exception as Exception
import Language.Forest.FS.FSDelta
import Control.Monad.Incremental
import Data.Unique
import Data.Int
import Control.Monad.Incremental.Adapton
import Control.Monad.Incremental.List
import Control.Monad.Incremental.Generics
import Data.Word
import Data.Time.Clock

import Data.DeepTypeable
import Data.WithClass.Derive.DeepTypeable
import Language.Haskell.TH.Syntax hiding (Loc(..))

import Debug.Trace

debug str = trace str

-- * Code that needs to be declared here due to module dependencies, though not really awesome

type GetForestMD fs = (FilePath -> FSTree fs -> ForestI fs (Forest_md fs))

data Forest_md fs = Forest_md { errors :: ForestFSThunk fs Inside Forest_err, fileInfo :: FileInfo } deriving (Typeable)

deriving instance (FSRep fs,ForestInput fs FSThunk Inside) => Eq (Forest_md fs)

instance (DeepTypeable fs) => DeepTypeable (Forest_md fs) where
	typeTree (_::Proxy (Forest_md fs)) = MkTypeTree (mkName "Language.Forest.FS.FSRep.Forest_md") [typeTree (Proxy::Proxy fs)] [MkConTree (mkName "Language.Forest.FS.FSRep.Forest_md") [typeTree (Proxy::Proxy (ForestFSThunk fs Inside Forest_err)),typeTree (Proxy::Proxy FileInfo)]]

data FileInfo = FileInfo { fullpath :: FilePath
                         , owner :: String
                         , group :: String
                         , size  :: COff
                         , access_time :: EpochTime
                         , mod_time :: EpochTime
                         , read_time :: EpochTime
                         , mode :: FileMode
                         , symLink :: Maybe FilePath
                         , kind :: FileType
                         }
       deriving (Eq, Typeable, Ord)
data FileType = UnknownK | AsciiK | BinaryK | DirectoryK 
 deriving (Eq, Typeable, Ord)

{- Should raise no exceptions -}
-- it loads the metadata from a physical path on disk, but returns the metadata as if it belonged to an original path alias
getForestMD :: (FSRep fs,ForestLayer fs l,ForestInput fs FSThunk Inside) => FilePath -> FilePath -> ForestL fs l (Forest_md fs)
getForestMD oripath diskpath = do
	fdEither <- forestIO $ Exception.try $ getFileStatus diskpath
	case fdEither of
		Left (e::Exception.SomeException) -> do
			err <- inside $ fsthunk [] $ return $ Forest_err { numErrors = 1, errorMsg = Just (ForestIOException (show e)) }
			return $ Forest_md { errors = err, fileInfo = mkErrorFileInfo oripath }
		Right fd -> do 
			let file_ownerID = fileOwner fd
			ownerEntry    <- forestIO $ getUserEntryForID file_ownerID
			let file_groupID = fileGroup fd
			groupEntry <- forestIO $ getGroupEntryForID file_groupID
			fdsym <- forestIO $ getSymbolicLinkStatus diskpath
			symLinkOpt <- forestIO $ if isSymbolicLink fdsym then readOptSymLink diskpath else return Nothing
			readTime <- forestIO epochTime
			knd <- forestIO $ fileStatusToKind diskpath fd
			err <- inside $ fsthunk [] $ return $ Forest_err { numErrors = 0, errorMsg = Nothing }
			let info = FileInfo { fullpath = oripath, owner = userName ownerEntry, group = groupName groupEntry, size  = fileSize fd, access_time = accessTime fd, mod_time = modificationTime fd, read_time = readTime, mode     =  fileMode fd, symLink =symLinkOpt, kind  = knd }
			return $ Forest_md { errors = err, fileInfo = info } 

mkErrorFileInfo :: FilePath -> FileInfo
mkErrorFileInfo  path = FileInfo 
     { fullpath = path
     , owner = ""
     , group = ""
     , size  = -1
     , access_time = -1
     , read_time = -1
     , mod_time = -1
     , mode     = -1
     , symLink = Nothing
     , kind = UnknownK
     }

readOptSymLink :: FilePath -> IO (Maybe FilePath)
readOptSymLink fp = do 
	link <- readSymbolicLink fp
	return (Just link)

fileStatusToKind :: FilePath -> FileStatus -> IO FileType
fileStatusToKind path fs = do
    if isRegularFile fs then 
      do { ascii <- isAscii path
         ; if ascii then return AsciiK else return BinaryK
         }
    else if isDirectory fs then return DirectoryK
    else return UnknownK

isAscii :: FilePath -> IO Bool
isAscii fp =  do
	let cmd = "file -i -L " ++ show fp
	result <- runShellCommand cmd
	return ("ascii" `List.isSuffixOf` result)

canonalizePath :: FilePath -> IO FilePath
canonalizePath path = liftM canonicalFilePath $ canonical path

---------------------------------------------------------------

-- * Incremental computation

data IncForest (fs :: FS) deriving Typeable

instance DeepTypeable IncForest where
	typeTree (_::Proxy (IncForest)) = MkTypeTree (mkName "Language.Forest.FS.FSRep.IncForest") [] []

instance DeepTypeable fs => DeepTypeable (IncForest fs) where
	typeTree (_::Proxy (IncForest fs)) = MkTypeTree (mkName "Language.Forest.FS.FSRep.IncForest") [typeTree (Proxy::Proxy fs)] []

-- * Filesystem-specific configuration and operations

data FS = NoFS | NILFS deriving Typeable

instance DeepTypeable FS where
	typeTree (_::Proxy FS) = MkTypeTree (mkName "Language.Forest.FS.FSRep.IncForest") [] [MkConTree (mkName "Language.Forest.FS.FSRep.FS") [typeTree (Proxy::Proxy NoFS),typeTree (Proxy::Proxy NILFS)]]
instance DeepTypeable NoFS where
	typeTree (_::Proxy NoFS) = MkTypeTree (mkName "Language.Forest.FS.FSRep.NoFS") [] []
instance DeepTypeable NILFS where
	typeTree (_::Proxy NILFS) = MkTypeTree (mkName "Language.Forest.FS.FSRep.NILFS") [] []

deriving instance Typeable NoFS
deriving instance Typeable NILFS

type ForestLayer fs l = (Layer l (IncForest fs) IORef IO,Layer l Adapton IORef IO)
type ForestL fs (l :: * -> (* -> *) -> (* -> *) -> * -> *) = l (IncForest fs) IORef IO

type ForestFSThunk fs l = FSThunk fs l (IncForest fs) IORef IO
type ForestFSThunkI fs = ForestFSThunk fs Inside
type ForestFSThunkO fs = ForestFSThunk fs Outside

type ForestICThunk fs l = ICThunk fs l (IncForest fs) IORef IO
type ForestICThunkI fs = ForestICThunk fs Inside
type ForestICThunkO fs = ForestICThunk fs Outside

type ForestHSThunk fs l = HSThunk fs l (IncForest fs) IORef IO
type ForestHSThunkI fs = ForestHSThunk fs Inside
type ForestHSThunkO fs = ForestHSThunk fs Outside

instance (FSRep fs) => Eq (ForestFSThunk fs l a) where
	t1 == t2 = eqFSThunk t1 t2
instance FSRep fs => Eq (ForestICThunk fs l a) where
	t1 == t2 = eqICThunk t1 t2
	
type ForestO fs = Outside (IncForest fs) IORef IO
type ForestI fs = Inside (IncForest fs) IORef IO

type ForestCfg fs = IncrementalArgs (IncForest fs)

-- | Class that implements filesystem-specific operations
class (ForestThunk fs HSThunk Inside,ForestThunk fs HSThunk Outside,ForestOutput fs ICThunk Outside,ForestOutput fs ICThunk Inside,ForestInput fs FSThunk Outside,ForestInput fs FSThunk Inside,Eq (FSTree fs),DeepTypeable fs,Incremental (IncForest fs) IORef IO) => FSRep (fs :: FS) where
	
	-- runs a Forest computation (includes the FS monitor, incremental computation, etc)
	runForest :: ForestCfg fs -> ForestO fs a -> IO a
	runForest = runIncremental
	
	-- we don't want to make the Forest monads instances of @MonadIO@ to prevent users from using it
	forestIO :: ForestLayer fs l => IO a -> ForestL fs l a
	
	-- runs an outer computation at the inner layer (this is unsafe, and only to used to be used internally!)
	unsafeInside :: ForestO fs a -> ForestI fs a
	
	-- | returns the forest directory used to store auxiliary data
	getForestDirectory :: ForestLayer fs l => ForestL fs l FilePath
	
	-- Filesystem tree representation
	data FSTree fs :: *
	-- registers the deletion of a path from the filesystem
	deletePath :: FilePath -> ForestO fs ()
	-- registers a write to the contents of a file in the filesystem; receives an on-disk location from which the content can be retrieved
	writeFile :: FilePath -> OnDisk -> ForestO fs ()
	-- registers the creation of a new empty directory in the filesystem
	writeDir :: FilePath -> ForestO fs ()
	-- registers a creation of a symbolic link in the filesystem; receives a target on-disk location that the link points to
	writeLink :: FilePath -> FilePath -> ForestO fs ()
	-- registers a write to the metadata of a path in the filesystem; receives an on-disk location from which the metadata can be retrieved
	writePathMD :: FilePath -> OnDisk -> ForestO fs ()
	-- generates a fresh forest temporary path
	tempPath :: ForestLayer fs l => ForestL fs l FilePath
	-- creates an AVFS virtual tree
	virtualTree :: ForestLayer fs l => FSTree fs -> ForestL fs l (FSTree fs)
	-- | removes a tree by making sure to evaluate all thunks depending on it and deleting all internal data stored about it
	purgeTree :: ForestLayer fs l => FSTree fs -> ForestL fs l ()
	-- * Operations on the filesystem
	-- gets the latest filesystem tree from the OS
--	latestTreeIO :: Proxy fs -> IO (FSTree fs)
	-- gets the latest tree and performs some book-keeping in the Forest monad
	latestTree :: ForestO fs (FSTree fs)
	-- returns a newer tree and a sequence of modifications that occurred between the last and the current tree
	-- the @FSTreeDelta@ must be absolute
	changesBetween :: FSTree fs -> FSTree fs -> ForestO fs FSTreeDelta
	-- translates a tree filepath to an on-disk filepath
	pathInTree :: ForestLayer fs l => FilePath -> FSTree fs -> ForestL fs l OnDisk
	-- translates an on-disk filepath to a tree filepath
	pathFromTree :: ForestLayer fs l => OnDisk -> FSTree fs -> ForestL fs l FilePath
	-- appends a relative path to an original tree path
	stepPathInTree :: ForestLayer fs l => FSTree fs -> FilePath -> FilePath -> ForestL fs l FilePath
	
	getForestMDInTree :: (ForestLayer fs l,ForestInput fs FSThunk Inside) => FilePath -> FSTree fs -> ForestL fs l (Forest_md fs)
	getForestMDInTree path tree = getForestMD path =<< pathInTree path tree
	getDirectoryContentsInTree :: ForestLayer fs l => FilePath -> FSTree fs -> ForestL fs l [FilePath]
	getDirectoryContentsInTree path tree = forestIO . getDirectoryContents =<< pathInTree path tree
	doesDirectoryExistInTree :: ForestLayer fs l => FilePath -> FSTree fs -> ForestL fs l Bool
	doesDirectoryExistInTree path tree = forestIO . doesDirectoryExist =<< pathInTree path tree
	doesFileExistInTree :: ForestLayer fs l => FilePath -> FSTree fs -> ForestL fs l Bool
	doesFileExistInTree path tree = forestIO . doesFileExist =<< pathInTree path tree
	doesExistInTree :: ForestLayer fs l => FilePath -> FSTree fs -> ForestL fs l Bool
	doesExistInTree path tree = pathInTree path tree >>= \diskpath -> do
		isDir <- forestIO $ doesDirectoryExist diskpath
		if isDir then return True else forestIO $ doesFileExist diskpath
	canonalizePathInTree :: ForestLayer fs l => FilePath -> FSTree fs -> ForestL fs l OnDisk
	canonalizePathInTree path tree = forestIO . canonalizePath =<< pathInTree path tree

	showFSTree :: FSTree fs -> String
	-- | is a tree more recent than a given point in time?
--	compareTreeWithTime :: FSTree fs -> UTCTime -> Ordering

	-- unevaluated thunks that depend on the filesystem (but not on other thunks) and can be modified, but that does not depend on other thunks
	-- XXX: lazy operations on the inner layer?
	data FSThunk fs (l :: * -> (* -> *) -> (* -> *) -> * -> *) (inc :: *) (r :: * -> *) (m :: * -> *) a :: *

	eqFSThunk :: FSThunk fs l (IncForest fs) IORef IO a -> FSThunk fs l (IncForest fs) IORef IO a -> Bool
	fsthunkID :: FSThunk fs l (IncForest fs) IORef IO a -> Unique

	isUnevaluatedFSThunk :: (ForestLayer fs l,ForestLayer fs l1) => ForestFSThunk fs l1 a -> ForestL fs l Bool
	isUnforcedFSThunk :: (ForestLayer fs l,ForestLayer fs l1) => ForestFSThunk fs l1 a -> ForestL fs l Bool

	-- incremental computation thunks that depend on @FSThunk@s and cannot be directly modified, but are updated for changes on dependencies
	data ICThunk fs (l :: * -> (* -> *) -> (* -> *) -> * -> *) (inc :: *) (r :: * -> *) (m :: * -> *) a :: *
	eqICThunk :: ICThunk fs l (IncForest fs) IORef IO a -> ICThunk fs l (IncForest fs) IORef IO a -> Bool
	icthunkID :: ICThunk fs l (IncForest fs) IORef IO a -> Unique

	isUnevaluatedICThunk :: (ForestLayer fs l,ForestLayer fs l1) => ForestICThunk fs l1 a -> ForestL fs l Bool

	-- | A plain thunk that simply stores a computation; cannot be mutated, and is not incrementally repaired
	data HSThunk fs (l :: * -> (* -> *) -> (* -> *) -> * -> *) (inc :: *) (r :: * -> *) (m :: * -> *) a :: *
	
	-- generic memoization functions
	memo :: (ForestInput fs FSThunk Inside,Typeable rep,Typeable md,Typeable arg) => FilePath -> (ForestFSThunk fs Inside rep,ForestFSThunk fs Inside md,arg) -> FSTree fs -> ForestI fs ()
	unmemo :: Typeable rep => Proxy fs -> FilePath -> Proxy rep -> IO ()
	lookupmemo :: (Typeable rep,Typeable md,Typeable arg) => FilePath -> Proxy rep -> ForestI fs (Maybe ((ForestFSThunk fs Inside rep,ForestFSThunk fs Inside md,arg),FSTree fs))

newHSThunk :: (Eq a,ForestThunk fs HSThunk l,ForestLayer fs l) => ForestL fs l a -> ForestL fs l (ForestHSThunk fs l a)
newHSThunk = new

type ForestThunk fs mod l = Thunk (mod fs) l (IncForest fs) IORef IO
type ForestOutput fs mod l = Output (mod fs) l (IncForest fs) IORef IO

class (ForestLayer fs l) => ForestInput fs mod l where
	fsref :: (Eq a) => a -> ForestL fs l (mod fs l (IncForest fs) IORef IO a)
	fsthunk :: (Eq a) => [FSTree fs] -> ForestL fs l a -> ForestL fs l (mod fs l (IncForest fs) IORef IO a)
	fsset :: (Eq a) => mod fs l (IncForest fs) IORef IO a -> a -> ForestO fs ()
	fsmodify :: (Eq a) => [FSTree fs] -> mod fs l (IncForest fs) IORef IO a -> (a -> ForestL fs l a) -> ForestO fs ()
	fsoverwrite :: (Eq a) => [FSTree fs] -> mod fs l (IncForest fs) IORef IO a -> ForestL fs l a -> ForestO fs ()
	fsforce :: (Eq a) => mod fs l (IncForest fs) IORef IO a -> ForestL fs l a

	-- adds a finalizer to a Forest Input thunk to be run whenever it is modified
	addUnmemoFSThunk :: mod fs l (IncForest fs) IORef IO a -> IO () -> ForestL fs l ()

fsRef :: (Eq a,FSRep fs,ForestInput fs FSThunk l) => a -> ForestL fs l (ForestFSThunk fs l a)
fsRef = fsref
fsThunk :: (Eq a,FSRep fs,ForestInput fs FSThunk l) => [FSTree fs] -> ForestL fs l a -> ForestL fs l (ForestFSThunk fs l a)
fsThunk = fsthunk

instance ForestInput fs FSThunk l => Thunk (FSThunk fs) l (IncForest fs) IORef IO where
	new = error "you shouldn't be able to manually create FSThunks"
	newc = fsref
	read = fsforce

instance (ForestLayer fs l,ForestInput fs FSThunk l) => Input (FSThunk fs) l (IncForest fs) IORef IO where
	ref = fsref
	mod = error "you shouldn't be able to manually create FSThunks"
	get = fsforce
	set = error "you shouldn't be able to manually modify FSThunks"
	overwrite = error "you shouldn't be able to manually modify FSThunks"
	modify = error "you shouldn't be able to manually modify FSThunks"

deriving instance Typeable FSThunk
deriving instance Typeable ICThunk

instance (Eq a,ForestLayer fs l,ForestThunk fs FSThunk l,MData ctx (ForestL fs l) a
		, Sat (ctx (ForestFSThunk fs l a)),DeepTypeable (ForestFSThunk fs l a)
		) => MData ctx (ForestL fs l) (ForestFSThunk fs l a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = Data.WithClass.MData.dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Language.Forest.FS.FSRep.FSThunk" [mkConstr ty "FSThunk" [] Prefix]
		
instance (Eq a,ForestLayer fs l,ForestThunk fs ICThunk l,MData ctx (ForestL fs l) a
		, Sat (ctx (ForestICThunk fs l a)),DeepTypeable (ForestICThunk fs l a)
		) => MData ctx (ForestL fs l) (ForestICThunk fs l a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = Data.WithClass.MData.dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Language.Forest.FS.FSRep.ICThunk" [mkConstr ty "ICThunk" [] Prefix]

instance (Eq a,ForestLayer fs l,ForestThunk fs HSThunk l,MData ctx (ForestL fs l) a
		, Sat (ctx (ForestHSThunk fs l a)),DeepTypeable (ForestHSThunk fs l a)
		) => MData ctx (ForestL fs l) (ForestHSThunk fs l a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = Data.WithClass.MData.dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Language.Forest.FS.FSRep.HSThunk" [mkConstr ty "HSThunk" [] Prefix]

----------

$(derive makeDataAbstract ''COff)
$(derive makeDataAbstract ''CMode)
$(derive makeDataAbstract ''CTime)
deriving instance Data FileType
deriving instance Data FileInfo

$(derive makeMDataAbstract ''COff)
$(derive makeMDataAbstract ''CMode)
$(derive makeMDataAbstract ''CTime)
$( derive makeMData ''FileType )
$( derive makeMData ''FileInfo )

$(derive makeDeepTypeableAbstract ''COff)
$(derive makeDeepTypeableAbstract ''CMode)
$(derive makeDeepTypeableAbstract ''CTime)
$( derive makeDeepTypeable ''FileType )
$( derive makeDeepTypeable ''FileInfo )

proxyIncForest :: Proxy fs -> Proxy (IncForest fs)
proxyIncForest fs = Proxy

type ForestListICThunk fs l a = ListMod (ICThunk fs) l (IncForest fs) IORef IO a
type ForestListICThunk' fs l a = ListMod' (ICThunk fs) l (IncForest fs) IORef IO a
type ForestListICThunkI fs a = ForestListICThunk fs Inside a
type ForestListICThunkO fs a = ForestListICThunk fs Outside a
type ForestListICThunkI' fs a = ForestListICThunk' fs Inside a
type ForestListICThunkO' fs a = ForestListICThunk' fs Outside a

type ForestJoinListICThunk fs l a = JoinListMod (ICThunk fs) l (IncForest fs) IORef IO a
type ForestJoinListICThunk' fs l a = JoinListMod' (ICThunk fs) l (IncForest fs) IORef IO a
type ForestJoinListICThunkI fs a = ForestJoinListICThunk fs Inside a
type ForestJoinListICThunkO fs a = ForestJoinListICThunk fs Outside a
type ForestJoinListICThunkI' fs a = ForestJoinListICThunk' fs Inside a
type ForestJoinListICThunkO' fs a = ForestJoinListICThunk' fs Outside a

type ForestGenericQMemoBut ctx fs l b = GenericQMemoBut ctx (ICThunk fs) l (IncForest fs) IORef IO b
type ForestGenericQMemo ctx fs l b = GenericQMemo ctx (ICThunk fs) l (IncForest fs) IORef IO b

instance DeepTypeable fs => DeepTypeable (FSThunk fs) where
	typeTree (_::Proxy (FSThunk fs)) = MkTypeTree (mkName "Language.Forest.FS.FSRep.FSThunk") [typeTree (Proxy::Proxy fs)] []

instance DeepTypeable fs => DeepTypeable (ICThunk fs) where
	typeTree (_::Proxy (ICThunk fs)) = MkTypeTree (mkName "Language.Forest.FS.FSRep.ICThunk") [typeTree (Proxy::Proxy fs)] []

instance (DeepTypeable fs,DeepTypeable l,DeepTypeable inc,DeepTypeable r,DeepTypeable m,DeepTypeable a) => DeepTypeable (FSThunk fs l inc r m a) where
	typeTree (_::Proxy (FSThunk fs l inc r m a)) = MkTypeTree (mkName "Language.Forest.FS.FSRep.FSThunk") args [MkConTree (mkName "Language.Forest.FS.FSRep.fsthunk") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy fs),typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy m),typeTree (Proxy::Proxy a)]
	
instance (DeepTypeable fs,DeepTypeable l,DeepTypeable inc,DeepTypeable r,DeepTypeable m,DeepTypeable a) => DeepTypeable (ICThunk fs l inc r m a) where
	typeTree (_::Proxy (ICThunk fs l inc r m a)) = MkTypeTree (mkName "Language.Forest.FS.FSRep.ICThunk") args [MkConTree (mkName "Language.Forest.FS.FSRep.thunk") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy fs),typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy m),typeTree (Proxy::Proxy a)]


-- | A type for twin-traversals
infixr 5 :.:
data a :.: b = a :.: b deriving (Typeable,Eq,Show,Ord) -- the forest datatype for pairs of arguments

fstF (x :.: y) = x
sndF (x :.: y) = y

instance (Memo a,Memo b) => Memo (a :.: b) where
	type Key (a :.: b) = (Key a,Key b)
	{-# INLINE memoKey #-}
	memoKey (x :.: y) = (andMkWeak wx wy,(kx,ky))
		where (wx,kx) = memoKey x
		      (wy,ky) = memoKey y

-- * copying @FSThunk@s

class (FSRep fs,ForestLayer fs l) => CopyFSThunks fs l a where
	copyFSThunks :: Proxy fs -> Proxy l -> a -> ForestL fs l a

copyFSThunksProxy :: Proxy fs -> Proxy l -> Proxy (CopyFSThunksDict fs l)
copyFSThunksProxy fs l = Proxy

data CopyFSThunksDict fs l a = CopyFSThunksDict { copyFSThunksDict :: Proxy fs -> Proxy l -> a -> ForestL fs l a }

instance (CopyFSThunks fs l a) => Sat (CopyFSThunksDict fs l a) where
	dict = CopyFSThunksDict { copyFSThunksDict = copyFSThunks }

-- we make a strict copy by forcing the original thunk
instance (FSRep fs,ForestLayer fs l,Eq a,ForestInput fs FSThunk l) => CopyFSThunks fs l (ForestFSThunk fs l a) where
	copyFSThunks _ _ t = fsforce t >>= fsref 

-- just traverse recursively, until there are no more @FSThunks@ inside the type
instance (FSRep fs,ForestLayer fs l,MData (CopyFSThunksDict fs l) (ForestL fs l) a) => CopyFSThunks fs l a where
	 copyFSThunks fs l x = do
		let hasFSThunk (MkTypeTree name _ _) = showName name == "Language.Forest.FS.FSRep.FSThunk"
		if hasDeepTypeable hasFSThunk (proxyOf x)
			then gmapT (copyFSThunksProxy fs l) (copyFSThunksDict dict fs l) x
			else return x

