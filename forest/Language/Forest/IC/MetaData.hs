{-# LANGUAGE TupleSections, StandaloneDeriving, DataKinds, TypeFamilies, TypeOperators, UndecidableInstances, ConstraintKinds, FlexibleContexts, MultiParamTypeClasses, RankNTypes, NamedFieldPuns, RecordWildCards, FlexibleInstances, DeriveDataTypeable, TemplateHaskell,ScopedTypeVariables, DoAndIfThenElse,
    TypeSynonymInstances #-}

module Language.Forest.IC.MetaData where

import Language.Forest.FS.FSRep
import Language.Forest.IC.ICRep
import Data.DeepTypeable
import Language.Forest.Pure.MetaData (FileInfo(..),FileType(..),(:*:)(..))
import qualified Language.Forest.Pure.MetaData as Pure
import Control.Exception as Exception
import System.Posix.Files
import System.Posix.User
import System.Posix.Types
import System.FilePath.Posix
import System.Process
import GHC.IO.Handle
import Foreign.C.Types
import Language.Forest.Errors
import Control.Monad.Incremental
import Prelude hiding (mod)
import Language.Forest.IO.Utils
import System.Time.Utils
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Proxy
import System.Posix.Time
import Data.WithClass.Derive.DeepTypeable
import Language.Haskell.TH.Syntax hiding (Loc(..))
import Data.Typeable
import Data.DeriveTH
import Data.WithClass.Derive.MData
import Data.WithClass.MData
import Data.Int
import Data.Word
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.ByteString as B
import Control.Monad
import Data.WithClass.MGenerics
import System.Mem.MemoTable
import System.Mem.StableName
import System.Mem.WeakKey
import System.IO.Unsafe
import System.Mem.Weak

import Language.Haskell.TH.Syntax
import Data.WithClass.MData
import Data.DeriveTH
import Data.WithClass.Derive.MData
import Data.WithClass.Derive.DeepTypeable
import Data.DeepTypeable

-- * Forest metadata lifted for IC, where errors have explicit thunks

type GetForestMD fs = (FilePath -> FSTree fs -> ForestI fs (Forest_md fs))

data Forest_md fs = Forest_md { errors :: ForestFSThunk fs Inside Forest_err, fileInfo :: FileInfo } deriving (Typeable)

deriving instance (ICRep fs,ForestInput fs FSThunk Inside) => Eq (Forest_md fs)

instance (DeepTypeable fs) => DeepTypeable (Forest_md fs) where
	typeTree (_::Proxy (Forest_md fs)) = MkTypeTree (mkName "Language.Forest.FS.ICRep.Forest_md") [typeTree (Proxy::Proxy fs)] [MkConTree (mkName "Language.Forest.FS.ICRep.Forest_md") [typeTree (Proxy::Proxy (ForestFSThunk fs Inside Forest_err)),typeTree (Proxy::Proxy FileInfo)]]


{- Should raise no exceptions -}
-- it loads the metadata from a physical path on disk, but returns the metadata as if it belonged to an original path alias
getForestMD :: (ICRep fs,ForestLayer fs l,ForestInput fs FSThunk Inside) => FilePath -> FilePath -> ForestL fs l (Forest_md fs)
getForestMD oripath diskpath = do
	fdEither <- forestM $ forestIO $ Exception.try $ getFileStatus diskpath
	case fdEither of
		Left (e::Exception.SomeException) -> do
			err <- inside $ mod $ return $ Forest_err { numErrors = 1, errorMsg = Just (ForestIOException (show e)) }
			return $ Forest_md { errors = err, fileInfo = Pure.mkErrorFileInfo oripath }
		Right fd -> do 
			let file_ownerID = fileOwner fd
			ownerEntry    <- forestM $ forestIO $ getUserEntryForID file_ownerID
			let file_groupID = fileGroup fd
			groupEntry <- forestM $ forestIO $ getGroupEntryForID file_groupID
			fdsym <- forestM $ forestIO $ getSymbolicLinkStatus diskpath
			symLinkOpt <- forestM $ forestIO $ if isSymbolicLink fdsym then readOptSymLink diskpath else return Nothing
			readTime <- forestM $ forestIO epochTime
			knd <- forestM $ forestIO $ Pure.fileStatusToKind diskpath fd
			err <- inside $ mod $ return $ Forest_err { numErrors = 0, errorMsg = Nothing }
			let info = FileInfo { fullpath = oripath, owner = userName ownerEntry, group = groupName groupEntry, size  = fileSize fd, access_time = accessTime fd, mod_time = modificationTime fd, read_time = readTime, mode     =  fileMode fd, symLink =symLinkOpt, kind  = knd }
			return $ Forest_md { errors = err, fileInfo = info } 


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

-- for cases where we want to avoid reading from the filesystme
doesExistInMD :: FilePath -> Forest_md fs -> Bool
doesExistInMD path fmd = (fullpath (fileInfo fmd) == path) && (access_time (fileInfo fmd) > 0 || read_time (fileInfo fmd) > 0)

doesFileExistInMD :: FilePath -> Forest_md fs -> Bool
doesFileExistInMD path fmd = doesExistInMD path fmd && kind (fileInfo fmd) /= DirectoryK

doesDirectoryExistInMD :: FilePath -> Forest_md fs -> Bool
doesDirectoryExistInMD path fmd = doesExistInMD path fmd && kind (fileInfo fmd) == DirectoryK

forest_md_def :: (ForestInput fs FSThunk Inside,ForestLayer fs l) => ForestL fs l (Forest_md fs)
forest_md_def = do
	err <- inside $ mod $ return Pure.forest_err_def
	return $ Forest_md err Pure.fileInfo_def
	
fmd_fullpath :: Forest_md fs -> FilePath
fmd_fullpath = fullpath . fileInfo

fmd_kind :: Forest_md fs -> FileType
fmd_kind = kind . fileInfo

fmd_symLink :: Forest_md fs -> Maybe FilePath
fmd_symLink = symLink . fileInfo

-- we need to actually create new thunks to allow the modification to occur at the inner layer
modify_errors_under :: (Typeable md,Eq md,ForestMD fs md) => ForestFSThunk fs Inside md -> (Forest_err -> ForestI fs Forest_err) -> ForestO fs ()
modify_errors_under t newerrs = modify t $ \md -> replace_errors md newerrs

{- Meta data type class -}
class (ICRep fs,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => ForestMD fs md where
	isUnevaluatedMDThunk :: ForestLayer fs l => md -> ForestL fs l Bool
	isUnforcedMDThunk :: ForestLayer fs l => md -> ForestL fs l Bool
	get_fmd_header :: ForestLayer fs l => md -> ForestL fs l (Forest_md fs)
	replace_fmd_header :: md -> (Forest_md fs -> ForestI fs (Forest_md fs)) -> ForestI fs md
	
	get_errors_thunk :: ForestLayer fs l => md -> ForestL fs l (ForestFSThunk fs Inside Forest_err)
	get_errors_thunk md = do
		fmd <- get_fmd_header md
		return $ errors fmd
	get_errors :: ForestLayer fs l => md -> ForestL fs l Forest_err
	get_errors md = do
		fmd <- get_fmd_header md
		inside $ get $ errors fmd
	isValidMD :: ForestLayer fs l => md -> ForestL fs l Bool
	isValidMD md = do
		err <- get_errors md
		return $ numErrors err == 0
	replace_errors :: md -> (Forest_err -> ForestI fs Forest_err) -> ForestI fs md
	replace_errors md f = do
		replace_fmd_header md $ \fmd -> do
			t <- mod $ f =<< get_errors fmd
			return $ fmd { errors = t }
	modify_errors :: md -> (Forest_err -> ForestI fs Forest_err) -> ForestO fs ()
	modify_errors md newerrs = do
		fmd <- get_fmd_header md
		let err_thunk = errors fmd
		modify err_thunk newerrs
	overwrite_errors :: md -> ForestI fs Forest_err -> ForestO fs ()
	overwrite_errors md newerrs = do
		fmd <- get_fmd_header md
		let err_thunk = errors fmd
		overwrite err_thunk newerrs
	
	get_fileInfo :: ForestLayer fs l => md -> ForestL fs l FileInfo
	get_fileInfo md = liftM fileInfo (get_fmd_header md)
	get_fullpath :: ForestLayer fs l => md -> ForestL fs l String
	get_fullpath md = liftM (Pure.fullpath . fileInfo) (get_fmd_header md)
	get_owner :: ForestLayer fs l => md -> ForestL fs l String
	get_owner md = liftM (Pure.owner . fileInfo) (get_fmd_header md)
	get_group :: ForestLayer fs l => md -> ForestL fs l String
	get_group md = liftM (Pure.group . fileInfo) (get_fmd_header md)
	get_size :: ForestLayer fs l => md -> ForestL fs l COff
	get_size md = liftM (size . fileInfo) (get_fmd_header md)
	get_access_time :: ForestLayer fs l => md -> ForestL fs l EpochTime
	get_access_time md = liftM (access_time . fileInfo) (get_fmd_header md)
	get_mod_time :: ForestLayer fs l => md -> ForestL fs l EpochTime
	get_mod_time md = liftM (mod_time . fileInfo) (get_fmd_header md)
	get_read_time :: ForestLayer fs l => md -> ForestL fs l EpochTime
	get_read_time md = liftM (read_time . fileInfo) (get_fmd_header md)
	get_mode :: ForestLayer fs l => md -> ForestL fs l FileMode
	get_mode md = liftM (mode . fileInfo) (get_fmd_header md)
	get_modes :: ForestLayer fs l => md -> ForestL fs l String
	get_modes md = liftM (Pure.modeToModeString . mode . fileInfo) (get_fmd_header md)
	get_symLink :: ForestLayer fs l => md -> ForestL fs l (Maybe FilePath)
	get_symLink md = liftM (symLink . fileInfo) (get_fmd_header md)
	get_kind :: ForestLayer fs l => md -> ForestL fs l FileType
	get_kind md = liftM (kind . fileInfo) (get_fmd_header md)

change_fmd_header :: (Typeable md,Eq md,ICRep fs,ForestInput fs FSThunk Inside,ForestInput fs FSThunk l) => ForestFSThunk fs l (Forest_md fs,md) -> Forest_md fs -> ForestO fs ()
change_fmd_header t fmd' = modify t $ \(fmd,md) -> return (fmd',md)

instance (ICRep fs,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => ForestMD fs (Forest_md fs) where
	isUnevaluatedMDThunk fmd = return False
	isUnforcedMDThunk fmd = return False
	get_fmd_header b = return b
	replace_fmd_header fmd f = f fmd

-- the left side may be a @Forest_md@ and the right side a metadata value
instance (ICRep fs,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => ForestMD fs (Forest_md fs,b) where
	isUnevaluatedMDThunk (fmd,md) = error "isUnforcedMDThunk should never be called for a Forest_md"
	isUnforcedMDThunk (fmd,md) = error "isUnforcedMDThunk should never be called for a Forest_md"
	get_fmd_header (a,b) = get_fmd_header a
	replace_fmd_header (fmd,b) f = replace_fmd_header fmd f >>= \fmd' -> return (fmd',b)
-- or the left side may be a metadata thunk and the right side a sequence of argument thunks
instance (ForestMD fs (ForestFSThunk fs l a),Eq a,MData NoCtx (ForestL fs l) b,ForestMD fs a) => ForestMD fs (ForestFSThunk fs l a,b) where
	isUnevaluatedMDThunk (t,args) = isUnevaluatedMDThunk t
	isUnforcedMDThunk (t,args) = isUnforcedMDThunk t
	get_fmd_header (a,b) = get_fmd_header a
	replace_fmd_header (t,b) f = replace_fmd_header t f >>= \t' -> return (t',b)
-- when load arguments and Adapton thunks are all mixed together
instance (ForestMD fs a) => ForestMD fs ((a,b),c) where
	isUnevaluatedMDThunk ((a,b),c) = isUnevaluatedMDThunk a
	isUnforcedMDThunk ((a,b),c) = isUnforcedMDThunk a
	get_fmd_header ((a,b),c) = get_fmd_header a
	replace_fmd_header ((a,b),c) f = replace_fmd_header a f >>= \a' -> return ((a',b),c)

instance (Typeable a,Eq a,ForestMD fs a,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => ForestMD fs (ForestFSThunk fs Inside a) where
	isUnevaluatedMDThunk t = inside $ isUnevaluatedMDThunk t
	isUnforcedMDThunk t = inside $ isUnforcedFSThunk t
	get_fmd_header t = do
		md <- inside (get t)
		get_fmd_header md
	replace_fmd_header t f = mod $ get t >>= \v -> replace_fmd_header v f
	
-- replaces the content of a stable metadata value with the content of another one
class ICRep fs => StableMD fs md where
	overwriteMD :: md -> ForestI fs md -> ForestO fs ()

instance (Typeable a,ICRep fs,Eq a,ForestInput fs FSThunk Inside) => StableMD fs (ForestFSThunk fs Inside a) where
	overwriteMD t m = overwrite t $ get =<< m
instance (Typeable a,Typeable b,Eq a,Eq b,StableMD fs a,StableMD fs b,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => StableMD fs (a,b) where
	overwriteMD (t1,t2) m = do
		load <- inside $ fsThunk m 
		overwriteMD t1 $ liftM fst $ get load
		overwriteMD t2 $ liftM snd $ get load
instance (Typeable a,Typeable b,Eq a,Eq b,StableMD fs a,StableMD fs b,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => StableMD fs (a :*: b) where
	overwriteMD (t1 :*: t2) m = do
		load <- inside $ fsThunk m
		overwriteMD t1 $ liftM Pure.fstStar $ get load
		overwriteMD t2 $ liftM Pure.sndStar $ get load

cleanForestMD :: (ForestInput fs FSThunk Inside) => ForestI fs (Forest_md fs)
cleanForestMD = do
	err <- mod $ return cleanForestMDErr
	return $ Forest_md { errors = err, fileInfo = Pure.fileInfo_def}
cleanForestMDErr = Forest_err {numErrors = 0, errorMsg = Nothing }

errorForestMD :: (ForestInput fs FSThunk Inside) => ForestI fs (Forest_md fs)
errorForestMD = do
	err <- mod $ return Pure.errorForestMDErr
	return $ Forest_md { errors = err, fileInfo = Pure.errorFileInfo}
missingPathForestMD path = do
	err <- mod $ return $ Pure.missingPathForestErr path
	return $ Forest_md { errors = err, fileInfo = Pure.mkErrorFileInfo path}
missingDirForestMD path = do
	err <- mod $ return $ Pure.missingDirForestErr path
	return $ Forest_md {errors = err, fileInfo = Pure.mkErrorFileInfo path}
fileMatchFailureForestMD path = do
	err <- mod $ return $ Pure.fileMatchFailureForestErr path
	return $ Forest_md {errors = err, fileInfo = Pure.mkErrorFileInfo path}
systemErrorForestMD i = do
	err <- mod $ return $ Pure.systemErrorForestErr i
	return $ Forest_md {errors = err, fileInfo = Pure.errorFileInfo}
notDirectoryForestMD path = do
	err <- mod $ return $ Pure.notDirectoryForestErr path
	return $ Forest_md {errors = err, fileInfo = Pure.mkErrorFileInfo path}
constraintViolationForestMD :: (ForestInput fs FSThunk Inside) => ForestI fs (Forest_md fs)
constraintViolationForestMD = do
	err <- mod $ return Pure.constraintViolationForestErr
	return $ Forest_md {errors = err, fileInfo = Pure.errorFileInfo}
ioExceptionForestMD :: (ForestInput fs FSThunk Inside) => ForestI fs (Forest_md fs)
ioExceptionForestMD = do
	err <- mod $ return Pure.ioExceptionForestErr
	return $ Forest_md { errors = err, fileInfo = Pure.errorFileInfo }

mergeErrors m1 m2 = case (m1,m2) of
            (Nothing,Nothing) -> Nothing
            (Just a, _) -> Just a
            (_, Just b) -> Just b

cleanForestMDwithFile :: (ForestInput fs FSThunk Inside) => FilePath -> ForestI fs (Forest_md fs)
cleanForestMDwithFile path = do
	fmd <- cleanForestMD
	return $ fmd { fileInfo = (fileInfo fmd) { fullpath = path } }

-- | Tests if two metadata values are both valid or invalid
sameValidity :: (ForestLayer fs l,ForestMD fs md1,ForestMD fs md2) => md1 -> md2 -> ForestL fs l Bool
sameValidity md1 md2 = do
	err1 <- get_errors md1
	err2 <- get_errors md2
	return $ (numErrors err1 == 0) == (numErrors err2 == 0)

sameValidity' :: (ForestLayer fs l,ForestMD fs md1) => md1 -> Forest_err -> ForestL fs l Bool
sameValidity' md1 err2 = do
	err1 <- get_errors md1
	return $ (numErrors err1 == 0) == (numErrors err2 == 0)

-- | Tests if two metadata values point to the same filepath
sameFullPath :: (ForestLayer fs l,ForestMD fs md1,ForestMD fs md2) => md1 -> md2 -> ForestL fs l Bool
sameFullPath md1 md2 = do
	fmd1 <- get_fmd_header md1
	fmd2 <- get_fmd_header md2
	return $ fullpath (fileInfo fmd1) == fullpath (fileInfo fmd2)

-- | Tests if two metadata values point to the same canonical filepath, in respect to a given tree
sameCanonicalFullPathInTree :: (ForestLayer fs l,ForestMD fs md1,ForestMD fs md2) => md1 -> md2 -> FSTree fs -> ForestL fs l Bool
sameCanonicalFullPathInTree md1 md2 tree = do
	fmd1 <- get_fmd_header md1
	fmd2 <- get_fmd_header md2
	let path1 = fullpath (fileInfo fmd1)
	let path2 = fullpath (fileInfo fmd2)
	canpath1 <- forestM $ canonalizePathWithTree path1 tree
	canpath2 <- forestM $ canonalizePathWithTree path2 tree
	return $ canpath1 == canpath2

addMultipleMatchesErrorMD :: (ForestMD fs md) => FilePath -> [String] -> md -> ForestO fs ()
addMultipleMatchesErrorMD path names md = modify_errors md $ \olderrors -> do
	let errMsg' = case errorMsg olderrors of
		Nothing -> MultipleMatches path names
		Just e ->  e
	return $ Forest_err { numErrors = numErrors olderrors + 1, errorMsg = Just errMsg' }

addMultipleMatchesErrorMDInside :: (ForestMD fs md) => FilePath -> [String] -> md -> ForestI fs md
addMultipleMatchesErrorMDInside path names md = replace_errors md $ \olderrors -> do
	let errMsg' = case errorMsg olderrors of
		Nothing -> MultipleMatches path names
		Just e ->  e
	return $ Forest_err { numErrors = numErrors olderrors + 1, errorMsg = Just errMsg' }

updateForestMDErrorsWith :: ForestMD fs md => md -> ForestI fs [Forest_err] -> ForestO fs ()
updateForestMDErrorsWith md get_errs = modify_errors md $ \err0 -> get_errs >>= \errs -> return $ Pure.updateForestErr err0 errs

updateForestMDErrorsInsideWith :: ForestMD fs md => md -> ForestI fs [Forest_err] -> ForestI fs md
updateForestMDErrorsInsideWith md get_errs = replace_errors md $ \err0 -> get_errs >>= \errs -> return $ Pure.updateForestErr err0 errs

replaceForestMDErrorsWith :: ForestMD fs md => md -> ForestI fs [Forest_err] -> ForestO fs ()
replaceForestMDErrorsWith md get_errs = overwrite_errors md $ do
	errs <- get_errs
	return $ Pure.mergeMDErrors errs

replaceForestMDErrorsInsideWith :: ForestMD fs md => md -> ForestI fs [Forest_err] -> ForestI fs md
replaceForestMDErrorsInsideWith md get_errs = replace_errors md $ \err0 -> get_errs >>= return . Pure.mergeMDErrors

forestdefault :: (ICRep fs,ForestLayer fs l) => GenericB NoCtx (ForestL fs l)
forestdefault = genericB forestdefault'

genericB :: (forall a. MData NoCtx m a => Proxy NoCtx -> a -> m a) -> GenericB NoCtx m
genericB gen = gen (Proxy::Proxy NoCtx) (error "genericB")

forestdefault' :: ICRep fs => (forall a. (ICRep fs,MData ctx (ForestL fs l) a) => Proxy ctx -> a -> (ForestL fs l) a)
forestdefault' ctx a = ext1B ctx (ext2B ctx (general ctx a
		`extB` char 
		`extB` int
		`extB` integer
		`extB` float 
		`extB` double 
		`extB` coff
		`extB` epochTime
		`extB` fileMode
		`extB` byteString)
--		`extB` (thunkFS a)
		map)
		(list ctx) where
	-- Generic case (does not guarantee termination for recursive types)
	general :: (ICRep fs,MData ctx (ForestL fs l) a) => Proxy ctx -> a -> ForestL fs l a
	general ctx proxy = Data.WithClass.MData.dataTypeOf ctx proxy >>= \d -> Data.WithClass.MData.fromConstrB ctx (forestdefault' ctx (error "general")) (Data.WithClass.MData.indexConstr d 1)
	
	-- Base cases
	char    = return '\NUL'
	int     = return 0      :: Monad m => m Int
	integer = return 0      :: Monad m => m Integer
	float   = return 0.0    :: Monad m => m Float
	double  = return 0.0    :: Monad m => m Double
	coff    = return 0      :: Monad m => m COff
	epochTime = return 0    :: Monad m => m EpochTime
	fileMode = return 0     :: Monad m => m FileMode
	byteString = return B.empty     :: Monad m => m ByteString
	list :: MData ctx m b => Proxy ctx -> m [b]
	list ctx   = return []
	map :: Monad m => m (Map.Map k v)
	map = return Map.empty


instance (ICRep fs,MData ctx m FileInfo,MData ctx m (ForestFSThunkI fs Forest_err),Sat (ctx (Forest_md fs)))
	=> MData ctx m (Forest_md fs) where
	gfoldl ctx k z (Forest_md x1 x2) = z (\mx1 -> return $ \mx2 -> mx1 >>= \x1 -> mx2 >>= \x2 -> return $ Forest_md x1 x2) >>= flip k (return x1) >>= flip k (return x2)
	gunfold ctx k z c = z (\mx1 -> return $ \mx2 -> mx1 >>= \x1 -> mx2 >>= \x2 -> return $ Forest_md x1 x2) >>= k >>= k
	toConstr ctx x@(Forest_md x1 x2) = Data.WithClass.MData.dataTypeOf ctx x >>= return . flip indexConstr 1
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Language.Forest.Pure.MetaData.Forest_md" [mkConstr ty "Forest_md" [] Prefix]

defaultForest_mdWithErrors :: (ICRep fs,ForestLayer fs l) => Forest_err -> ForestL fs l (Forest_md fs)
defaultForest_mdWithErrors err = do
	errors <- inside $ ref err
	info <- forestdefault
	return $ Forest_md errors info

instance Typeable fs => Memo (Forest_md fs) where
	type Key (Forest_md fs) = StableName (Forest_md fs)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo FileInfo where
	type Key FileInfo = StableName FileInfo
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)
	
instance Memo FileType where
	type Key FileType = StableName FileType
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo ErrMsg where
	type Key ErrMsg = StableName ErrMsg
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo CMode where
	type Key CMode = StableName CMode
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo CTime where
	type Key CTime = StableName CTime
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo COff where
	type Key COff = StableName COff
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo Forest_err where
	type Key Forest_err = StableName Forest_err
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance (Memo a,Memo b) => Memo (a :*: b) where
	type Key (a :*: b) = (Key a,Key b)
	{-# INLINE memoKey #-}
	memoKey (x :*: y) = (andMkWeak w1 w2,(k1,k2))
		where (w1,k1) = memoKey x
		      (w2,k2) = memoKey y


getForestMDInTree :: (ICRep fs,ForestLayer fs l) => FilePath -> FSTree fs -> ForestL fs l (Forest_md fs)
getForestMDInTree path tree = forestM (pathInTree path tree) >>= getForestMD path

getRelForestMDInTree :: (ICRep fs,ForestLayer fs l) => FilePath -> FSTree fs -> FilePath -> ForestL fs l (Forest_md fs)
getRelForestMDInTree path tree file = getForestMDInTree (path </> file) tree


mergeJustDefault :: (ICRep fs,ForestLayer fs l,MData NoCtx (ForestL fs l) a,MData NoCtx (ForestL fs l) b) => (Maybe a,Maybe b) -> ForestL fs l (a,b)
mergeJustDefault (Just x,Just y) = return (x,y)
mergeJustDefault (Just x,Nothing) = liftM (x,) forestdefault
mergeJustDefault (Nothing,Just y) = liftM (,y) forestdefault
mergeJustDefault (Nothing,Nothing) = do
	x <- forestdefault
	y <- forestdefault
	return (x,y)

$( derive makeDeepTypeable ''(:*:) )
$( derive makeMData ''(:*:) )


