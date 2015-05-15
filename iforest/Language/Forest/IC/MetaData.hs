{-# LANGUAGE OverlappingInstances, TupleSections, StandaloneDeriving, DataKinds, TypeFamilies, TypeOperators, UndecidableInstances, ConstraintKinds, FlexibleContexts, MultiParamTypeClasses, RankNTypes, NamedFieldPuns, RecordWildCards, FlexibleInstances, DeriveDataTypeable, TemplateHaskell,ScopedTypeVariables, DoAndIfThenElse,
    TypeSynonymInstances #-}

module Language.Forest.IC.MetaData where

import qualified Data.List as List 
import System.Mem.Weak.Exts
import Language.Forest.FS.FSRep
import Language.Forest.IC.ICRep
import Data.DeepTypeable
import Control.Exception as Exception
import System.Posix.Files
import Language.Pads.Padsc as Pads hiding (ErrMsg,numErrors)
import Data.Memo
import qualified Data.Set as Set
import System.Posix.User
import System.Posix.Types
import System.FilePath.Posix
import Language.Forest.IC.PadsInstances
import System.Process
import GHC.IO.Handle
import Foreign.C.Types
import Language.Forest.Errors
import Control.Monad.Incremental as Inc
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
import Data.Derive.Memo
import Data.WithClass.Derive.MData
import Data.WithClass.MData
import Data.Int
import Data.Word
import Data.Map (Map(..))
import qualified Data.Map as Map
import qualified Data.ByteString as B
import Control.Monad
import Data.WithClass.MGenerics
import System.Mem.MemoTable
import System.Mem.StableName

import System.IO.Unsafe
import System.Mem.Weak

import Language.Haskell.TH.Syntax
import Data.WithClass.MData
import Data.DeriveTH
import Data.WithClass.Derive.MData
import Data.WithClass.Derive.DeepTypeable
import Data.DeepTypeable

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
       deriving (Eq, Typeable, Ord,Show)
data FileType = UnknownK | AsciiK | BinaryK | DirectoryK 
 deriving (Eq, Typeable, Ord,Show)


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

fileStatusToKind :: FilePath -> FileStatus -> IO FileType
fileStatusToKind path fs = do
    if isRegularFile fs then 
      do { ascii <- isAscii path
         ; if ascii then return AsciiK else return BinaryK
         }
    else if isDirectory fs then return DirectoryK
    else return UnknownK

-- * Forest metadata lifted for IC, where errors have explicit thunks

type GetForestMD fs = (FilePath -> FSTree fs -> ForestI fs (Forest_md fs))

data Forest_md fs = Forest_md { errors :: ForestFSThunkI fs Forest_err, fileInfo :: ForestFSThunkI fs FileInfo } deriving (Typeable)

deriving instance (ICRep fs,ForestInput fs FSThunk Inside) => Eq (Forest_md fs)
deriving instance (ICRep fs,ForestInput fs FSThunk Inside) => Ord (Forest_md fs)

instance (DeepTypeable fs) => DeepTypeable (Forest_md fs) where
	typeTree (_::Proxy (Forest_md fs)) = MkTypeTree (mkName "Language.Forest.FS.ICRep.Forest_md") [typeTree (Proxy::Proxy fs)] [MkConTree (mkName "Language.Forest.FS.ICRep.Forest_md") [typeTree (Proxy::Proxy (ForestFSThunkI fs Forest_err)),typeTree (Proxy::Proxy (ForestFSThunkI fs FileInfo))]]

{- Should raise no exceptions -}
-- it loads the metadata from a physical path on disk, but returns the metadata as if it belonged to an original path alias
getForestMD :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ICRep fs,ForestLayer fs l,ForestInput fs FSThunk Inside) => FilePath -> FilePath -> ForestL fs l (Forest_md fs)
getForestMD oripath diskpath = do
	
	info <- inside $ fsThunk $ do
		abspath <- forestM $ forestIO $ absolutePath oripath
		i <- forestM $ forestIO $ flip Exception.catch (\(e::Exception.SomeException) -> return $ mkErrorFileInfo abspath) $ do
			fd <- getFileStatus diskpath
			let file_ownerID = fileOwner fd
			ownerEntry    <- getUserEntryForID file_ownerID
			let file_groupID = fileGroup fd
			groupEntry <- getGroupEntryForID file_groupID
			fdsym <- getSymbolicLinkStatus diskpath
			symLinkOpt <- if isSymbolicLink fdsym then readOptSymLink diskpath else return Nothing
			readTime <- epochTime
			knd <- fileStatusToKind diskpath fd
			return $ FileInfo { fullpath = abspath, owner = userName ownerEntry, group = groupName groupEntry, size  = fileSize fd, access_time = accessTime fd, mod_time = modificationTime fd, read_time = readTime, mode     =  fileMode fd, symLink =symLinkOpt, kind  = knd }
		return $ i { fullpath = abspath }
			
	err <- inside $ fsThunk $ do
		i <- Inc.get info
		return $ if (size i >= 0) then Forest_err { numErrors = 0, errorMsg = Nothing } else Forest_err { numErrors = 1, errorMsg = Just (ForestIOException "") }
	return $ Forest_md err info


-- for cases where we want to avoid reading from the filesystme
doesExistInMD :: (ICRep fs,IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ForestInput fs FSThunk Inside,ForestLayer fs l) => Forest_md fs -> ForestL fs l Bool
doesExistInMD fmd = do
	info <- get_info fmd
	return (access_time info >= 0 || read_time info >= 0)

doesFileExistInMD :: (ICRep fs,IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ForestInput fs FSThunk Inside,ForestLayer fs l) => Forest_md fs -> ForestL fs l Bool
doesFileExistInMD fmd = do
	b <- doesExistInMD fmd
	info <- get_info fmd
	return $ b && kind info /= DirectoryK

doesLinkExistInMD :: (ICRep fs,IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ForestInput fs FSThunk Inside,ForestLayer fs l) => Forest_md fs -> ForestL fs l (Maybe FilePath)
doesLinkExistInMD fmd = do
	b <- doesExistInMD fmd
	info <- get_info fmd
	return $ if (b && kind info /= DirectoryK) then symLink info else Nothing

doesDirectoryExistInMD :: (ICRep fs,IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ForestInput fs FSThunk Inside,ForestLayer fs l) => Forest_md fs -> ForestL fs l Bool
doesDirectoryExistInMD fmd = do
	b <- doesExistInMD fmd
	info <- get_info fmd
	return $ b && kind info == DirectoryK

forest_err_def :: Forest_err
forest_err_def = Forest_err 1 Nothing
forest_md_def :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ForestInput fs FSThunk Inside,ForestLayer fs l) => ForestL fs l (Forest_md fs)
forest_md_def = do
	err <- inside $ ref $ forest_err_def
	info <- inside $ ref $ fileInfo_def
	return $ Forest_md err info
	
fmd_fullpath :: (IncK (IncForest fs) FileInfo,ForestInput fs FSThunk Inside,ForestLayer fs l) => Forest_md fs -> ForestL fs l FilePath
fmd_fullpath md = liftM fullpath $ inside $ Inc.get $ fileInfo md

fmd_kind :: (IncK (IncForest fs) FileInfo,ForestInput fs FSThunk Inside,ForestLayer fs l) => Forest_md fs -> ForestL fs l FileType
fmd_kind md = liftM kind $ inside $ Inc.get $ fileInfo md

fmd_symLink :: (IncK (IncForest fs) FileInfo,ForestInput fs FSThunk Inside,ForestLayer fs l) => Forest_md fs -> ForestL fs l (Maybe FilePath)
fmd_symLink md = liftM symLink $ inside $ Inc.get $ fileInfo md

-- we need to actually create new thunks to allow the modification to occur at the inner layer
modify_errors_under :: (IncK (IncForest fs) md,ForestMD fs md) => ForestFSThunk fs Inside md -> (Forest_err -> ForestI fs Forest_err) -> ForestO fs ()
modify_errors_under t newerrs = modify t $ \md -> replace_errors md newerrs

{- Meta data type class -}
class (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ICRep fs,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => ForestMD fs md where
	isUnevaluatedMDThunk :: ForestLayer fs l => md -> ForestL fs l Bool
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
	
	get_info :: ForestLayer fs l => md -> ForestL fs l FileInfo
	get_info md = inside . Inc.get . fileInfo =<< get_fmd_header md
	get_fullpath :: ForestLayer fs l => md -> ForestL fs l String
	get_fullpath md = return . fullpath =<< inside . Inc.get =<< return . fileInfo =<< get_fmd_header md
	get_owner :: ForestLayer fs l => md -> ForestL fs l String
	get_owner md = return . owner =<< inside . Inc.get =<< return .  fileInfo =<< get_fmd_header md
	get_group :: ForestLayer fs l => md -> ForestL fs l String
	get_group md = return . group =<< inside . Inc.get =<< return . fileInfo =<< get_fmd_header md
	get_size :: ForestLayer fs l => md -> ForestL fs l COff
	get_size md = return . size =<< inside . Inc.get =<< return . fileInfo =<< get_fmd_header md
	get_access_time :: ForestLayer fs l => md -> ForestL fs l EpochTime
	get_access_time md = return . access_time =<< inside . Inc.get =<< return . fileInfo =<< get_fmd_header md
	get_mod_time :: ForestLayer fs l => md -> ForestL fs l EpochTime
	get_mod_time md = return . mod_time =<< inside . Inc.get =<< return . fileInfo =<< get_fmd_header md
	get_read_time :: ForestLayer fs l => md -> ForestL fs l EpochTime
	get_read_time md = return . read_time =<< inside . Inc.get =<< return . fileInfo =<< get_fmd_header md
	get_mode :: ForestLayer fs l => md -> ForestL fs l FileMode
	get_mode md = return . mode =<< inside . Inc.get =<< return . fileInfo =<< get_fmd_header md
	get_modes :: ForestLayer fs l => md -> ForestL fs l String
	get_modes md = return . modeToModeString . mode =<< inside . Inc.get =<< return . fileInfo =<< get_fmd_header md
	get_symLink :: ForestLayer fs l => md -> ForestL fs l (Maybe FilePath)
	get_symLink md = return . symLink =<< inside . Inc.get =<< return . fileInfo =<< get_fmd_header md
	get_kind :: ForestLayer fs l => md -> ForestL fs l FileType
	get_kind md = return . kind =<< inside . Inc.get =<< return . fileInfo =<< get_fmd_header md

change_fmd_header :: (IncK (IncForest fs) (Forest_md fs, md),ICRep fs,ForestInput fs FSThunk Inside,ForestInput fs FSThunk l) => ForestFSThunk fs l (Forest_md fs,md) -> Forest_md fs -> ForestO fs ()
change_fmd_header t fmd' = modify t $ \(fmd,md) -> return (fmd',md)

instance (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ICRep fs,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => ForestMD fs (Forest_md fs) where
	isUnevaluatedMDThunk fmd = return False
	get_fmd_header b = return b
	replace_fmd_header fmd f = f fmd

-- the left side may be a @Forest_md@ and the right side a metadata value
instance (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ICRep fs,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => ForestMD fs (Forest_md fs,b) where
	get_fmd_header (a,b) = get_fmd_header a
	replace_fmd_header (fmd,b) f = replace_fmd_header fmd f >>= \fmd' -> return (fmd',b)
instance (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ICRep fs,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => ForestMD fs ((Forest_md fs,bmd),b) where
	get_fmd_header ((a,bmd),b) = get_fmd_header a
	replace_fmd_header ((fmd,bmd),b) f = replace_fmd_header fmd f >>= \fmd' -> return ((fmd',bmd),b)
-- or the left side may be a metadata thunk and the right side a sequence of argument thunks
instance (ForestMD fs (ForestFSThunk fs l a),Eq a,MData NoCtx (ForestL fs l) b,ForestMD fs a) => ForestMD fs (ForestFSThunk fs l a,b) where
	isUnevaluatedMDThunk (t,args) = isUnevaluatedMDThunk t
	get_fmd_header (a,b) = get_fmd_header a
	replace_fmd_header (t,b) f = replace_fmd_header t f >>= \t' -> return (t',b)
-- when load arguments and Adapton thunks are all mixed together
instance (ForestMD fs a) => ForestMD fs ((a,b),c) where
	isUnevaluatedMDThunk ((a,b),c) = isUnevaluatedMDThunk a
	get_fmd_header ((a,b),c) = get_fmd_header a
	replace_fmd_header ((a,b),c) f = replace_fmd_header a f >>= \a' -> return ((a',b),c)

instance (IncK (IncForest fs) a,ForestMD fs a,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => ForestMD fs (ForestFSThunk fs Inside a) where
	isUnevaluatedMDThunk t = inside $ isUnevaluatedFSThunk t
	get_fmd_header t = do
		md <- inside (get t)
		get_fmd_header md
	replace_fmd_header t f = mod $ get t >>= \v -> replace_fmd_header v f

instance (ForestRep fs rep,ForestMD fs (ForestFSThunkI fs (ForestRepTy fs rep))) => ForestMD fs rep where
	isUnevaluatedMDThunk = isUnevaluatedMDThunkRep Proxy
	get_fmd_header = get_fmd_headerRep Proxy
	replace_fmd_header r (f :: Forest_md fs -> ForestI fs (Forest_md fs)) = do
		let fs = Proxy :: Proxy fs
		liftM (from $ iso_rep_thunk fs) $ replace_fmd_header (to (iso_rep_thunk fs) r) f

isUnevaluatedMDThunkRep :: (ForestLayer fs l,ForestRep fs rep,ForestMD fs (ForestFSThunkI fs (ForestRepTy fs rep))) => Proxy fs -> rep -> ForestL fs l Bool
isUnevaluatedMDThunkRep fs r = isUnevaluatedMDThunk (to (iso_rep_thunk fs) r)
get_fmd_headerRep :: (ForestLayer fs l,ForestRep fs rep,ForestMD fs (ForestFSThunkI fs (ForestRepTy fs rep))) => Proxy fs -> rep -> ForestL fs l (Forest_md fs)
get_fmd_headerRep fs r = get_fmd_header (to (iso_rep_thunk fs) r)

-- we provide these instances just to get errors...
instance (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ForestMD fs rep) => ForestMD fs (Maybe rep) where
	get_fmd_header Nothing = inside cleanForestMD
	get_fmd_header (Just rep) = get_fmd_header rep
instance (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ICRep fs) => ForestMD fs (ForestFSThunkI fs Forest_err,b) where
	get_fmd_header (err_t,_) = return $ Forest_md err_t (error "noFileInfo")
instance (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ICRep fs) => ForestMD fs (FileInfo,b) where
	get_fmd_header (info,_) = inside (ref info) >>= return . Forest_md (error "noForest_err")
instance (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ICRep fs) => ForestMD fs ((FileInfo,bmd),b) where
	get_fmd_header ((info,_),_) = inside (ref info) >>= return . Forest_md (error "noForest_err")

-- replaces the content of a stable metadata value with the content of another one
class ICRep fs => StableMD fs md where
	overwriteMD :: md -> ForestI fs md -> ForestO fs ()

instance (IncK (IncForest fs) a,ICRep fs,ForestInput fs FSThunk Inside) => StableMD fs (ForestFSThunk fs Inside a) where
	overwriteMD t m = overwrite t $ get =<< m
instance (IncK (IncForest fs) (a,b),StableMD fs a,StableMD fs b,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => StableMD fs (a,b) where
	overwriteMD (t1,t2) m = do
		load <- inside $ fsThunk m 
		overwriteMD t1 $ liftM fst $ get load
		overwriteMD t2 $ liftM snd $ get load
instance (IncK (IncForest fs) (a :*: b),StableMD fs a,StableMD fs b,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => StableMD fs (a :*: b) where
	overwriteMD (t1 :*: t2) m = do
		load <- inside $ fsThunk m
		overwriteMD t1 $ liftM fstStar $ get load
		overwriteMD t2 $ liftM sndStar $ get load

instance (ForestRep fs a,StableMD fs (ForestFSThunkI fs (ForestRepTy fs a))) => StableMD fs a where
	overwriteMD r (m :: ForestI fs md) = do
		let fs = Proxy :: Proxy fs
		overwriteMD (to (iso_rep_thunk fs) r) (liftM (to (iso_rep_thunk fs)) m)

mergeForestErrs :: Forest_err -> Forest_err -> Forest_err
mergeForestErrs err1 err2 = Forest_err { numErrors = numErrors err1 + numErrors err2, errorMsg = mergeErrors (errorMsg err1) (errorMsg err2) }

mergeMDErrors :: [Forest_err] -> Forest_err
mergeMDErrors = List.foldl' mergeForestErrs cleanForestMDErr

cleanForestMD :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ForestInput fs FSThunk Inside) => ForestI fs (Forest_md fs)
cleanForestMD = do
	err <- ref $ cleanForestMDErr
	info <- ref $ fileInfo_def
	return $ Forest_md err info
cleanForestMDErr = Forest_err {numErrors = 0, errorMsg = Nothing }

errorForestMD :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ForestInput fs FSThunk Inside) => ForestI fs (Forest_md fs)
errorForestMD = do
	err <- ref $ errorForestMDErr
	info <- ref $ errorFileInfo
	return $ Forest_md { errors = err, fileInfo = info }
missingPathForestMD path = do
	err <- ref $ missingPathForestErr path
	info <- ref $ mkErrorFileInfo path
	return $ Forest_md { errors = err, fileInfo = info }
missingDirForestMD path = do
	err <- ref $ missingDirForestErr path
	info <- ref $ mkErrorFileInfo path
	return $ Forest_md {errors = err, fileInfo = info }
fileMatchFailureForestMD path = do
	err <- ref $ fileMatchFailureForestErr path
	info <- ref $ mkErrorFileInfo path
	return $ Forest_md {errors = err, fileInfo = info }
systemErrorForestMD i = do
	err <- ref $ systemErrorForestErr i
	info <- ref $ errorFileInfo
	return $ Forest_md {errors = err, fileInfo = info }
notDirectoryForestMD path = do
	err <- ref $ notDirectoryForestErr path
	info <- ref $ mkErrorFileInfo path
	return $ Forest_md {errors = err, fileInfo = info }
constraintViolationForestMD :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ForestInput fs FSThunk Inside) => ForestI fs (Forest_md fs)
constraintViolationForestMD = do
	err <- ref $ constraintViolationForestErr
	info <- ref $ errorFileInfo
	return $ Forest_md {errors = err, fileInfo = info }
ioExceptionForestMD :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ForestInput fs FSThunk Inside) => ForestI fs (Forest_md fs)
ioExceptionForestMD = do
	err <- ref $ ioExceptionForestErr
	info <- ref $ errorFileInfo
	return $ Forest_md { errors = err, fileInfo = info }
ioExceptionForestMDwithFile :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ForestInput fs FSThunk Inside) => FilePath -> ForestI fs (Forest_md fs)
ioExceptionForestMDwithFile path = do
	err <- ref $ ioExceptionForestErr
	info <- ref $ mkErrorFileInfo path
	return $ Forest_md { errors = err, fileInfo = info }

mergeErrors m1 m2 = case (m1,m2) of
            (Nothing,Nothing) -> Nothing
            (Just a, _) -> Just a
            (_, Just b) -> Just b

cleanForestMDwithFile :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ICRep fs,ForestLayer fs l,ForestInput fs FSThunk Inside) => FilePath -> ForestL fs l (Forest_md fs)
cleanForestMDwithFile path = inside $ do
	abspath <- forestM $ forestIO $ absolutePath path
	err <- ref $ cleanForestMDErr
	info <- ref $ fileInfo_def { fullpath = abspath }
	return $ Forest_md err info

fileInfo_def = FileInfo { fullpath = ""
                        , owner = ""
                        , group = ""
                        , size = 0
                        , access_time = 0
                        , read_time = 0
                        , mod_time = 0
                        , mode = 0
                        , symLink = Nothing
                        , kind = UnknownK       
                        }

cleanSymLinkForestMDwithFile :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ICRep fs,ForestLayer fs l,ForestInput fs FSThunk Inside) => FilePath -> FilePath -> ForestL fs l (Forest_md fs)
cleanSymLinkForestMDwithFile path tgt = inside $ do
	abspath <- forestM $ forestIO $ absolutePath path
	err <- ref $ cleanForestMDErr
	info <- ref $ fileInfo_def { fullpath = abspath, symLink = Just tgt }
	return $ Forest_md err info

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
	info1 <- get_info md1
	info2 <- get_info md2
	return $ fullpath info1 == fullpath info2

-- | Tests if two metadata values point to the same canonical filepath, in respect to a given tree
sameCanonicalFullPathInTree :: (ICRep fs) => FilePath -> FilePath -> FSTree fs -> ForestM fs Bool
sameCanonicalFullPathInTree path1 path2 tree = do
	canpath1 <- canonalizePathInTree path1 tree
	canpath2 <- canonalizePathInTree path2 tree
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
updateForestMDErrorsWith md get_errs = modify_errors md $ \err0 -> get_errs >>= \errs -> return $ updateForestErr err0 errs

updateForestMDErrorsWithPadsMD :: (PadsMD pads_md,ForestMD fs md) => md -> ForestI fs pads_md -> ForestO fs ()
updateForestMDErrorsWithPadsMD md get_errs = modify_errors md $ \err0 -> get_errs >>= \errs -> return $ updateForestErr err0 [padsError $ get_md_header errs]

replaceForestMDErrorsWithPadsMD :: (PadsMD pads_md,ForestMD fs md) => md -> ForestI fs pads_md -> ForestO fs ()
replaceForestMDErrorsWithPadsMD md get_errs = modify_errors md $ \err0 -> get_errs >>= \errs -> return $ padsError $ get_md_header errs

updateForestMDErrorsInsideWith :: ForestMD fs md => md -> ForestI fs [Forest_err] -> ForestI fs md
updateForestMDErrorsInsideWith md get_errs = replace_errors md $ \err0 -> get_errs >>= \errs -> return $ updateForestErr err0 errs

updateForestErr :: Forest_err -> [Forest_err] -> Forest_err
updateForestErr err errs = List.foldl' mergeForestErrs err errs

updateForestMDErrorsInsideWithPadsMD :: (PadsMD pads_md,ForestMD fs md) => md -> ForestI fs pads_md -> ForestI fs md
updateForestMDErrorsInsideWithPadsMD md get_errs = replace_errors md $ \err0 -> get_errs >>= \errs -> return $ updateForestErr err0 [padsError $ get_md_header errs]

replaceForestMDErrorsWith :: (IncK (IncForest fs) Forest_err,ForestMD fs md) => md -> ForestI fs [Forest_err] -> ForestO fs ()
replaceForestMDErrorsWith md get_errs = overwrite_errors md $ do
	errs <- get_errs
	return $ mergeMDErrors errs

replaceForestMDErrorsInsideWith :: ForestMD fs md => md -> ForestI fs [Forest_err] -> ForestI fs md
replaceForestMDErrorsInsideWith md get_errs = replace_errors md $ \err0 -> get_errs >>= return . mergeMDErrors

instance (MData ctx m (ForestFSThunkI fs FileInfo),ICRep fs,MData ctx m FileInfo,MData ctx m (ForestFSThunkI fs Forest_err),Sat (ctx (Forest_md fs)))
	=> MData ctx m (Forest_md fs) where
	gfoldl ctx k z (Forest_md x1 x2) = z (\mx1 -> return $ \mx2 -> mx1 >>= \x1 -> mx2 >>= \x2 -> return $ Forest_md x1 x2) >>= flip k (return x1) >>= flip k (return x2)
	gunfold ctx k z c = z (\mx1 -> return $ \mx2 -> mx1 >>= \x1 -> mx2 >>= \x2 -> return $ Forest_md x1 x2) >>= k >>= k
	toConstr ctx x@(Forest_md x1 x2) = Data.WithClass.MData.dataTypeOf ctx x >>= return . flip indexConstr 1
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Language.Forest.MetaData.Forest_md" [mkConstr ty "Forest_md" [] Prefix]

instance Typeable fs => Memo (Forest_md fs) where
	type Key (Forest_md fs) = StableName (Forest_md fs)
	{-# INLINE memoKey #-}
	memoKey x = (unsafePerformIO $ makeStableName x)
	memoWeak x = MkWeak $ mkWeak x

instance (Memo a,Memo b) => Memo (a :*: b) where
	type Key (a :*: b) = (Key a,Key b)
	{-# INLINE memoKey #-}
	memoKey (x :*: y) = (memoKey x,memoKey y)
	{-# INLINE memoWeak #-}
	memoWeak (x :*: y) = memoWeak x `andMkWeak` memoWeak y

errorForestMDErr = Forest_err {numErrors = 1, errorMsg = Nothing }
missingPathForestErr path = Forest_err { numErrors = 1, errorMsg = Just (MissingFile path) }
wrongFileExtensionForestErr ext path = Forest_err { numErrors = 1, errorMsg = Just (WrongFileExtension ext path) }
missingDirForestErr path = Forest_err { numErrors = 1, errorMsg = Just (MissingDirectory path) }
fileMatchFailureForestErr path = Forest_err { numErrors = 1, errorMsg = Just (MatchFailure path) }
systemErrorForestErr i = Forest_err { numErrors = 1, errorMsg = Just (SystemError i) }
notDirectoryForestErr path = Forest_err { numErrors = 1, errorMsg = Just (NotADirectory path) }
constraintViolationForestErr = Forest_err { numErrors = 1, errorMsg = Just ConstraintViolation }
ioExceptionForestErr = Forest_err { numErrors = 1, errorMsg = Just (ForestIOException $ "not a symbolic link") }

specialStringToMode special = case special of
	'-' -> [regularFileMode]
	'd' -> [directoryMode]
	'b' -> [blockSpecialMode]
	'c' -> [characterSpecialMode]
	'l' -> [symbolicLinkMode]
	'p' -> [namedPipeMode]
	's' -> [socketMode]
	'?' -> []

modeStringToMode' :: String -> [FileMode]
modeStringToMode' [special,or,ow,ox,gr,gw,gx,otr,otw,otx] = 
                  specialM ++ ownerrM ++ ownerwM ++ ownerxM ++ grouprM ++ groupwM ++ groupxM  ++ otherrM ++ otherwM ++ otherxM
  where  specialM = specialStringToMode special
         ownerrM  = if or == 'r' then [ownerReadMode] else []
         ownerwM  = if ow == 'w' then [ownerWriteMode] else []
         ownerxM  = if ox == 'x' then [ownerExecuteMode] else []
         grouprM  = if gr == 'r' then [groupReadMode] else []
         groupwM  = if gw == 'w' then [groupWriteMode] else []
         groupxM  = if gx == 'x' then [groupExecuteMode] else []
         otherrM  = if otr == 'r' then [otherReadMode] else []
         otherwM  = if otw == 'w' then [otherWriteMode] else []
         otherxM  = if otx == 'x' then [otherExecuteMode] else []

unionFileModeL modes = foldl1 unionFileModes modes


modeStringToMode :: String -> FileMode
modeStringToMode s = unionFileModeL $ modeStringToMode' s

modeToModeString :: FileMode -> String
modeToModeString mode = special ++ ownerr ++ ownerw ++ ownerx ++ groupr ++ groupw ++ groupx ++ otherr ++ otherw ++ otherx
   where special = if intersectFileModes mode regularFileMode == regularFileMode then "-"
                   else if intersectFileModes mode directoryMode == directoryMode then "d"
                   else if intersectFileModes mode blockSpecialMode == blockSpecialMode then "b"
                   else if intersectFileModes mode characterSpecialMode == characterSpecialMode then "c"
                   else if intersectFileModes mode symbolicLinkMode == symbolicLinkMode then "l"
                   else if intersectFileModes mode namedPipeMode == namedPipeMode then "p"
                   else if intersectFileModes mode socketMode == socketMode then "s"
                   else "?"
         ownerr = if intersectFileModes mode ownerReadMode == ownerReadMode then "r" else "-"
         ownerw = if intersectFileModes mode ownerWriteMode == ownerWriteMode then "w" else "-"
         ownerx = if intersectFileModes mode ownerExecuteMode == ownerExecuteMode then "x" else "-"
         groupr = if intersectFileModes mode groupReadMode == groupReadMode then "r" else "-"
         groupw = if intersectFileModes mode groupWriteMode == groupWriteMode then "w" else "-"
         groupx = if intersectFileModes mode groupExecuteMode == groupExecuteMode then "x" else "-"
         otherr = if intersectFileModes mode otherReadMode == otherReadMode then "r" else "-"
         otherw = if intersectFileModes mode otherWriteMode == otherWriteMode then "w" else "-"
         otherx = if intersectFileModes mode otherExecuteMode == otherExecuteMode then "x" else "-"

pl (c1, c2) = case (c1,c2) of
  (c1, '-') -> True
  (c1,c2) | c1 == c2 -> True
  otherwise -> False

(<==) s1 s2 = and (map pl (zip s1 s2))

data Permission = Read | Write | Execute
  deriving (Eq, Ord, Show, Typeable, Data)

ownModeToPermission :: FileMode -> [Permission]
ownModeToPermission mode = ownerr ++ ownerw ++ ownerx
   where ownerr = if intersectFileModes mode ownerReadMode == ownerReadMode then [Read] else []
         ownerw = if intersectFileModes mode ownerWriteMode == ownerWriteMode then [Write] else []
         ownerx = if intersectFileModes mode ownerExecuteMode == ownerExecuteMode then [Execute] else []

grpModeToPermission :: FileMode -> [Permission]
grpModeToPermission mode = groupr ++ groupw ++ groupx
   where
     groupr = if intersectFileModes mode groupReadMode == groupReadMode then [Read] else []
     groupw = if intersectFileModes mode groupWriteMode == groupWriteMode then [Write] else []
     groupx = if intersectFileModes mode groupExecuteMode == groupExecuteMode then [Execute] else []

othModeToPermission :: FileMode -> [Permission]
othModeToPermission mode = otherr ++ otherw ++ otherx
   where
     otherr = if intersectFileModes mode otherReadMode == otherReadMode then [Read] else []
     otherw = if intersectFileModes mode otherWriteMode == otherWriteMode then [Write] else []
     otherx = if intersectFileModes mode otherExecuteMode == otherExecuteMode then [Execute] else []

getPermissionsForGroups :: [String] -> FileInfo -> [Permission]
getPermissionsForGroups groups fInfo = 
  if (group fInfo) `elem` groups 
    then grpModeToPermission (mode fInfo)
    else []

getPermissionsForOwner :: String -> FileInfo -> [Permission]
getPermissionsForOwner id fInfo = 
  if (owner fInfo) == id
    then ownModeToPermission (mode fInfo)
    else []

getPermissionsForOther :: FileInfo -> [Permission]
getPermissionsForOther  fInfo =  othModeToPermission (mode fInfo)

getPermissions :: String -> [String] -> FileInfo -> [Permission] 
getPermissions id groups fInfo = List.nub (owner++group++other)
  where owner = getPermissionsForOwner id fInfo
        group = getPermissionsForGroups groups fInfo
        other = getPermissionsForOther fInfo

getGroups :: String -> IO [String]
getGroups id = do
	groupEntries <- getAllGroupEntries 
	let id_groups = filter (\g -> id `elem` (groupMembers g)) groupEntries
	return (List.map groupName id_groups)

errorFileInfo :: FileInfo
errorFileInfo  = mkErrorFileInfo ""

predForestErr :: ForestLayer fs l => ForestI fs Bool -> ForestL fs l (Forest_err)
predForestErr m = do
	cond <- inside m
	if cond then return cleanForestMDErr else return constraintViolationForestErr
	

getForestMDInTree :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ICRep fs,ForestLayer fs l) => FilePath -> FSTree fs -> ForestL fs l (Forest_md fs)
getForestMDInTree path tree = forestM (pathInTree path tree) >>= getForestMD path

getRelForestMDInTree :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ICRep fs,ForestLayer fs l) => FilePath -> FSTree fs -> FilePath -> ForestL fs l (Forest_md fs)
getRelForestMDInTree path tree file = getForestMDInTree (path </> file) tree

$( derive makeDeepTypeable ''(:*:) )
$( derive makeMData ''(:*:) )

-- forest errors kind
data EC = E -- errors
		| C -- content
		deriving Typeable

deriving instance Typeable E
deriving instance Typeable C
$( derive makeDeepTypeable ''EC )

instance DeepTypeable E where
	typeTree (_::Proxy E) = MkTypeTree (mkName "Language.Forest.IC.Generic.E") [] []
instance DeepTypeable C where
	typeTree (_::Proxy C) = MkTypeTree (mkName "Language.Forest.IC.Generic.C") [] []


type family ECMd (ec :: EC) (fs :: FS) (a :: *) where
	ECMd E fs a = (Forest_md fs,a)
	ECMd C fs a = (FileInfo,a)
type family ECErr (ec :: EC) (fs :: FS) (a :: *) where
	ECErr E fs a = (ForestFSThunkI fs Forest_err,a)
	ECErr C fs a = a

cleanFileInfo path = fileInfo_def { fullpath = path }
cleanSymLinkFileInfo path tgt = fileInfo_def { fullpath = path, symLink = Just tgt }

$(deriveMemo ''FileInfo)
$(deriveMemo ''FileType)
$(deriveMemo ''ErrMsg)
$(deriveMemo ''CMode)
$(deriveMemo ''CTime)
$(deriveMemo ''COff)
$(deriveMemo ''Forest_err)

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