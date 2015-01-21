{-# LANGUAGE OverlappingInstances, TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}



module Language.Forest.IC.IO.ZLoading where

import Language.Forest.IC.PadsInstances
import Prelude hiding (const,read,mod)
import qualified Prelude
import Language.Forest.IO.Utils
import Language.Forest.IO.Shell
import Language.Forest.Syntax
import Language.Forest.IC.FS.Diff
import Language.Forest.IC.ValueDelta
import Language.Forest.Pure.MetaData (FileInfo(..),FileType(..))
import qualified Language.Forest.Pure.MetaData as Pure
import Language.Pads.Padsc
import Language.Forest.IC.MetaData
import Language.Forest.IC.Generic
import Language.Forest.Errors
import Language.Forest.IC.FS.FSDelta
import Data.WithClass.MGenerics.Twins
import Control.Monad.Incremental as Inc hiding (memo)
--import Language.Forest.ListDiff
import Data.IORef

import qualified System.FilePath.Posix
import System.FilePath.Glob
import System.Posix.Env
import System.Posix.Files
import System.Process
import System.Exit
import System.Directory
import System.IO
import System.IO.Unsafe
import Text.Regex
import System.FilePath.Posix
import Control.Monad
import Data.List
import Language.Forest.FS.FSRep
import Language.Forest.IC.ICRep
--import Control.Monad.IO.Class
import Data.WithClass.MData

import Language.Forest.IC.IO.Memo

import qualified Control.Exception as CE

import Data.Data
import Data.Maybe
import System.Random
import Data.Proxy

-- | lazy file loading
-- XXX: Pads specs currently accept a single optional argument and have no incremental loading, so a change in the argument's value requires recomputation
-- Pads errors contribute to the Forest error count
doZLoadFile1 :: (ICRep fs,ZippedICMemo fs,MData NoCtx (Inside (IncForest fs) IORef IO) arg,Typeable arg,Eq arg,Eq pads,Eq md,MData NoCtx (ForestI fs) pads,MData NoCtx (ForestI fs) md,FSRep fs,Pads1 arg pads md) =>
	Proxy pads -> Pure.Arg arg -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs (ForestFSThunkI fs (Forest_md fs,(pads,md)))
doZLoadFile1 (repProxy :: Proxy pads) (Pure.Arg arg :: Pure.Arg arg) oldpath_f path (tree :: FSTree fs) getMD = debug ("doLoadFile1 " ++ show path) $ do
	let argProxy = Proxy :: Proxy (Pure.Arg arg)
	let fsrepProxy = Proxy -- :: Proxy (ForestFSThunkI fs pads)
	let fs = (Proxy::Proxy fs)
	-- default static loading
	let load_file = do
		rep_thunk <- checkZPath path tree $ do
			rep@(pads,md) <- forestM $ pathInTree path tree >>= forestIO . parseFile1 arg
			fmd <- getMD path tree
			fmd' <- updateForestMDErrorsInsideWithPadsMD fmd (return md) -- adds the Pads errors
			return (fmd',rep)
		return rep_thunk
	-- memoized reuse
	let reuse_same_file old_rep_thunk = return old_rep_thunk
	-- memoized reuse for moves
	let reuse_other_file from old_rep_thunk = do
		fmd' <- getMD path tree
		fmd'' <- updateForestMDErrorsInsideWithPadsMD fmd' (liftM (snd . snd) $ Inc.read old_rep_thunk) -- adds the Pads errors
		rep_thunk <- get old_rep_thunk >>= \(fmd::Forest_md fs,rep) -> fsRef (fmd'',rep) -- since the old file may come from another location and/or its attributes may have changed
		return (rep_thunk)

	oldpath <- oldpath_f path
	mb <- findZippedMemo argProxy oldpath fsrepProxy 
	rep <- case mb of
		(Just (memo_tree@(isObservableFSTree -> True),memo_marg,memo_rep)) -> do
			memo_arg <- memo_marg
			samearg <- geq proxyNoCtx memo_arg arg
			if samearg
				then do
					debug ("memo hit " ++ show path) $ do
					df <- forestM $ diffFS memo_tree tree path
					dv <- diffValue memo_tree memo_rep
					case (isIdValueDelta dv,df) of
						(True,isEmptyFSTreeDeltaNodeMay -> True) -> if oldpath==path
							then reuse_same_file memo_rep
							else reuse_other_file oldpath memo_rep
						(True,Just (FSTreeChg _ _)) -> reuse_other_file path memo_rep
						(True,Just (FSTreeNew _ (Just ((==oldpath) -> True)) _)) -> reuse_other_file oldpath memo_rep
						otherwise -> load_file
				else load_file
		Nothing -> load_file				
	
	addZippedMemo path argProxy (return arg) rep tree
	return rep

-- | compressed archive (tar,gz,zip)
-- incremental loading is only supported if the specification for the archive's contents is:
-- 1) closed = does not depend on free variables -- this ensures that specs can be reused locally
doZLoadArchive :: (ZippedICMemo fs,ForestRep rep (ForestFSThunkI fs content),ForestMD fs rep,Eq rep,MData NoCtx (ForestI fs) rep,FSRep fs) =>
	Bool -> Proxy rep
	-> [ArchiveType] -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs
	-> (FilePath -> GetForestMD fs -> FSTree fs -> ForestI fs rep)
	-> (ForestI fs FilePath -> FilePath -> (rep,GetForestMD fs) -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep -> ForestO fs (SValueDelta rep))
	-> ForestI fs (ForestFSThunkI fs (Forest_md fs,rep))
doZLoadArchive isClosed (repProxy :: Proxy rep) exts oldpath_f path (tree :: FSTree fs) getMD load loadD = do
	let fs = Proxy :: Proxy fs
	let argsProxy = Proxy :: Proxy ()
	let fsrepProxy = Proxy -- :: Proxy (ForestFSThunkI fs rep)
	-- static loading
	let load_folder = fsThunk $ doZLoadArchive' exts oldpath_f path tree getMD load
		
	if isClosed
		then do
			oldpath <- oldpath_f path
			mb <- findZippedMemo argsProxy oldpath fsrepProxy
			rep <- case mb of
				Just (memo_tree@(isObservableFSTree -> True),(),memo_rep_thunk) -> do
	
					md@(fmd,irep) <- get memo_rep_thunk
					
					avfsTree <- forestM $ virtualTree tree
					avfsOldTree <- forestM $ virtualTree memo_tree
					let oldpathC = cardinalPath oldpath
					let pathC = cardinalPath path
					archiveDf <- forestM $ focusDiffFSTree memo_tree oldpathC tree pathC
					dv <- diffValue memo_tree irep
					
					unsafeWorld $ do
						loadD (return oldpathC) pathC (irep,getForestMDInTree) avfsOldTree archiveDf avfsTree dv
						fmd' <- inside $ getForestMDInTree path tree
						updateForestMDErrorsWith fmd' $ liftM (:[]) $ get_errors irep -- like a directory
						set memo_rep_thunk (fmd',irep)
					return memo_rep_thunk
				Nothing -> load_folder
			addZippedMemo path argsProxy () rep tree
			return rep
		else load_folder

doZLoadArchiveInner :: (ForestMD fs rep,MData NoCtx (ForestI fs) rep,ICRep fs) =>
	[ArchiveType] -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs
	-> (FilePath -> GetForestMD fs -> FSTree fs -> ForestI fs rep)
	-> ForestI fs rep
doZLoadArchiveInner exts oldpath_f path  tree getMD load = liftM snd $ doZLoadArchive' exts oldpath_f path  tree getMD load

doZLoadArchive' :: (ForestMD fs rep,MData NoCtx (ForestI fs) rep,ICRep fs) =>
	[ArchiveType] -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs
	-> (FilePath -> GetForestMD fs -> FSTree fs -> ForestI fs rep)
	-> ForestI fs (Forest_md fs,rep)
doZLoadArchive' exts oldpath_f path  tree getMD load = checkZPath' (Just False) path tree $ checkZFileExtension (archiveExtension exts) path $ do
	fmd <- getMD path tree
	avfsTree <- forestM $ virtualTree tree
	rep_arch <- load (cardinalPath path) getForestMDInTree avfsTree
	fmd' <- updateForestMDErrorsInsideWith fmd $ liftM (:[]) $ get_errors rep_arch -- like a directory
	return (fmd',rep_arch)
		
doZLoadSymLink :: (ForestInput fs FSThunk Inside,ICRep fs) => FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs (SymLink fs)
doZLoadSymLink path tree getMD = liftM SymLink $ fsThunk $ doZLoadSymLink' path tree getMD

doZLoadSymLink' :: (ForestInput fs FSThunk Inside,ICRep fs) => FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs (Forest_md fs,(FilePath,Base_md))
doZLoadSymLink' path tree getMD = checkZPath' Nothing path tree $ do
	md <- getMD path tree
	case symLink (fileInfo md) of
		Just sym -> return (md, (sym,cleanBasePD))
		Nothing -> do
			md' <- updateForestMDErrorsInsideWith md $ return [Pure.ioExceptionForestErr]
			return (md', ("",cleanBasePD))

doZLoadConstraint :: (ForestOutput fs ICThunk Inside,ForestMD fs rep,MData NoCtx (ForestI fs) rep) =>
	FSTree fs -> (rep -> ForestI fs Bool) -> ForestI fs rep -> ForestI fs rep
doZLoadConstraint tree pred load = do -- note that constraints do not consider the current path
	rep <- load
	rep' <- replace_errors rep $ \err -> do
		cond <- pred rep
		if cond
			then return err
			else return $ Pure.updateForestErr err [Pure.constraintViolationForestErr]
	return rep'

-- changes the current path
doZLoadFocus :: (Matching fs a,ForestMD fs rep) => FilePathFilter fs -> FilePath -> a -> FSTree fs -> GetForestMD fs -> (FilePath -> GetForestMD fs -> ForestI fs rep) -> ForestI fs rep
doZLoadFocus pathfilter path matching tree getMD load = do
	files <- forestM $ getMatchingFilesInTree path matching tree
	case files of
		[file] -> doZLoadNewPath pathfilter path file tree getMD load
		files -> doZLoadNewPath pathfilter path (pickFile files) tree getMD $ \newpath newgetMD -> do
			rep <- load newpath newgetMD
			rep' <- if length files == 0
				then return rep -- if there is no match then an error will pop from the recursive load
				else addMultipleMatchesErrorMDInside newpath files rep
			return rep'

doZLoadNewPath :: (ICRep fs) => FilePathFilter fs -> FilePath -> FilePath -> FSTree fs -> GetForestMD fs -> (FilePath -> GetForestMD fs -> ForestI fs x) -> ForestI fs x
doZLoadNewPath pathfilter oldpath file tree getMD load = debug ("doLoadNewPath " ++ show (oldpath </> file)) $ do
	newpath <- forestM $ stepPathInTree tree oldpath file -- changes the old path by a relative path, check the path traversal restrictions specific to each FS instantiation
	load newpath getMD

doZLoadDirectory :: (ForestInput fs FSThunk Inside,Eq rep,MData NoCtx (ForestI fs) rep,ICRep fs)
	=> FilePath -> FSTree fs -> (rep -> ForestI fs Forest_err) -> GetForestMD fs -> ForestI fs rep -> ForestI fs (ForestFSThunkI fs (Forest_md fs,rep))
doZLoadDirectory path tree collectMDErrors getMD load = fsThunk $ doZLoadDirectory' path tree collectMDErrors getMD load

-- the error count of the directory is computed lazily, so that if we only want, e.g., the fileinfo of the directory we don't need to check its contents
doZLoadDirectory' :: (ICRep fs,ForestInput fs FSThunk Inside,Eq rep,MData NoCtx (ForestI fs) rep)
	=> FilePath -> FSTree fs -> (rep -> ForestI fs Forest_err) -> GetForestMD fs -> ForestI fs rep -> ForestI fs (Forest_md fs,rep)
doZLoadDirectory' path tree collectMDErrors getMD ifGood = debug ("doLoadDirectory: "++show path) $ do
	rep_thunk <- checkZPath' (Just True) path tree $ do
		rep <- ifGood
		fmd <- getMD path tree
		fmd' <- updateForestMDErrorsInsideWith fmd $ liftM (:[]) $ collectMDErrors rep
		return (fmd',rep)
	return rep_thunk

doZLoadMaybe :: (Typeable rep,Eq rep,ForestMD fs rep) => FilePathFilter fs -> FilePath -> FSTree fs -> ForestI fs rep -> ForestI fs (ForestFSThunkI fs (Forest_md fs,Maybe rep))
doZLoadMaybe pathfilter path tree ifExists = fsThunk $ doZLoadMaybe' pathfilter path tree ifExists

doZLoadMaybeInner :: (Typeable rep,Eq rep,ForestMD fs rep) => FilePathFilter fs -> FilePath -> FSTree fs -> ForestI fs rep -> ForestI fs (Maybe rep)
doZLoadMaybeInner pathfilter path tree ifExists = liftM snd $ doZLoadMaybe' pathfilter path tree ifExists

doZLoadMaybe' :: (Typeable rep,Eq rep,ForestMD fs rep) => FilePathFilter fs -> FilePath -> FSTree fs -> ForestI fs rep -> ForestI fs (Forest_md fs,Maybe rep)
doZLoadMaybe' pathfilter path tree ifExists = do
	exists <- forestM $ doesExistInTree path tree
	if exists
		then do
			rep <- ifExists
			fmd <- get_fmd_header rep
			return (fmd,Just rep)
		else do
			fmd <- cleanForestMDwithFile path
			return (fmd,Nothing)

-- since the focus changes we need to compute the (eventually) previously loaded metadata of the parent node
doZLoadSimple :: (ForestMD fs rep,Matching fs a,MData NoCtx (ForestI fs) rep) =>
	FilePathFilter fs -> FilePath -> ForestI fs a -> FSTree fs
	-> (FilePath -> GetForestMD fs -> ForestI fs rep)
	-> ForestI fs rep
doZLoadSimple pathfilter path matching tree load = matching >>= \m -> doZLoadFocus pathfilter path m tree getForestMDInTree load

-- since the focus changes we need to compute the (eventually) previously loaded metadata of the parent node
doZLoadSimpleWithConstraint :: (ForestOutput fs ICThunk Inside,ForestMD fs rep,Matching fs a,MData NoCtx (ForestI fs) rep) =>
	FilePathFilter fs -> FilePath -> ForestI fs a -> FSTree fs -> (rep -> ForestI fs Bool)
	-> (FilePath -> GetForestMD fs -> ForestI fs rep)
	-> ForestI fs rep
doZLoadSimpleWithConstraint pathfilter path matching tree pred load = doZLoadConstraint tree pred $ matching >>= \m -> doZLoadFocus pathfilter path m tree getForestMDInTree load

doZLoadCompound :: (Typeable container_rep,Eq container_rep,Matching fs a,MData NoCtx (ForestI fs) rep',ForestMD fs rep') =>
	FilePathFilter fs -> FilePath -> ForestI fs a -> FSTree fs
	-> ([(FilePath,rep')] -> container_rep)
	-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> GetForestMD fs -> ForestI fs rep')
	-> ForestI fs container_rep
doZLoadCompound pathfilter path matchingM tree buildContainerRep load = debug ("doLoadCompound: "++show path) $ do
	matching <- matchingM
	files <- forestM $ getMatchingFilesInTree path matching tree
	metadatas <- mapM (getRelForestMDInTree path tree) files
	let filesmetas = zip files metadatas
	let loadEach (n,n_md) = liftM (n,) $ doZLoadFocus pathfilter path n tree (const2 $ return n_md) $ \newpath newGetMD -> do
		fileInfo_thunk <- ref $ fileInfo n_md
		rep' <- load n fileInfo_thunk newpath newGetMD
		return rep'
	loadlist <- mapM loadEach filesmetas
	return $ buildContainerRep loadlist

doZLoadCompoundWithConstraint :: (Typeable container_rep,Eq container_rep,ForestOutput fs ICThunk Inside,Matching fs a,MData NoCtx (ForestI fs) rep',ForestMD fs rep') =>
	FilePathFilter fs -> FilePath -> ForestI fs a -> FSTree fs
	-> (FilePath -> ForestFSThunkI fs FileInfo -> ForestI fs Bool)
	-> ([(FilePath,rep')] -> container_rep)
	-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> GetForestMD fs -> ForestI fs rep')
	-> ForestI fs container_rep
doZLoadCompoundWithConstraint pathfilter path matchingM tree pred buildContainerRep load = debug ("doLoadCompound: "++show path) $ do
	matching <- matchingM -- matching expressions are not saved for incremental reuse
	files <- forestM $ getMatchingFilesInTree path matching tree
	metadatas <- mapM (getRelForestMDInTree path tree) files
	let filesmetas = zip files metadatas
	let makeInfo (n,fmd) = do
		t <- ref $ fileInfo fmd -- we store the @FileInfo@ in a @FSThunk@ to allow incremental evaluation of the constraint expression
		u <- icThunk $ pred n t --the filename is a constant. during delta loading, whenever it changes we will load from scratch
		return (n,(fmd,(t,u)))
	filesmetasInfo <- mapM makeInfo filesmetas
	filesmetasInfo' <- filterM (force . snd . snd . snd) filesmetasInfo
	let loadEach (n,(n_md,(t,u))) = do
		rep <- doZLoadFocus pathfilter path n tree (const2 $ return n_md) $ load n t
		return (n,rep)
	loadlist <- mapM loadEach filesmetasInfo'
	return (buildContainerRep loadlist)

---- ** auxiliary functions

checkZPath :: (ICRep fs,Eq rep,MData NoCtx (ForestI fs) rep,ForestMD fs rep) => FilePath -> FSTree fs -> ForestI fs rep -> ForestI fs (ForestFSThunkI fs rep)
checkZPath path tree ifExists = mod $ checkZPath' (Just False) path tree ifExists

checkZPath' :: (MData NoCtx (ForestI fs) rep,ForestMD fs rep) => Maybe Bool -> FilePath -> FSTree fs -> ForestI fs rep -> ForestI fs rep
checkZPath' cond path tree ifExists = {-debug ("checkPath' "++show path ++ showFSTree tree) $ -} do
	exists <- case cond of
		Nothing -> forestM $ doesExistInTree path tree
		Just isDir -> forestM $ if isDir then doesDirectoryExistInTree path tree else doesFileExistInTree path tree
	if exists
		then ifExists
		else do
			def_rep <- forestdefault
			def_rep' <- replace_errors def_rep $ Prelude.const $ return $ Pure.missingPathForestErr path
			return def_rep'
			
checkZFileExtension :: (MData NoCtx (ForestI fs) rep,ForestMD fs rep) => String -> FilePath -> ForestI fs rep -> ForestI fs rep
checkZFileExtension ext path ifExists = do
	if isSuffixOf ext path
		then ifExists
		else do
			def_rep <- forestdefault
			def_rep' <- replace_errors def_rep $ Prelude.const $ return $ Pure.wrongFileExtensionForestErr ext path
			return def_rep'
