{-# LANGUAGE ConstraintKinds, OverlappingInstances, TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}



module Language.Forest.IC.IO.ZLoading where

import Language.Forest.IC.IO.ZDefault
import Language.Forest.IC.PadsInstances
import Prelude hiding (const,read,mod)
import qualified Prelude
import Language.Forest.IC.Default
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
doZLoadFile1 :: (IncK (IncForest fs) Forest_err,IncK (IncForest fs) ((Forest_md fs, md), pads),ICRep fs,ZippedICMemo fs,MData NoCtx (ForestI fs) arg,Typeable arg,Eq arg,FSRep fs,Pads1 arg pads md) =>
	Proxy pads -> Pure.Arg arg -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs
	-> ForestI fs (ForestFSThunkI fs ((Forest_md fs,md),pads))
doZLoadFile1 (repProxy :: Proxy pads) (Pure.Arg arg :: Pure.Arg arg) oldpath_f path (tree :: FSTree fs) getMD = debug ("doLoadFile1 " ++ show path) $ do
	let argProxy = Proxy :: Proxy (Pure.Arg arg)
	let fsrepProxy = Proxy
	let fs = (Proxy::Proxy fs)
	-- default static loading
	
	let fileGood = do
		(pads,md) <- forestM $ pathInTree path tree >>= forestIO . parseFile1 arg
		fmd <- getMD path tree
		fmd' <- updateForestMDErrorsInsideWithPadsMD fmd (return md) -- adds the Pads errors
		return ((fmd',md),pads)
	
	let fileBad = doZDefaultFile1' (Pure.Arg arg) path
	
	let load_file = checkZPath path tree fileGood fileBad
	
	-- memoized reuse
	let reuse_same_file old_rep_thunk = return old_rep_thunk
	-- memoized reuse for moves
	let reuse_other_file from old_rep_thunk = do
		fmd' <- getMD path tree
		fmd'' <- updateForestMDErrorsInsideWithPadsMD fmd' (liftM (snd . fst) $ Inc.read old_rep_thunk) -- adds the Pads errors
		rep_thunk <- get old_rep_thunk >>= \((fmd,bmd),rep) -> fsRef ((fmd'',bmd),rep) -- since the old file may come from another location and/or its attributes may have changed
		return (rep_thunk)

	oldpath <- oldpath_f path
	mb <- findZippedMemo argProxy oldpath fsrepProxy 
	rep <- case mb of
		(Just (memo_tree,memo_marg,memo_rep)) -> do
			memo_arg <- memo_marg
			-- deep equality of arguments, since thunks can be arguments and change
			samearg <- geq proxyNoCtx memo_arg arg
			if samearg
				then do
					debug ("memo hit " ++ show path) $ do
					df <- forestM $ diffFS memo_tree tree path
					dv <- unsafeWorld $ diffValueThunk memo_tree memo_rep
					case (isIdValueDelta dv,df) of
						(True,Just (isEmptyFSTreeDeltaNodeMay -> True)) -> if oldpath==path
							then reuse_same_file memo_rep
							else reuse_other_file oldpath memo_rep
						(True,Just (Just (FSTreeChg _ _))) -> reuse_other_file path memo_rep
						(True,Just (Just (FSTreeNew _ (Just ((==oldpath) -> True)) _ _))) -> reuse_other_file oldpath memo_rep
						otherwise -> load_file
				else load_file
		Nothing -> load_file				
	
	addZippedMemo path argProxy (return arg) rep (Just tree)
	return rep

-- | compressed archive (tar,gz,zip)
-- incremental loading is only supported if the specification for the archive's contents is:
-- 1) closed = does not depend on free variables -- this ensures that specs can be reused locally
doZLoadArchive :: (IncK (IncForest fs) (Forest_md fs, rep),Typeable rep,DeltaClass d,ZippedICMemo fs,ForestMD fs rep,FSRep fs) =>
	Bool -> Proxy rep
	-> [ArchiveType] -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs
	-> (FilePath -> GetForestMD fs -> FSTree fs -> ForestI fs rep)
	-> (FilePath -> ForestI fs rep)
	-> (ForestI fs FilePath -> FilePath -> (rep,GetForestMD fs) -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep -> ForestO fs (d rep))
	-> (FSTree fs -> rep -> ForestO fs (ValueDelta fs rep))
	-> ForestI fs (ForestFSThunkI fs (Forest_md fs,rep))
doZLoadArchive isClosed (repProxy :: Proxy rep) exts oldpath_f path (tree :: FSTree fs) getMD loadGood loadBad loadD diffValue = do
	let fs = Proxy :: Proxy fs
	let argsProxy = Proxy :: Proxy ()
	let fsrepProxy = Proxy
	-- static loading
	let load_folder = fsThunk $ doZLoadArchiveInner exts oldpath_f path tree getMD loadGood loadBad
		
	if isClosed
		then do
			oldpath <- oldpath_f path
			mb <- findZippedMemo argsProxy oldpath fsrepProxy
			rep <- case mb of
				Just (memo_tree,(),memo_rep_thunk) -> do
	
					md@(fmd,irep) <- get memo_rep_thunk
					
					avfsTree <- forestM $ virtualTree tree
					avfsOldTree <- forestM $ virtualTree memo_tree
					let oldpathC = cardinalPath oldpath
					let pathC = cardinalPath path
					archiveDf <- forestM $ focusDiffFSTree memo_tree oldpathC tree pathC
					
					unsafeWorld $ do
						dv <- diffValue memo_tree irep
						direp <- liftM toNSValueDelta $ loadD (return oldpathC) pathC (irep,getForestMDInTree) avfsOldTree archiveDf avfsTree dv
						case (oldpath==path,direp) of
							(True,StableVD _) -> do
								fmd' <- inside $ getForestMDInTree path tree
								replaceForestMDErrorsWith fmd $ liftM (:[]) $ get_errors fmd'
								updateForestMDErrorsWith fmd $ liftM (:[]) $ get_errors irep
							otherwise -> do
								fmd' <- inside $ getForestMDInTree path tree
								let irep' = applyNSValueDelta direp irep
								updateForestMDErrorsWith fmd' $ liftM (:[]) $ get_errors irep' -- like a directory
								set memo_rep_thunk (fmd',irep')
					return memo_rep_thunk
				Nothing -> load_folder
			addZippedMemo path argsProxy () rep (Just tree)
			return rep
		else load_folder

doZLoadArchiveInner :: (ForestMD fs rep,ICRep fs) =>
	[ArchiveType] -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs
	-> (FilePath -> GetForestMD fs -> FSTree fs -> ForestI fs rep)
	-> (FilePath -> ForestI fs rep)
	-> ForestI fs (Forest_md fs,rep)
doZLoadArchiveInner exts oldpath_f path  tree getMD loadGood loadBad = do
	
	let archGood = do
		fmd <- getMD path tree
		avfsTree <- forestM $ virtualTree tree
		rep_arch <- loadGood (cardinalPath path) getForestMDInTree avfsTree
		fmd' <- updateForestMDErrorsInsideWith fmd $ liftM (:[]) $ get_errors rep_arch -- like a directory
		return (fmd',rep_arch)
	
	let archBad = doZDefaultArchiveInner path loadBad
	
	checkZPath' (Just False) path tree (checkZFileExtension (archiveExtension exts) path archGood archBad) archBad
		
doZLoadSymLink :: (IncK (IncForest fs) Forest_err,IncK (IncForest fs) ((Forest_md fs, Base_md), FilePath),ForestInput fs FSThunk Inside,ICRep fs) => FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs (SymLink fs)
doZLoadSymLink path tree getMD = liftM SymLink $ fsThunk $ doZLoadSymLink' path tree getMD

doZLoadSymLink' :: (IncK (IncForest fs) Forest_err,ForestInput fs FSThunk Inside,ICRep fs) => FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs ((Forest_md fs,Base_md),FilePath)
doZLoadSymLink' path tree getMD = do
	
	let linkGood = do
		md <- getMD path tree
		case symLink (fileInfo md) of
			Just sym -> return ((md,cleanBasePD), sym)
			Nothing -> do
				md' <- updateForestMDErrorsInsideWith md $ return [Pure.ioExceptionForestErr]
				return ((md',cleanBasePD), "")
				
	let linkBad = doZDefaultSymLink' path
		
	checkZPath' Nothing path tree linkGood linkBad

doZLoadConstraint :: (IncK (IncForest fs) (ForestFSThunkI fs Forest_err, rep),ForestOutput fs ICThunk Inside,ForestMD fs rep,MData NoCtx (ForestI fs) rep) =>
	FSTree fs -> (rep -> ForestI fs Bool) -> ForestI fs rep -> ForestI fs (ForestFSThunkI fs (ForestFSThunkI fs Forest_err,rep))
doZLoadConstraint tree pred load = fsThunk $ doZLoadConstraintInner tree pred load

-- for IC, we need to be able to dissociate the constraint error from the inner errors
doZLoadConstraintInner :: (ForestOutput fs ICThunk Inside,ForestMD fs rep,MData NoCtx (ForestI fs) rep) =>
	FSTree fs -> (rep -> ForestI fs Bool) -> ForestI fs rep -> ForestI fs (ForestFSThunkI fs Forest_err,rep)
doZLoadConstraintInner tree pred load = do -- note that constraints do not consider the current path
	rep <- load
	err_t <- fsThunk $ do
		err_cond <- predForestErr $ pred rep
		err_inner <- get_errors rep
		return $ Pure.mergeForestErrs err_cond err_inner
	return (err_t,rep)

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

doZLoadDirectory :: (IncK (IncForest fs) Forest_err,IncK (IncForest fs) (Forest_md fs, rep),ForestInput fs FSThunk Inside,ICRep fs)
	=> FilePath -> FSTree fs -> (rep -> ForestI fs Forest_err) -> GetForestMD fs -> ForestI fs rep -> ForestI fs rep -> ForestI fs (ForestFSThunkI fs (Forest_md fs,rep))
doZLoadDirectory path tree collectMDErrors getMD ifGood ifBad = fsThunk $ doZLoadDirectory' path tree collectMDErrors getMD ifGood ifBad

-- the error count of the directory is computed lazily, so that if we only want, e.g., the fileinfo of the directory we don't need to check its contents
doZLoadDirectory' :: (IncK (IncForest fs) Forest_err,ICRep fs,ForestInput fs FSThunk Inside)
	=> FilePath -> FSTree fs -> (rep -> ForestI fs Forest_err) -> GetForestMD fs -> ForestI fs rep -> ForestI fs rep -> ForestI fs (Forest_md fs,rep)
doZLoadDirectory' path tree collectMDErrors getMD ifGood ifBad = debug ("doLoadDirectory: "++show path) $ do
	
	let dirGood = do
		rep <- ifGood
		fmd <- getMD path tree
		fmd' <- updateForestMDErrorsInsideWith fmd $ liftM (:[]) $ collectMDErrors rep
		return (fmd',rep)
	
	let dirBad = doZDefaultDirectory' path collectMDErrors ifBad
	
	checkZPath' (Just True) path tree dirGood dirBad

doZLoadMaybe :: (IncK (IncForest fs) (Forest_md fs, Maybe rep),ForestMD fs rep) => FilePathFilter fs -> FilePath -> FSTree fs -> ForestI fs rep -> ForestI fs (ForestFSThunkI fs (Forest_md fs,Maybe rep))
doZLoadMaybe pathfilter path tree ifExists = fsThunk $ doZLoadMaybe' pathfilter path tree ifExists

doZLoadMaybeInner :: (ForestMD fs rep) => FilePathFilter fs -> FilePath -> FSTree fs -> ForestI fs rep -> ForestI fs (Maybe rep)
doZLoadMaybeInner pathfilter path tree ifExists = liftM snd $ doZLoadMaybe' pathfilter path tree ifExists

doZLoadMaybe' :: (ForestMD fs rep) => FilePathFilter fs -> FilePath -> FSTree fs -> ForestI fs rep -> ForestI fs (Forest_md fs,Maybe rep)
doZLoadMaybe' pathfilter path tree ifExists = do
	exists <- forestM $ doesExistInTree path tree
	if exists
		then do
			rep <- ifExists
			fmd <- cleanForestMDwithFile path
			fmd' <- updateForestMDErrorsInsideWith fmd $ liftM (:[]) $ get_errors rep
			return (fmd',Just rep)
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
	-> ForestI fs (ForestFSThunkI fs Forest_err,rep)
doZLoadSimpleWithConstraint pathfilter path matching tree pred load = doZLoadConstraintInner tree pred $ matching >>= \m -> doZLoadFocus pathfilter path m tree getForestMDInTree load

doZLoadCompound :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) container_rep,Matching fs a,MData NoCtx (ForestI fs) rep',ForestMD fs rep') =>
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

doZLoadCompoundWithConstraint :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Bool,IncK (IncForest fs) container_rep,ForestOutput fs ICThunk Inside,Matching fs a,MData NoCtx (ForestI fs) rep',ForestMD fs rep') =>
	FilePathFilter fs -> FilePath -> ForestI fs a -> FSTree fs
	-> (FilePath -> ForestFSThunkI fs FileInfo -> ForestI fs Bool)
	-> ([(FileName,rep')] -> container_rep)
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

checkZPath :: (IncK (IncForest fs) rep,ICRep fs,ForestMD fs rep) => FilePath -> FSTree fs -> ForestI fs rep -> ForestI fs rep -> ForestI fs (ForestFSThunkI fs rep)
checkZPath path tree ifExists ifNotExists = mod $ checkZPath' (Just False) path tree ifExists ifNotExists

checkZPath' :: (ForestMD fs rep) => Maybe Bool -> FilePath -> FSTree fs -> ForestI fs rep -> ForestI fs rep -> ForestI fs rep
checkZPath' cond path tree ifExists ifNotExists = {-debug ("checkPath' "++show path ++ showFSTree tree) $ -} do
	exists <- case cond of
		Nothing -> forestM $ doesExistInTree path tree
		Just isDir -> forestM $ if isDir then (doesDirectoryExistInTree path tree) else (doesFileExistInTree path tree)
	if exists
		then ifExists
		else ifNotExists
			
checkZFileExtension :: (ForestMD fs rep) => String -> FilePath -> ForestI fs rep -> ForestI fs rep -> ForestI fs rep
checkZFileExtension ext path ifExists ifNotExists = do
	if isSuffixOf ext path
		then ifExists
		else ifNotExists
