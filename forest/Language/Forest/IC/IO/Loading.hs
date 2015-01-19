{-# LANGUAGE OverlappingInstances, TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}



module Language.Forest.IC.IO.Loading where

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

doLoadArgs :: (ForestArgs fs args,Eq rep,Eq md,ICRep fs,imd ~ MDArgs mode md (ForestICThunksI fs args)) => 
	LiftedICMode mode -> Proxy args -> ForestICThunksI fs args -> ForestI fs (rep,md) -> ForestI fs (rep,imd)
doLoadArgs mode proxy args load = do
	(rep,md) <- load
	return (rep,mkMDArgs mode md args)

-- | lazy file loading
-- Pads errors contribute to the Forest error count
doLoadFile :: (ICMemo fs,ForestInput fs FSThunk Inside,Eq md,Eq pads,MData NoCtx (ForestI fs) pads,MData NoCtx (ForestI fs) md,ICRep fs,Pads pads md) =>
	Proxy pads -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs (ForestFSThunkI fs pads,ForestFSThunkI fs (Forest_md fs,ForestFSThunkI fs md))
doLoadFile repProxy oldpath_f path (tree :: FSTree fs) getMD = debug ("doLoadFile " ++ show path) $ do
	let fs = Proxy::Proxy fs
	let argProxy = Proxy :: Proxy ()
	let fsrepProxy = Proxy :: Proxy (ForestFSThunkI fs pads)
	-- default static loading
	let load_file = do
		parseThunk <- newHSThunk $ debug ("reading file "++show path) $ forestM $ pathInTree path tree >>= forestIO . parseFile
		rep_thunk <- checkPathData path tree (getRep $ Inc.read parseThunk)
		md_thunk <- checkPathMeta path tree $ do
			fmd <- getMD path tree
			fmd' <- updateForestMDErrorsInsideWithPadsMD fmd (getMd $ Inc.read parseThunk) -- adds the Pads errors
			md <- mod $ getMd $ Inc.read parseThunk -- to avoid Inc.reading the file unless strictly necessary
			return (fmd',md)
		return (rep_thunk,md_thunk)
	-- memoized reuse
	let reuse_same_file old_rep old_md = return (old_rep,old_md)
	-- memoized reuse for moves
	let reuse_other_file from old_rep_thunk old_md_thunk = do
			fmd' <- getMD path tree
			fmd'' <- updateForestMDErrorsInsideWithPadsMD fmd' (Inc.read =<< liftM snd (Inc.read old_md_thunk)) -- adds the Pads errors
			md_thunk <- get old_md_thunk >>= \(fmd::Forest_md fs,md) -> ref (fmd'',md) -- since the old file may come from another location and/or its attributes may have changed
			remMemo fs from fsrepProxy
			return (old_rep_thunk,md_thunk)
	oldpath <- oldpath_f path
	mb <- findMemo argProxy oldpath fsrepProxy 
	(rep,md) <- case mb of
		(Just (memo_tree,(),memo_rep,memo_md)) -> debug ("memo hit " ++ show path) $ do
			df <- forestM $ diffFS memo_tree tree path
			case df of
				(isEmptyFSTreeDeltaNodeMay -> True) -> if oldpath==path
					then reuse_same_file memo_rep memo_md
					else reuse_other_file oldpath memo_rep memo_md
				Just (FSTreeChg _ _) -> reuse_other_file path memo_rep memo_md
				Just (FSTreeNew _ (Just ((==oldpath) -> True)) _) -> reuse_other_file oldpath memo_rep memo_md
				otherwise -> load_file
		Nothing -> load_file				
	
	addMemo path argProxy () (rep,md) tree
	return (rep,md)

-- | lazy file loading
-- XXX: Pads specs currently accept a single optional argument and have no incremental loading, so a change in the argument's value requires recomputation
-- XXX: Pads errors do not contribute to the Forest error count
doLoadFile1 :: (ICMemo fs,MData NoCtx (Inside (IncForest fs) IORef IO) arg,ForestIs fs arg ~ ForestI fs arg,ForestMD fs md,Typeable arg,Eq arg,Eq pads,Eq md,MData NoCtx (ForestI fs) pads,MData NoCtx (ForestI fs) md,FSRep fs,Pads1 arg pads md) =>
	Proxy pads -> arg -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs (ForestFSThunkI fs pads,ForestFSThunkI fs (Forest_md fs,ForestFSThunkI fs md))
doLoadFile1 (repProxy :: Proxy pads) (arg :: arg) oldpath_f path (tree :: FSTree fs) getMD = debug ("doLoadFile1 " ++ show path) $ do
	let argProxy = Proxy :: Proxy arg
	let fsrepProxy = Proxy :: Proxy (ForestFSThunkI fs pads)
	let fs = (Proxy::Proxy fs)
	-- default static loading
	let load_file = do
		parseThunk <- newHSThunk $ debug ("reading file "++show path) $ forestM $ pathInTree path tree >>= forestIO . parseFile1 arg
		rep_thunk <- checkPathData path tree (getRep $ Inc.read parseThunk)
		md_thunk <- checkPathMeta path tree $ do
			fmd <- getMD path tree
			fmd' <- updateForestMDErrorsInsideWithPadsMD fmd (getMd $ Inc.read parseThunk) -- adds the Pads errors
			md <- mod $ getMd $ Inc.read parseThunk -- to avoid Inc.reading the file unless strictly necessary
			return (fmd',md)
		return (rep_thunk,md_thunk)
	-- memoized reuse
	let reuse_same_file old_rep_thunk old_md_thunk = return (old_rep_thunk,old_md_thunk)
	-- memoized reuse for moves
	let reuse_other_file from old_rep_thunk old_md_thunk = do
		fmd' <- getMD path tree
		fmd'' <- updateForestMDErrorsInsideWithPadsMD fmd' (Inc.read =<< liftM snd (Inc.read old_md_thunk)) -- adds the Pads errors
		md_thunk <- get old_md_thunk >>= \(fmd::Forest_md fs,md) -> fsRef (fmd'',md) -- since the old file may come from another location and/or its attributes may have changed
		remMemo fs from fsrepProxy
		return (old_rep_thunk,md_thunk)

	oldpath <- oldpath_f path
	mb <- findMemo argProxy oldpath fsrepProxy 
	(rep,md) <- case mb of
		(Just (memo_tree,memo_marg,memo_rep,memo_md)) -> do
			memo_arg <- memo_marg
			samearg <- geq proxyNoCtx memo_arg arg
			if samearg
				then do
					debug ("memo hit " ++ show path) $ do
					df <- forestM $ diffFS memo_tree tree path
					case df of
						(isEmptyFSTreeDeltaNodeMay -> True) -> if oldpath==path
							then reuse_same_file memo_rep memo_md
							else reuse_other_file oldpath memo_rep memo_md
						Just (FSTreeChg _ _) -> reuse_other_file path memo_rep memo_md
						Just (FSTreeNew _ (Just ((==oldpath) -> True)) _) -> reuse_other_file oldpath memo_rep memo_md
						otherwise -> load_file
				else load_file
		Nothing -> load_file				
	
	addMemo path argProxy (return arg) (rep,md) tree
	return (rep,md)

-- | compressed archive (tar,gz,zip)
-- incremental loading is only supported if the specification for the archive's contents is:
-- 1) closed = does not depend on free variables -- this ensures that specs can be reused locally
doLoadArchive :: (ICMemo fs,ForestMD fs md,Eq rep,Eq md,MData NoCtx (ForestI fs) rep,FSRep fs,MData NoCtx (ForestI fs) md) =>
	Bool -> Proxy rep
	-> [ArchiveType] -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs
	-> (FilePath -> GetForestMD fs -> FSTree fs -> ForestI fs (rep,md))
	-> (ForestI fs FilePath -> FilePath -> OldData fs rep md -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ForestO fs (SValueDelta rep,SValueDelta md))
	-> ForestI fs (ForestFSThunkI fs rep,ForestFSThunkI fs (Forest_md fs,md))
doLoadArchive isClosed (repProxy :: Proxy rep) exts oldpath_f path (tree :: FSTree fs) getMD load loadD = do
	let fs = Proxy :: Proxy fs
	let argsProxy = Proxy :: Proxy ()
	let fsrepProxy = Proxy :: Proxy (ForestFSThunkI fs rep)
	-- static loading
	let load_folder = mkThunks tree $ doLoadArchive' exts oldpath_f path tree getMD load
		
	if isClosed
		then do
			oldpath <- oldpath_f path
			mb <- findMemo argsProxy oldpath fsrepProxy
			(rep,md) <- case mb of
				Just (memo_tree,(),memo_rep_thunk,memo_md_thunk) -> do
					
					rep <- get memo_rep_thunk
					md@(fmd,imd) <- get memo_md_thunk
					
					avfsTree <- forestM $ virtualTree tree
					avfsOldTree <- forestM $ virtualTree memo_tree
					let oldpathC = cardinalPath oldpath
					let pathC = cardinalPath path
					archiveDf <- forestM $ focusDiffFSTree memo_tree oldpathC tree pathC
					
					unsafeWorld $ do
						loadD (return oldpathC) pathC ((rep,imd),getForestMDInTree) avfsOldTree archiveDf avfsTree
						fmd' <- inside $ getForestMDInTree path tree
						updateForestMDErrorsWith fmd' $ liftM (:[]) $ get_errors imd -- like a directory
						set memo_md_thunk (fmd',imd)
					remMemo fs oldpath repProxy
					return (memo_rep_thunk,memo_md_thunk)
				Nothing -> load_folder
			addMemo path argsProxy () (rep,md) tree
			return (rep,md)
		else load_folder

doLoadArchive' :: (ForestMD fs md,MData NoCtx (ForestI fs) rep,ICRep fs,MData NoCtx (ForestI fs) md) =>
	[ArchiveType] -> FilePathFilter fs -> FilePath -> FSTree fs -> GetForestMD fs
	-> (FilePath -> GetForestMD fs -> FSTree fs -> ForestI fs (rep,md))
	-> ForestI fs (rep,(Forest_md fs,md))
doLoadArchive' exts oldpath_f path  tree getMD load = checkPath' (Just False) path tree $ checkFileExtension (archiveExtension exts) path $ do
	fmd <- getMD path tree
	avfsTree <- forestM $ virtualTree tree
	(rep,md_arch) <- load (cardinalPath path) getForestMDInTree avfsTree
	fmd' <- updateForestMDErrorsInsideWith fmd $ liftM (:[]) $ get_errors md_arch -- like a directory
	return (rep,(fmd',md_arch))
		
doLoadSymLink :: (ForestInput fs FSThunk Inside,ICRep fs) => FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs (ForestFSThunkI fs FilePath,ForestFSThunkI fs (Forest_md fs, Base_md))
doLoadSymLink path tree getMD = mkThunks tree $ doLoadSymLink' path tree getMD

doLoadSymLink' :: (ForestInput fs FSThunk Inside,ICRep fs) => FilePath -> FSTree fs -> GetForestMD fs -> ForestI fs (FilePath,(Forest_md fs, Base_md))
doLoadSymLink' path tree getMD = checkPath' Nothing path tree $ do
	md <- getMD path tree
	case symLink (fileInfo md) of
		Just sym -> return (sym,(md,cleanBasePD))
		Nothing -> do
			md' <- updateForestMDErrorsInsideWith md $ return [Pure.ioExceptionForestErr]
			return ("", (md',cleanBasePD))

doLoadConstraint :: (Typeable imd,ForestOutput fs ICThunk Inside,Eq imd,ForestMD fs imd,MData NoCtx (ForestI fs) rep,md ~ ForestFSThunkI fs imd) =>
	LiftedICMode mode -> FSTree fs -> ((rep,md) -> ForestI fs Bool) -> ForestI fs (rep,md) -> ForestI fs (rep,MDArgs mode md (ForestICThunkI fs Bool))
doLoadConstraint mode tree pred load = do -- note that constraints do not consider the current path
	result@(rep,md) <- load
	cond_thunk <- inside $ icThunk $ pred result
	md' <- replace_errors md $ \err -> do
		cond <- force cond_thunk
		if cond
			then return err
			else return $ Pure.updateForestErr err [Pure.constraintViolationForestErr]
	return (rep,mkMDArgs mode md' cond_thunk)

-- changes the current path
doLoadFocus :: (Matching fs a,ForestMD fs md) => FilePathFilter fs -> FilePath -> a -> FSTree fs -> GetForestMD fs -> (FilePath -> GetForestMD fs -> ForestI fs (rep,md)) -> ForestI fs (rep,md)
doLoadFocus pathfilter path matching tree getMD load = do
	files <- forestM $ getMatchingFilesInTree path matching tree
	case files of
		[file] -> doLoadNewPath pathfilter path file tree getMD load
		files -> doLoadNewPath pathfilter path (pickFile files) tree getMD $ \newpath newgetMD -> do
			(rep,md) <- load newpath newgetMD
			md' <- if length files == 0
				then return md -- if there is no match then an error will pop from the recursive load
				else addMultipleMatchesErrorMDInside newpath files md
			return (rep,md')

doLoadNewPath :: (ICRep fs) => FilePathFilter fs -> FilePath -> FilePath -> FSTree fs -> GetForestMD fs -> (FilePath -> GetForestMD fs -> ForestI fs x) -> ForestI fs x
doLoadNewPath pathfilter oldpath file tree getMD load = debug ("doLoadNewPath " ++ show (oldpath </> file)) $ do
	newpath <- forestM $ stepPathInTree tree oldpath file -- changes the old path by a relative path, check the path traversal restrictions specific to each FS instantiation
	load newpath getMD

doLoadDirectory :: (ForestInput fs FSThunk Inside,Eq rep,Eq md,MData NoCtx (ForestI fs) rep,ICRep fs,MData NoCtx (ForestI fs) md)
	=> FilePath -> FSTree fs -> (md -> ForestI fs Forest_err) -> GetForestMD fs -> ForestI fs (rep,md) -> ForestI fs (ForestFSThunkI fs rep,ForestFSThunkI fs (Forest_md fs,md))
doLoadDirectory path tree collectMDErrors getMD load = mkThunksM tree $ doLoadDirectory' path tree collectMDErrors getMD load

-- the error count of the directory is computed lazily, so that if we only want, e.g., the fileinfo of the directory we don't need to check its contents
doLoadDirectory' :: (ICRep fs,ForestInput fs FSThunk Inside,Eq rep,Eq md,MData NoCtx (ForestI fs) rep,MData NoCtx (ForestI fs) md)
	=> FilePath -> FSTree fs -> (md -> ForestI fs Forest_err) -> GetForestMD fs -> ForestI fs (rep,md) -> ForestI fs (ForestI fs rep,ForestI fs (Forest_md fs,md))
doLoadDirectory' path tree collectMDErrors getMD ifGood = debug ("doLoadDirectory: "++show path) $ do
	ifGoodThunk <- newHSThunk ifGood
	let loadData = checkPathData' True path tree $ getRep $ Inc.read ifGoodThunk
	let loadMeta = checkPathMeta' True path tree $ do
		fmd <- getMD path tree
		mds <- getMd $ Inc.read ifGoodThunk
		fmd' <- updateForestMDErrorsInsideWith fmd $ liftM (:[]) $ collectMDErrors mds
		return (fmd',mds)
	return (loadData,loadMeta)

doLoadMaybe :: (Typeable rep,Typeable md,Eq rep,Eq md,ForestMD fs md) => FilePathFilter fs -> FilePath -> FSTree fs -> ForestI fs (rep,md) -> ForestI fs (ForestFSThunkI fs (Maybe rep),ForestFSThunkI fs (Forest_md fs,Maybe md))
doLoadMaybe pathfilter path tree ifExists = mkThunksM tree $ doLoadMaybe' pathfilter path tree ifExists

doLoadMaybe' :: (Typeable rep,Typeable md,Eq rep,Eq md,ForestMD fs md) => FilePathFilter fs -> FilePath -> FSTree fs -> ForestI fs (rep,md) -> ForestI fs (ForestI fs (Maybe rep),ForestI fs (Forest_md fs,Maybe md))
doLoadMaybe' pathfilter path tree ifExists = do
	ifExistsThunk <- newHSThunk ifExists
	let loadData = do
		exists <- forestM $ doesExistInTree path tree
		if exists
			then liftM Just $ getRep $ Inc.read ifExistsThunk
			else return Nothing
	let loadMeta = do
		exists <- forestM $ doesExistInTree path tree
		debug ("doLoadMaybe: "++show (path,exists)) $ if exists
			then do
				md <- getMd $ Inc.read ifExistsThunk
				fmd <- get_fmd_header md
				return (fmd,Just md) -- use the same @Forest_md@
			else do
				fmd <- cleanForestMDwithFile path
				return (fmd,Nothing)
	return (loadData,loadMeta)

-- since the focus changes we need to compute the (eventually) previously loaded metadata of the parent node
doLoadSimple :: (Eq imd',ForestMD fs imd',Matching fs a,MData NoCtx (ForestI fs) rep',ForestMD fs md', md' ~ ForestFSThunkI fs imd') =>
	FilePathFilter fs -> FilePath -> ForestI fs a -> FSTree fs
	-> (FilePath -> GetForestMD fs -> ForestI fs (rep',md'))
	-> ForestI fs (rep',md')
doLoadSimple pathfilter path matching tree load = matching >>= \m -> doLoadFocus pathfilter path m tree getForestMDInTree load

-- since the focus changes we need to compute the (eventually) previously loaded metadata of the parent node
doLoadSimpleWithConstraint :: (Typeable imd',ForestOutput fs ICThunk Inside,Eq imd',ForestMD fs imd',Matching fs a,MData NoCtx (ForestI fs) rep',ForestMD fs md', md' ~ ForestFSThunkI fs imd') =>
	LiftedICMode mode -> FilePathFilter fs -> FilePath -> ForestI fs a -> FSTree fs -> ((rep',md') -> ForestI fs Bool)
	-> (FilePath -> GetForestMD fs -> ForestI fs (rep',md'))
	-> ForestI fs (rep',MDArgs mode md' (ForestICThunkI fs Bool))
doLoadSimpleWithConstraint mode pathfilter path matching tree pred load = doLoadConstraint mode tree pred $ matching >>= \m -> doLoadFocus pathfilter path m tree getForestMDInTree load

doLoadCompound :: (Typeable container_rep,Typeable container_md,Eq container_md,Eq container_rep,ForestMD fs (md',FSThunk fs Inside (IncForest fs) IORef IO FileInfo),Matching fs a,MData NoCtx (ForestI fs) rep',ForestMD fs imd, imd ~ MDArgs mode md' (ForestFSThunkI fs FileInfo)) =>
	LiftedICMode mode -> FilePathFilter fs -> FilePath -> ForestI fs a -> FSTree fs
	-> ([(FilePath,rep')] -> container_rep) -> ([(FilePath,imd)] -> container_md)
	-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> GetForestMD fs -> ForestI fs (rep',md'))
	-> ForestI fs (ForestFSThunkI fs container_rep,ForestFSThunkI fs container_md)
doLoadCompound mode pathfilter path matchingM tree buildContainerRep buildContainerMd load = mkThunks tree $ debug ("doLoadCompound: "++show path) $ do
	matching <- matchingM
	files <- forestM $ getMatchingFilesInTree path matching tree
	metadatas <- mapM (getRelForestMDInTree path tree) files
	let filesmetas = zip files metadatas
	let loadEach (n,n_md) = liftM (n,) $ doLoadFocus pathfilter path n tree (const2 $ return n_md) $ \newpath newGetMD -> do
		fileInfo_thunk <- ref $ fileInfo n_md
		(rep',md') <- load n fileInfo_thunk newpath newGetMD
		return (rep',mkMDArgs mode md' $ fileInfo_thunk)
	loadlist <- mapM loadEach filesmetas
	let replist = map (id >< fst) loadlist
	let mdlist = map (id >< snd) loadlist
	return (buildContainerRep replist,buildContainerMd mdlist)

doLoadCompoundWithConstraint :: (Typeable container_rep,Typeable container_md,Eq container_md,Eq container_rep,ForestOutput fs ICThunk Inside,Matching fs a,MData NoCtx (ForestI fs) rep',ForestMD fs md',imd ~ MDArgs mode md' (ForestFSThunkI fs FileInfo,ForestICThunkI fs Bool) ) =>
	LiftedICMode mode -> FilePathFilter fs -> FilePath -> ForestI fs a -> FSTree fs
	-> (FilePath -> ForestFSThunkI fs FileInfo -> ForestI fs Bool)
	-> ([(FilePath,rep')] -> container_rep) -> ([(FilePath,imd)] -> container_md)
	-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> GetForestMD fs -> ForestI fs (rep',md'))
	-> ForestI fs (ForestFSThunkI fs container_rep,ForestFSThunkI fs container_md)
doLoadCompoundWithConstraint mode pathfilter path matchingM tree pred buildContainerRep buildContainerMd load = mkThunks tree $ debug ("doLoadCompound: "++show path) $ do
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
		(rep,md) <- doLoadFocus pathfilter path n tree (const2 $ return n_md) $ load n t
		return (n,(rep,mkMDArgs mode md (t,u)))
	loadlist <- mapM loadEach filesmetasInfo'
	let replist = map (id >< fst) loadlist
	let mdlist = map (id >< snd) loadlist
	return (buildContainerRep replist,buildContainerMd mdlist)

-- ** auxiliary functions

-- tries to pick a file that has been moved
pickFileNoDelta :: ICRep fs => FilePathFilter fs -> FilePath -> [FileName] -> FSTree fs -> ForestI fs FileName
pickFileNoDelta pathfilter path' files' tree' = do
	return $ pickFile files'

checkPathData :: (ICRep fs,ForestInput fs FSThunk Inside,Eq rep,MData NoCtx (ForestI fs) rep) => FilePath -> FSTree fs -> ForestI fs rep -> ForestI fs (ForestFSThunkI fs rep)
checkPathData path tree ifExists = mod $ checkPathData' False path tree ifExists
checkPathMeta :: (MData NoCtx (ForestI fs) md,Eq md,ForestMD fs md) => FilePath -> FSTree fs -> ForestI fs md -> ForestI fs (ForestFSThunkI fs md)
checkPathMeta path tree ifExists = mod $ checkPathMeta' False path tree ifExists
checkPath :: (ICRep fs,MData NoCtx (ForestI fs) md,Eq rep,Eq md,MData NoCtx (ForestI fs) rep,ForestMD fs md) => FilePath -> FSTree fs -> ForestI fs (rep,md) -> ForestI fs (ForestFSThunkI fs rep,ForestFSThunkI fs md)
checkPath path tree ifExists = do
	ifExistsThunk <- newHSThunk ifExists
	dataThunk <- checkPathData path tree $ getRep $ Inc.read ifExistsThunk
	metaThunk <- checkPathMeta path tree $ getMd $ Inc.read ifExistsThunk
	return (dataThunk,metaThunk)
	
checkPathData' :: (ICRep fs,MData NoCtx (ForestI fs) rep,FSRep fs) => Bool -> FilePath -> FSTree fs -> ForestI fs rep -> ForestI fs rep
checkPathData' isDir path tree ifExists = do
	exists <- forestM $ if isDir then doesDirectoryExistInTree path tree else doesFileExistInTree path tree
	if exists then ifExists else forestdefault
checkPathMeta' :: (MData NoCtx (ForestI fs) md,ForestMD fs md) => Bool -> FilePath -> FSTree fs -> ForestI fs md -> ForestI fs md
checkPathMeta' isDir path tree ifExists = {-debug ("checkPathMeta' "++show path ++ showFSTree tree) $ -} do
	let (doesExist,missingErr) = if isDir then (doesDirectoryExistInTree,Pure.missingDirForestErr) else (doesFileExistInTree,Pure.missingPathForestErr)
	exists <- forestM $ doesExist path tree
	if exists
		then ifExists
		else do
			def_md <- forestdefault
			def_md' <- replace_errors def_md $ Prelude.const $ return $ missingErr path
			return def_md'
checkPath' :: (MData NoCtx (ForestI fs) md,MData NoCtx (ForestI fs) rep,ForestMD fs md) => Maybe Bool -> FilePath -> FSTree fs -> ForestI fs (rep,md) -> ForestI fs (rep,md)
checkPath' cond path tree ifExists = {-debug ("checkPath' "++show path ++ showFSTree tree) $ -} do
	exists <- case cond of
		Nothing -> forestM $ doesExistInTree path tree
		Just isDir -> forestM $ if isDir then doesDirectoryExistInTree path tree else doesFileExistInTree path tree
	if exists
		then ifExists
		else do
			def_rep <- forestdefault
			def_md <- forestdefault
			def_md' <- replace_errors def_md $ Prelude.const $ return $ Pure.missingPathForestErr path
			return (def_rep,def_md')
			
checkFileExtension :: (MData NoCtx (ForestI fs) md,MData NoCtx (ForestI fs) rep,ForestMD fs md) => String -> FilePath -> ForestI fs (rep,md) -> ForestI fs (rep,md)
checkFileExtension ext path ifExists = do
	if isSuffixOf ext path
		then ifExists
		else do
			def_rep <- forestdefault
			def_md <- forestdefault
			def_md' <- replace_errors def_md $ Prelude.const $ return $ Pure.wrongFileExtensionForestErr ext path
			return (def_rep,def_md')

mkThunks :: (Typeable rep,Typeable md,ForestInput fs FSThunk Inside,Eq rep,Eq md,ICRep fs) => FSTree fs -> ForestI fs (rep,md) -> ForestI fs (ForestFSThunkI fs rep,ForestFSThunkI fs md)
mkThunks tree load = do
	loadThunk <- newHSThunk load
	dataThunk <- mod $ getRep $ Inc.read loadThunk
	metaThunk <- mod $ getMd $ Inc.read loadThunk
	return (dataThunk,metaThunk)

mkThunksM :: (Typeable rep,Typeable md,ICRep fs,ForestInput fs FSThunk Inside,Eq rep,Eq md) => FSTree fs -> ForestI fs (ForestI fs rep,ForestI fs md) -> ForestI fs (ForestFSThunkI fs rep,ForestFSThunkI fs md)
mkThunksM tree load = do
	(loadData,loadMeta) <- load
	dataThunk <- mod loadData
	metaThunk <- mod loadMeta
	return (dataThunk,metaThunk)

