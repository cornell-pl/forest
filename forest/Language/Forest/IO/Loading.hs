{-# LANGUAGE TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}



module Language.Forest.IO.Loading where

import Prelude hiding (const,read,mod)
import qualified Prelude
import Language.Forest.IO.Utils
import Language.Forest.Syntax
import Language.Forest.FS.Diff
import Language.Forest.ValueDelta
import Language.Pads.Padsc
import Language.Forest.MetaData
import Language.Forest.Generic
import Language.Forest.Errors
import Language.Forest.FS.FSDelta
import Language.Forest.MetaData
import Control.Monad.Incremental hiding (memo)
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
--import Control.Monad.IO.Class
import Data.WithClass.MData

import Language.Forest.IO.Memo

import qualified Control.Exception as CE

import Data.Data
import Data.Maybe
import System.Random
import Data.Proxy


-- | lazy file loading
-- XXX: Pads errors do not contribute to the Forest error count
doLoadFile :: (ForestInput fs FSThunk Inside,Eq md,Eq pads,MData NoCtx (ForestI fs) pads,MData NoCtx (ForestI fs) md,FSRep fs,Pads pads md) => Proxy pads -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> GetForestMD fs -> ForestI fs (ForestFSThunkI fs pads,ForestFSThunkI fs (Forest_md fs,ForestFSThunkI fs md))
doLoadFile repProxy path (oldtree::FSTree fs) df tree getMD = debug ("doLoadFile " ++ show (path,df)) $ do
	let fs = Proxy::Proxy fs
	-- default static loading
	let load_file = do
		parseThunk <- newHSThunk $ debug ("reading file "++show path) $ pathInTree path tree >>= forestIO . parseFile
		rep_thunk <- checkPathData path tree (getRep $ read parseThunk)
		md_thunk <- checkPathMeta path tree $ do
			fmd <- getMD path tree
			md <- fsThunk [tree] $ getMd $ read parseThunk -- to avoid reading the file unless strictly necessary
			return (fmd,md)
		memo path (rep_thunk,md_thunk,()) tree
		return (rep_thunk,md_thunk)
	-- memoized reuse
	let reuse_same_file = do
		mb <- lookupmemo path repProxy
		case mb of
			Just ((old_rep_thunk,old_md_thunk,()),(==oldtree) -> True) -> debug ("memo hit " ++ show path) $ do
				rep_thunk <- copyFSThunks fs proxyInside old_rep_thunk
				md_thunk <- copyFSThunks fs proxyInside old_md_thunk
				memo path (rep_thunk,md_thunk,()) tree -- overrides the old entry
				return (rep_thunk,md_thunk)
			Nothing -> load_file
	-- memoized reuse for moves
	let reuse_other_file from = do
		mb <- lookupmemo from repProxy
		case mb of
			Just ((old_rep_thunk,old_md_thunk,()),(==oldtree) -> True) -> debug ("memo hit " ++ show from) $ do
				rep_thunk <- copyFSThunks fs proxyInside old_rep_thunk
				fmd' <- getMD path tree
				md_thunk <- fsforce old_md_thunk >>= \(fmd::Forest_md fs,md) -> fsref (fmd',md) -- since the old file may come from another location and/or its attributes may have changed
				forestIO $ unmemo fs from repProxy
				memo path (rep_thunk,md_thunk,()) tree
				return (rep_thunk,md_thunk)
			Nothing -> load_file
	case df of
		(isEmptyFSTreeDeltaNodeMay -> True) -> reuse_same_file
		Just (FSTreeChg _ _) -> reuse_other_file path
		Just (FSTreeNew _ (Just from) _) -> reuse_other_file from
		otherwise -> load_file

-- | lazy file loading
-- XXX: Pads specs currently accept a single optional argument and have no incremental loading, so a change in the argument's value requires recomputation
-- XXX: Pads errors do not contribute to the Forest error count
doLoadFile1 :: (ForestMD fs md,Typeable arg,Eq arg,Eq pads,Eq md,MData NoCtx (ForestI fs) pads,MData NoCtx (ForestI fs) md,FSRep fs,Pads1 arg pads md) => Proxy pads -> arg -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> GetForestMD fs -> ForestI fs (ForestFSThunkI fs pads,ForestFSThunkI fs (Forest_md fs,ForestFSThunkI fs md))
doLoadFile1 repProxy arg path (oldtree::FSTree fs) df tree getMD = debug ("doLoadFile1 " ++ show (path,df)) $ do
	let fs = (Proxy::Proxy fs)
	-- default static loading
	let load_file = do
		parseThunk <- newHSThunk $ debug ("reading file "++show path) $ pathInTree path tree >>= forestIO . parseFile1 arg
		rep_thunk <- checkPathData path tree (getRep $ read parseThunk)
		md_thunk <- checkPathMeta path tree $ do
			fmd <- getMD path tree
			md <- fsThunk [tree] $ getMd $ read parseThunk -- to avoid reading the file unless strictly necessary
			return (fmd,md)
		memo path (rep_thunk,md_thunk,arg) tree
		return (rep_thunk,md_thunk)
	-- memoized reuse
	let reuse_same_file = do
		mb <- lookupmemo path repProxy
		case mb of
			Just ((old_rep_thunk,old_md_thunk,(== arg) -> True),(==oldtree) -> True) -> debug ("memo hit " ++ show path) $ do
				rep_thunk <- fsforce old_rep_thunk >>= fsref
				md_thunk <- fsforce old_md_thunk >>= fsref
				memo path (rep_thunk,md_thunk,()) tree -- overrides the old entry
				return (rep_thunk,md_thunk)
			Nothing -> load_file
	-- memoized reuse for moves
	let reuse_other_file from = do
		mb <- lookupmemo from repProxy
		case mb of
			Just ((old_rep_thunk,old_md_thunk,(== arg) -> True),(==oldtree) -> True) -> debug ("memo hit " ++ show from) $ do --XXX: revise this for thunk arguments!!!
				rep_thunk <- copyFSThunks fs proxyInside old_rep_thunk
				fmd' <- getMD path tree
				md_thunk <- fsforce old_md_thunk >>= \(fmd::Forest_md fs,md) -> fsRef (fmd',md) -- since the old file may come from another location and/or its attributes may have changed
				forestIO $ unmemo fs from repProxy
				memo path (rep_thunk,md_thunk,arg) tree
				return (rep_thunk,md_thunk)
			Nothing -> load_file
	case df of
		(isEmptyFSTreeDeltaNodeMay -> True) -> reuse_same_file
		Just (FSTreeChg _ _) -> reuse_other_file path
		Just (FSTreeNew _ (Just from) _) -> reuse_other_file from
		otherwise -> load_file

-- | compressed archive (tar,gz,zip)
-- incremental loading is only supported if the specification for the archive's contents is:
-- 1) closed = does not depend on free variables -- this ensures that specs can be reused locally
-- 2) static = its type contains no @ICThunk@s inside (or more specifically, only constant @ICThunk@s) -- this is due to a limitation that we cannot consistently copy an @ICThunk@ and its dependencies.
-- The copy operation needs to make a deep strict copy of the argument value, to ensure that we are copying the correct state of a @FSThunk@ and its recursively contained @FSThunk@s
-- XXX: we are actually disregarding the filepaths that occur in the forest metadata for the contents of the archived specification
doLoadArchive :: (ForestMD fs md,Eq rep,Eq md,MData NoCtx (ForestI fs) rep,FSRep fs,MData NoCtx (ForestI fs) md) =>
	String -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> GetForestMD fs
	-> (FilePath -> GetForestMD fs -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ForestI fs (rep,md))
	-> (FilePath -> FilePath -> OldData fs rep md -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ForestO fs (SValueDelta rep,SValueDelta md))
	-> ForestI fs (ForestFSThunkI fs rep,ForestFSThunkI fs (Forest_md fs,md))
doLoadArchive ext path oldtree df tree getMD load loadD = do
	-- static loading
	let load_folder = mkThunks tree $ doLoadArchive' ext path oldtree df tree getMD load
	-- memoized reuse
--	let reuse_same_file = do
--		mb <- lookupmemo path
--		case mb of
--			Just ((old_rep_thunk,old_md_thunk,()),(==oldtree) -> True) -> debug ("memo hit " ++ show path) $ do
--				memo path (rep_thunk,md_thunk,()) tree
--				return (rep_thunk,md_thunk)
--			Nothing -> load_folder
	-- memoized reuse for moves
--	let reuse_other_file from = do
--		mb <- lookupmemo from
--		case mb of
--			Just ((old_rep_thunk,old_md_thunk,()),(==oldtree) -> True) -> debug ("memo hit " ++ show from) $ do
--				-- since we are working at the inner layer, we need to copy the old data into new @FSThunk@s
--				-- note that @copyInc@ is lazy for lazy @FSThunk@s
--				(rep_thunk,md_thunk) <- copyInc (old_rep_thunk,old_md_thunk)
--				rep <- fsforce rep_thunk
--				md@(fmd,imd) <- fsforce md_thunk
--				avfsTree <- virtualTree tree
--				avfsOldTree <- virtualTree oldtree
--				let fromC = cardinalPath from
--				let pathC = cardinalPath path
--				newdf <- focusDiffFSTree oldtree fromC tree pathC
--				
--				-- this is safe because we are modifying fresh modifiables (the copies)
--				unsafeInside $ do
--					loadD fromC (inferSValueDelta fromC pathC) ((new_rep,new_imd),getMD) avfsOldTree newdf avfsTree
--					fmd' <- getMD path tree
--					updateForestMDErrorsWith fmd' $ liftM (:[]) $ get_errors imd'
--					fsset md_thunk (fmd',imd)
--				memo path (rep_thunk,md_thunk,()) tree
--				return (rep_thunk,md_thunk)
--			Nothing -> load_folder
	case df of
--		(isEmptyFSTreeDeltaNodeMay -> True) -> reuse_same_file
--		Just FSTreeChgFile -> reuse_other_file path
--		Just (FSTreeNewFile (Just from)) -> reuse_other_file from
		otherwise -> load_folder

doLoadArchive' :: (ForestMD fs md,MData NoCtx (ForestI fs) rep,FSRep fs,MData NoCtx (ForestI fs) md) =>
	String -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> GetForestMD fs
	-> (FilePath -> GetForestMD fs -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ForestI fs (rep,md))
	-> ForestI fs (rep,(Forest_md fs,md))
doLoadArchive' ext path oldtree df tree getMD load = checkPath' (Just False) path tree $ checkFileExtension ext path $ do
	fmd <- getMD path tree
	avfsTree <- virtualTree tree
	(rep,md_arch) <- load (cardinalPath path) getForestMDInTree avfsTree Nothing avfsTree -- since we use the same tree, there is no problem here
	fmd' <- updateForestMDErrorsInsideWith fmd $ liftM (:[]) $ get_errors md_arch
	return (rep,(fmd',md_arch))
		
doLoadSymLink :: (ForestInput fs FSThunk Inside,FSRep fs) => FilePath -> FSTreeDeltaNodeMay -> FSTree fs -> GetForestMD fs -> ForestI fs (ForestFSThunkI fs FilePath,ForestFSThunkI fs (Forest_md fs, Base_md))
doLoadSymLink path df tree getMD = mkThunks tree $ doLoadSymLink' path df tree getMD

doLoadSymLink' :: (ForestInput fs FSThunk Inside,FSRep fs) => FilePath -> FSTreeDeltaNodeMay -> FSTree fs -> GetForestMD fs -> ForestI fs (FilePath,(Forest_md fs, Base_md))
doLoadSymLink' path df tree getMD = checkPath' Nothing path tree $ do
	md <- getMD path tree
	case symLink (fileInfo md) of
		Just sym -> return (sym,(md,cleanBasePD))
		Nothing -> do
			md' <- updateForestMDErrorsInsideWith md $ return [ioExceptionForestErr]
			return ("", (md',cleanBasePD))

doLoadConstraint :: (ForestOutput fs ICThunk Inside,Eq imd,ForestMD fs imd,MData NoCtx (ForestI fs) rep,md ~ ForestFSThunkI fs imd) => FSTree fs -> ((rep,md) -> ForestI fs Bool) -> ForestI fs (rep,md) -> ForestI fs (rep,(md,ForestICThunkI fs Bool))
doLoadConstraint tree pred load = do -- note that constraints do not consider the current path
	result@(rep,md) <- load
--	md_thunk <- fsThunk [tree] $ do
	cond_thunk <- inside $ thunk $ pred result
	md' <- replace_errors md $ \err -> do
		cond <- force cond_thunk
		if cond
			then return err
			else return $ updateForestErr err [constraintViolationForestErr]
--	return (md',cond_thunk)
	return (rep,(md',cond_thunk))

-- changes the current path
doLoadFocus :: (Matching fs a,ForestMD fs md) => FilePath -> a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> GetForestMD fs -> (FilePath -> FSTreeDeltaNodeMay -> GetForestMD fs -> ForestI fs (rep,md)) -> ForestI fs (rep,md)
doLoadFocus path matching oldtree df tree getMD load = do
	files <- getMatchingFilesInTree path matching tree
	case files of
		[file] -> doLoadNewPath path file oldtree df tree getMD load
		files -> doLoadNewPath path (pickFile files) oldtree df tree getMD $ \newpath newdf newgetMD -> do
			(rep,md) <- load newpath newdf newgetMD
			md' <- if length files == 0
				then return md -- if there is no match then an error will pop from the recursive load
				else addMultipleMatchesErrorMDInside newpath files md
			return (rep,md')

doLoadNewPath :: (FSRep fs) => FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> GetForestMD fs -> (FilePath -> FSTreeDeltaNodeMay -> GetForestMD fs -> ForestI fs x) -> ForestI fs x
doLoadNewPath oldpath file oldtree df tree getMD load = debug ("doLoadNewPath " ++ show (oldpath </> file)) $ do
	newpath <- stepPathInTree tree oldpath file -- changes the old path by a relative path, check the path traversal restrictions specific to each FS instantiation
	let newdf = focusFSTreeDeltaNodeMayByRelativePath df file -- focusing the tree deltas is important for the skipping conditions to fire for unchanged branches of the FS
	load newpath newdf getMD

checkPathData :: (ForestInput fs FSThunk Inside,Eq rep,MData NoCtx (ForestI fs) rep,FSRep fs) => FilePath -> FSTree fs -> ForestI fs rep -> ForestI fs (ForestFSThunkI fs rep)
checkPathData path tree ifExists = fsthunk [tree] $ checkPathData' False path tree ifExists
checkPathMeta :: (MData NoCtx (ForestI fs) md,Eq md,ForestMD fs md) => FilePath -> FSTree fs -> ForestI fs md -> ForestI fs (ForestFSThunkI fs md)
checkPathMeta path tree ifExists = fsthunk [tree] $ checkPathMeta' False path tree ifExists
checkPath :: (MData NoCtx (ForestI fs) md,Eq rep,Eq md,MData NoCtx (ForestI fs) rep,ForestMD fs md) => FilePath -> FSTree fs -> ForestI fs (rep,md) -> ForestI fs (ForestFSThunkI fs rep,ForestFSThunkI fs md)
checkPath path tree ifExists = do
	ifExistsThunk <- newHSThunk ifExists
	dataThunk <- checkPathData path tree $ getRep $ read ifExistsThunk
	metaThunk <- checkPathMeta path tree $ getMd $ read ifExistsThunk
	return (dataThunk,metaThunk)
	
checkPathData' :: (MData NoCtx (ForestI fs) rep,FSRep fs) => Bool -> FilePath -> FSTree fs -> ForestI fs rep -> ForestI fs rep
checkPathData' isDir path tree ifExists = do
	exists <- if isDir then doesDirectoryExistInTree path tree else doesFileExistInTree path tree
	if exists then ifExists else mdefault
checkPathMeta' :: (MData NoCtx (ForestI fs) md,ForestMD fs md) => Bool -> FilePath -> FSTree fs -> ForestI fs md -> ForestI fs md
checkPathMeta' isDir path tree ifExists = {-debug ("checkPathMeta' "++show path ++ showFSTree tree) $ -} do
	let (doesExist,missingErr) = if isDir then (doesDirectoryExistInTree,missingDirForestErr) else (doesFileExistInTree,missingPathForestErr)
	exists <- doesExist path tree
	if exists
		then ifExists
		else do
			def_md <- mdefault
			def_md' <- replace_errors def_md $ Prelude.const $ return $ missingErr path
			return def_md'
checkPath' :: (MData NoCtx (ForestI fs) md,MData NoCtx (ForestI fs) rep,ForestMD fs md) => Maybe Bool -> FilePath -> FSTree fs -> ForestI fs (rep,md) -> ForestI fs (rep,md)
checkPath' cond path tree ifExists = {-debug ("checkPath' "++show path ++ showFSTree tree) $ -} do
	exists <- case cond of
		Nothing -> doesExistInTree path tree
		Just isDir -> if isDir then doesDirectoryExistInTree path tree else doesFileExistInTree path tree
	if exists
		then ifExists
		else do
			def_rep <- mdefault
			def_md <- mdefault
			def_md' <- replace_errors def_md $ Prelude.const $ return $ missingPathForestErr path
			return (def_rep,def_md')
			
checkFileExtension :: (MData NoCtx (ForestI fs) md,MData NoCtx (ForestI fs) rep,ForestMD fs md) => String -> FilePath -> ForestI fs (rep,md) -> ForestI fs (rep,md)
checkFileExtension ext path ifExists = do
	if isSuffixOf ext path
		then ifExists
		else do
			def_rep <- mdefault
			def_md <- mdefault
			def_md' <- replace_errors def_md $ Prelude.const $ return $ wrongFileExtensionForestErr ext path
			return (def_rep,def_md')

mkThunks :: (ForestInput fs FSThunk Inside,Eq rep,Eq md,FSRep fs) => FSTree fs -> ForestI fs (rep,md) -> ForestI fs (ForestFSThunkI fs rep,ForestFSThunkI fs md)
mkThunks tree load = do
	loadThunk <- newHSThunk load
	dataThunk <- fsThunk [tree] $ getRep $ read loadThunk
	metaThunk <- fsThunk [tree] $ getMd $ read loadThunk
	return (dataThunk,metaThunk)

mkThunksM :: (ForestInput fs FSThunk Inside,FSRep fs,Eq rep,Eq md) => FSTree fs -> ForestI fs (ForestI fs rep,ForestI fs md) -> ForestI fs (ForestFSThunkI fs rep,ForestFSThunkI fs md)
mkThunksM tree load = do
	(loadData,loadMeta) <- load
	dataThunk <- fsthunk [tree] loadData
	metaThunk <- fsthunk [tree] loadMeta
	return (dataThunk,metaThunk)

doLoadDirectory :: (ForestInput fs FSThunk Inside,Eq rep,Eq md,MData NoCtx (ForestI fs) rep,FSRep fs,MData NoCtx (ForestI fs) md)
	=> FilePath -> FSTree fs -> (md -> ForestI fs Forest_err) -> GetForestMD fs -> ForestI fs (rep,md) -> ForestI fs (ForestFSThunkI fs rep,ForestFSThunkI fs (Forest_md fs,md))
doLoadDirectory path tree collectMDErrors getMD load = mkThunksM tree $ doLoadDirectory' path tree collectMDErrors getMD load

-- the error count of the directory is computed lazily, so that if we only want, e.g., the fileinfo of the directory we don't need to check its contents
doLoadDirectory' :: (ForestInput fs FSThunk Inside,Eq rep,Eq md,MData NoCtx (ForestI fs) rep,FSRep fs,MData NoCtx (ForestI fs) md)
	=> FilePath -> FSTree fs -> (md -> ForestI fs Forest_err) -> GetForestMD fs -> ForestI fs (rep,md) -> ForestI fs (ForestI fs rep,ForestI fs (Forest_md fs,md))
doLoadDirectory' path tree collectMDErrors getMD ifGood = debug ("doLoadDirectory: "++show path) $ do
	ifGoodThunk <- newHSThunk ifGood
	let loadData = checkPathData' True path tree $ getRep $ read ifGoodThunk
	let loadMeta = checkPathMeta' True path tree $ do
		fmd <- getMD path tree
		mds <- getMd $ read ifGoodThunk
		fmd' <- updateForestMDErrorsInsideWith fmd $ liftM (:[]) $ collectMDErrors mds
		return (fmd',mds)
	return (loadData,loadMeta)

doLoadMaybe :: (Eq rep,Eq md,ForestMD fs md) => FilePath -> FSTreeDeltaNodeMay -> FSTree fs -> ForestI fs (rep,md) -> ForestI fs (ForestFSThunkI fs (Maybe rep),ForestFSThunkI fs (Forest_md fs,Maybe md))
doLoadMaybe path df tree ifExists = mkThunksM tree $ doLoadMaybe' path df tree ifExists

doLoadMaybe' :: (Eq rep,Eq md,ForestMD fs md) => FilePath -> FSTreeDeltaNodeMay -> FSTree fs -> ForestI fs (rep,md) -> ForestI fs (ForestI fs (Maybe rep),ForestI fs (Forest_md fs,Maybe md))
doLoadMaybe' path df tree ifExists = do
	ifExistsThunk <- newHSThunk ifExists
	let loadData = do
		exists <- doesExistInTree path tree
		if exists
			then liftM Just $ getRep $ read ifExistsThunk
			else return Nothing
	let loadMeta = do
		exists <- doesExistInTree path tree
		debug ("doLoadMaybe: "++show (path,exists)) $ if exists
			then do
				md <- getMd $ read ifExistsThunk
				fmd <- get_fmd_header md
				return (fmd,Just md) -- use the same @Forest_md@
			else do
				fmd <- cleanForestMDwithFile path
				return (fmd,Nothing)
	return (loadData,loadMeta)

-- since the focus changes we need to compute the (eventually) previously loaded metadata of the parent node
doLoadSimple :: (Eq imd',ForestMD fs imd',Matching fs a,MData NoCtx (ForestI fs) rep',ForestMD fs md', md' ~ ForestFSThunkI fs imd') =>
	FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> (FilePath -> FSTreeDeltaNodeMay -> GetForestMD fs -> ForestI fs (rep',md'))
	-> ForestI fs (rep',md')
doLoadSimple path matching oldtree df tree load = matching >>= \m -> doLoadFocus path m oldtree df tree getForestMDInTree load

-- since the focus changes we need to compute the (eventually) previously loaded metadata of the parent node
doLoadSimpleWithConstraint :: (ForestOutput fs ICThunk Inside,Eq imd',ForestMD fs imd',Matching fs a,MData NoCtx (ForestI fs) rep',ForestMD fs md', md' ~ ForestFSThunkI fs imd') =>
	FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ((rep',md') -> ForestI fs Bool)
	-> (FilePath -> FSTreeDeltaNodeMay -> GetForestMD fs -> ForestI fs (rep',md'))
	-> ForestI fs (rep',(md',ForestICThunkI fs Bool))
doLoadSimpleWithConstraint path matching oldtree df tree pred load = doLoadConstraint tree pred $ matching >>= \m -> doLoadFocus path m oldtree df tree getForestMDInTree load

doLoadCompound :: (ForestMD fs (md',FSThunk fs Inside (IncForest fs) IORef IO FileInfo),Matching fs a,MData NoCtx (ForestI fs) rep',ForestMD fs md', imd ~ (md',ForestFSThunkI fs FileInfo)) =>
	FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> ([(FilePath,rep')] -> container_rep) -> ([(FilePath,imd)] -> container_md)
	-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> FSTreeDeltaNodeMay -> GetForestMD fs -> ForestI fs (rep',md'))
	-> ForestI fs (container_rep,container_md)
doLoadCompound path matchingM oldtree df tree buildContainerRep buildContainerMd load = debug ("doLoadCompound: "++show path) $ do
	matching <- matchingM
	files <- getMatchingFilesInTree path matching tree
	metadatas <- mapM (getRelForestMDInTree path tree) files
	let filesmetas = zip files metadatas
	let loadEach (n,n_md) = liftM (n,) $ doLoadFocus path n oldtree df tree (const2 $ return n_md) $ \newpath newdf newGetMD -> do
		fileInfo_thunk <- fsref $ fileInfo n_md
		(rep',md') <- load n fileInfo_thunk newpath newdf newGetMD
		return (rep',(md',fileInfo_thunk))
	loadlist <- mapM loadEach filesmetas
	let replist = map (id >< fst) loadlist
	let mdlist = map (id >< snd) loadlist
	return (buildContainerRep replist,buildContainerMd mdlist)

doLoadCompoundWithConstraint :: (ForestOutput fs ICThunk Inside,Matching fs a,MData NoCtx (ForestI fs) rep',ForestMD fs md',imd ~ (md',(ForestFSThunkI fs FileInfo,ForestICThunkI fs Bool)) ) =>
	FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> (FilePath -> ForestFSThunkI fs FileInfo -> ForestI fs Bool)
	-> ([(FilePath,rep')] -> container_rep) -> ([(FilePath,imd)] -> container_md)
	-> (FileName -> ForestFSThunkI fs FileInfo -> FilePath -> FSTreeDeltaNodeMay -> GetForestMD fs -> ForestI fs (rep',md'))
	-> ForestI fs (container_rep,container_md)
doLoadCompoundWithConstraint path matchingM oldtree df tree pred buildContainerRep buildContainerMd load = debug ("doLoadCompound: "++show path) $ do
	matching <- matchingM -- matching expressions are not saved for incremental reuse
	files <- getMatchingFilesInTree path matching tree
	metadatas <- mapM (getRelForestMDInTree path tree) files
	let filesmetas = zip files metadatas
	let makeInfo (n,fmd) = do
		t <- ref $ fileInfo fmd -- we store the @FileInfo@ in a @FSThunk@ to allow incremental evaluation of the constraint expression
		u <- thunk $ pred n t --the filename is a constant. during delta loading, whenever it changes we will load from scratch
		return (n,(fmd,(t,u)))
	filesmetasInfo <- mapM makeInfo filesmetas
	filesmetasInfo' <- filterM (force . snd . snd . snd) filesmetasInfo
	let loadEach (n,(n_md,(t,u))) = do
		(rep,md) <- doLoadFocus path n oldtree df tree (const2 $ return n_md) $ load n t
		return (n,(rep,(md,(t,u))))
	loadlist <- mapM loadEach filesmetasInfo'
	let replist = map (id >< fst) loadlist
	let mdlist = map (id >< snd) loadlist
	return (buildContainerRep replist,buildContainerMd mdlist)

-- tries to pick a file that has been moved
pickFileNoDelta :: FSRep fs => FilePath -> [FileName] -> FSTreeDeltaNodeMay -> FSTree fs -> ForestI fs FileName
pickFileNoDelta path' files' df tree' = do
	let reorderFile xs file' = case focusFSTreeDeltaNodeMayByRelativePath df file' of
		Just (FSTreeNew _ (Just from) _) -> file' : xs
		otherwise -> xs ++ [file']
	let files'' = foldl' reorderFile [] files'
	return $ pickFile files''


