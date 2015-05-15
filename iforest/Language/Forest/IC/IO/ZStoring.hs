{-# LANGUAGE ConstraintKinds, TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}

module Language.Forest.IC.IO.ZStoring where

import Language.Pads.Generic as Pads
import Control.Monad.Trans
import Control.Monad.Writer (Writer(..),WriterT(..))
import qualified Control.Monad.Writer as Writer
import Language.Forest.Manifest
import Prelude hiding (const,read,mod)
import qualified Prelude
import Language.Forest.IO.Utils
import Language.Forest.Syntax
import Language.Forest.FS.Diff
import Language.Forest.IC.ValueDelta
import Language.Forest.IC.ICRep
import Language.Pads.Padsc hiding (lift,numErrors)
import qualified Language.Pads.Padsc as Pads
import Language.Forest.IC.MetaData
import Language.Forest.IC.Default
import Language.Forest.IC.Generic
--import Language.Forest.IC.IO.Storing
import Language.Forest.Errors
import Language.Forest.FS.FSDelta
--import Language.Forest.MetaData (FileInfo(..),FileType(..),(:*:)(..))
--import qualified Language.Forest.MetaData as Pure
import Control.Monad.Incremental as Inc hiding (memo)
import Data.IORef
import Data.List as List
import Language.Forest.IC.BX as BX
import Language.Forest.IO.Shell

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
import Data.Monoid
import Data.List
import Language.Forest.FS.FSRep
--import Control.Monad.IO.Class
import Data.WithClass.MData
import Control.Monad.Reader (Reader(..),ReaderT(..))
import qualified Control.Monad.Reader as Reader
import qualified Control.Exception as CE

import Data.Data
import Data.Maybe
import System.Random
import Data.Proxy
import Data.WithClass.MGenerics.Twins

doZManifestNamed :: (FTK fs rep,ZippedICForest fs rep) =>
	Proxy rep -> ForestIs fs (ForestArgs rep) -> FilePath -> FSTree fs -> (ForestFSThunkI fs rep) -> Manifest fs -> MManifestForestO fs
doZManifestNamed proxy args path tree rep man = do
	irep <- lift $ Inc.getOutside rep
	zupdateManifestScratchGeneric args path tree irep man

-- adds consistency checks for top-level specification arguments
doZManifestArgs :: (ICRep fs,ForestArgsClass fs args) =>
	Proxy args -> ForestIs fs args
	-> rep
	-> (rep -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZManifestArgs proxy margs rep manifestContent man = do
	manifestContent rep man

doZManifestFile1 :: (
	ForestRepTy fs (ForestFSThunkI fs ((Forest_md fs,md),pads)) ~ ((Forest_md fs,md),pads)
	,ForestContentTy fs ((Forest_md fs,md),pads) ~ ((FileInfo,md),padsc)
	,ForestArgs (ForestFSThunkI fs ((Forest_md fs,md),pads)) ~Arg arg,FTK fs (ForestFSThunkI fs ((Forest_md fs,md),pads))
	,MData NoCtx (ForestI fs) arg,IncK (IncForest fs) Forest_err,IncK (IncForest fs) ((Forest_md fs, md), pads),Typeable arg,ZippedICMemo fs,ICRep fs,Pads1 arg pads md)
	=> ForestI fs arg -> FilePath -> FSTree fs -> ForestFSThunkI fs ((Forest_md fs,md),pads) -> Manifest fs -> MManifestForestO fs
doZManifestFile1 (marg :: ForestI fs arg) path tree rep_t man = do
	
	repairMd <- Reader.ask
	let argProxy = Proxy :: Proxy (Arg arg)
	let fsrepProxy = Proxy
	let fs = (Proxy::Proxy fs)
	
	let mani_scratch = do
		rep <- lift $ Inc.getOutside rep_t
		doZManifestFileInner1 marg path tree rep man
	
	mb <- lift $ inside $ findZippedMemo path fsrepProxy 
	newman <- case mb of
		(Just (memo_tree,memo_marg,(== rep_t) -> True)) -> do
			memo_arg <- lift $ inside memo_marg
			arg <- lift $ inside marg
			-- deep equality of arguments, since thunks can be arguments and change
			samearg <- lift $ inside $ geq proxyNoCtx memo_arg arg
			if samearg
				then do
					debug ("memo hit " ++ show path) $ do
					df <- lift $ forestM $ diffFS memo_tree tree path
					dv <- lift $ diffValueThunk memo_tree rep_t
					case (isIdValueDelta dv,df) of
						(True,Just (isEmptyFSTreeD fs -> True)) -> return man
						otherwise -> mani_scratch
				else mani_scratch
		otherwise -> mani_scratch
	Writer.tell $ inside . addZippedMemo path marg rep_t . Just
	return newman

doZManifestFileInner1 :: (IncK (IncForest fs) FileInfo,MData NoCtx (ForestI fs) arg,IncK (IncForest fs) Forest_err,IncK (IncForest fs) ((Forest_md fs, md), pads),Typeable arg,ZippedICMemo fs,ICRep fs,Pads1 arg pads md) => ForestI fs arg -> FilePath -> FSTree fs -> ((Forest_md fs,md),pads) -> Manifest fs -> MManifestForestO fs
doZManifestFileInner1 (marg :: ForestI fs arg) path tree ((fmd,bmd),pads) man = do
	
	repairMd <- Reader.ask
	let argProxy = Proxy :: Proxy (Arg arg)
	let fsrepProxy = Proxy
	let fs = (Proxy::Proxy fs)
	
	let mani_scratch = do
		arg <- lift $ inside marg
		path_fmd <- lift $ inside $ get_fullpath fmd
		exists <- lift $ doesFileExistInMD fmd
		newman <- if (exists && Pads.numErrors (get_md_header bmd) == 0)
			then do
				let testm = do
					status1 <- liftM (boolStatus $ ConflictingMdValidity) $ return $ Pads.numErrors (get_md_header bmd) == 0
					status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
					return $ status1 `mappend` status2
				(man1,printF) <- if repairMd
					then do
						Writer.tell $ Prelude.const $ replaceForestMDErrorsWithPadsMD fmd (return $ get_md_header bmd) -- adds the Pads errors
						return (man,\p -> printFile1 arg p (pads,bmd))
					else return (addTestToManifest testm man,\p -> printFile1 arg p (pads,bmd))
				lift $ forestM $ addFileToManifestInTree printF path tree man1
			else do
				-- inconsistent unless the non-stored representation has default data
				let testm = do
					status1 <- liftM (boolStatus ConflictingRepMd) $ forestO $ do
					 	exists <- doesFileExistInMD fmd
						return $ (not exists) || (Pads.numErrors (get_md_header bmd) > 0)
					status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
					return $ status1 `mappend` status2
				man1 <- if repairMd
					then do
						Writer.tell $ Prelude.const $ do
							replaceForestMDErrorsWith fmd $ return [missingPathForestErr path]
							updateForestMDErrorsWithPadsMD fmd (return $ get_md_header bmd) -- adds the Pads errors
						return man
					else return $ addTestToManifest testm man
				lift $ forestM $ removePathFromManifestInTree path tree man1
		return newman
	
	mani_scratch

doZManifestArchive :: (IncK (IncForest fs) (Forest_md fs, rep),Typeable rep,ZippedICMemo fs,toprep ~ ForestFSThunkI fs (Forest_md fs,rep),ForestMD fs rep,ForestInput fs FSThunk Inside,ICRep fs) =>
	Bool -> [ArchiveType] -> FilePath -> FSTree fs 
	-> toprep
	-> (FilePath -> FSTree fs -> rep -> Manifest fs -> MManifestForestO fs)
	-> (FilePath -> FilePath -> FSTree fs -> FSTreeD fs -> FSTree fs -> rep -> ValueDelta fs rep -> Manifest fs -> MManifestForestO fs)
	-> (FSTree fs -> rep -> ForestO fs (ValueDelta fs rep))
	-> Manifest fs -> MManifestForestO fs
doZManifestArchive isClosed archTy path tree toprep manifest manifestD diffValue man = do
	isRepairMd <- Reader.ask
	let argsProxy = Proxy :: Proxy ()
	let repProxy = Proxy
	(fmd,rep) <- lift $ Inc.getOutside toprep
	
	path_fmd <- lift $ get_fullpath fmd
	exists <- lift $ doesFileExistInMD fmd
	if exists
		then do
			canpath <- lift $ forestM $ canonalizeDirectoryInTree path tree
			dskpath <- lift $ forestM $ pathInTree canpath tree
			let arch_canpath = cardinalPath canpath
			avfsTree <- lift $ forestM $ virtualTree tree
			
			archiveDir <- lift $ forestM $ forestIO $ getTempPath -- unlogged temporary directory, since we remove it ourselves
			archiveManifest <- lift $ forestM $ newManifestWith arch_canpath tree
			lift $ forestM $ forestIO $ CE.onException (decompressArchive archTy dskpath archiveDir) (createDirectoryIfMissing True archiveDir) -- decompress the original content, since some of it may be preserved in the new archive
			
			let mani_scratch = manifest arch_canpath avfsTree rep archiveManifest
			
			archiveManifest' <- mani_scratch --if isClosed
--				then do
--					mb <- lift $ inside $ findZippedMemo argsProxy path repProxy
--					rep <- case mb of
--						Just (memo_tree,(),(== toprep) -> True) -> do
--							md@(fmd,irep) <- lift $ Inc.getOutside toprep
--							avfsOldTree <- lift $ forestM $ virtualTree memo_tree
--							archiveDf <- lift $ forestM $ focusDiffFSTreeD memo_tree arch_canpath tree arch_canpath
--							dv <- lift $ diffValue memo_tree irep
--							manifestD arch_canpath arch_canpath avfsOldTree archiveDf avfsTree irep dv archiveManifest
--						Nothing -> mani_scratch
--					Writer.tell $ inside . addZippedMemo path argsProxy () toprep . Just
--					return rep
--				else mani_scratch
			
			-- NOTE: we only need to commit the writes that contribute to the new archive, inside the forest temp dir; if we chose otherwise we could unsafely commit to the filesystem!
			man1 <- lift $ forestM $ storeManifestAt archiveDir archiveManifest' -- store the manifest at the temp dir, and return all the modifications outside the archive
			archiveFile <- lift $ forestM tempPath
			lift $ forestM $ forestIO $ compressArchive archTy archiveDir archiveFile -- compresses the new data into a new temp file
			lift $ forestM $ forestIO $ removePath archiveDir -- purges all temporary archive data 
			
			let testm = do
				isValid <- forestO $ isValidMD fmd
				path_fmd <- forestO $ get_fullpath fmd
				status1 <- if isRepairMd then return Valid else liftM (boolStatus ConflictingMdValidity) $ forestO $ sameValidity fmd rep
				status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path	
				return $ status1 `mappend` status2
			
			Writer.tell $ \latest -> replaceForestMDErrorsWith fmd $ liftM (:[]) $ get_errors rep
				
			let man2 = addTestToManifest testm man -- errors in the metadata must be consistent
			
			abspath <- lift $ forestM $ forestIO $ absolutePath path
			let man3 = addFileToManifest' canpath abspath archiveFile man2
			
			return $ mergeManifests man1 man3
		else do
			man1 <- lift $ forestM $ removePathFromManifestInTree path tree man -- remove the archive
			let testm = do
				status1 <- if isRepairMd then return Valid else liftM (boolStatus $ ConflictingMdValidity) $ forestO $ inside (get_errors rep) >>= sameValidity' fmd
				status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
				return $ status1 `mappend` status2
			Writer.tell $ \latest -> replaceForestMDErrorsWith fmd $ liftM (:[]) $ liftM (missingPathForestErr path `mergeForestErrs`) $ get_errors rep
			return $ addTestToManifest testm man1

doZManifestArchiveInner :: (ForestMD fs rep,ForestInput fs FSThunk Inside,ICRep fs) =>
	[ArchiveType] -> FilePath -> FSTree fs 
	-> (Forest_md fs,rep)
	-> (FilePath -> FSTree fs -> rep -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZManifestArchiveInner archTy path tree (fmd,rep) manifestContents man = do
	isRepairMd <- Reader.ask
	path_fmd <- lift $ inside $ get_fullpath fmd
	exists <- lift $ doesFileExistInMD fmd
	if exists
		then do
			canpath <- lift $ forestM $ canonalizeDirectoryInTree path tree
			dskpath <- lift $ forestM $ pathInTree canpath tree
			let arch_canpath = cardinalPath canpath
			avfsTree <- lift $ forestM $ virtualTree tree
			
			archiveDir <- lift $ forestM $ forestIO $ getTempPath -- unlogged temporary directory, since we remove it ourselves
			archiveManifest <- lift $ forestM $ newManifestWith arch_canpath tree
			lift $ forestM $ forestIO $ CE.onException (decompressArchive archTy dskpath archiveDir) (createDirectoryIfMissing True archiveDir) -- decompress the original content, since some of it may be preserved in the new archive
			
			archiveManifest' <- manifestContents arch_canpath avfsTree rep archiveManifest
			
			-- NOTE: we only need to commit the writes that contribute to the new archive, inside the forest temp dir; if we chose otherwise we could unsafely commit to the filesystem!
			man1 <- lift $ forestM $ storeManifestAt archiveDir archiveManifest' -- store the manifest at the temp dir, and return all the modifications outside the archive
			archiveFile <- lift $ forestM tempPath
			lift $ forestM $ forestIO $ compressArchive archTy archiveDir archiveFile -- compresses the new data into a new temp file
			lift $ forestM $ forestIO $ removePath archiveDir -- purges all temporary archive data 
			
			let testm = do
				status1 <- if isRepairMd then return Valid else liftM (boolStatus $ ConflictingMdValidity) $ forestO $ inside (get_errors rep) >>= sameValidity' fmd
				status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
				return $ status1 `mappend` status2
			Writer.tell $ Prelude.const $ replaceForestMDErrorsWith fmd $ liftM (:[]) $ get_errors rep
			let man2 = addTestToManifest testm man -- errors in the metadata must be consistent
			
			abspath <- lift $ forestM $ forestIO $ absolutePath path
			let man3 = addFileToManifest' canpath abspath archiveFile man2
			return $ mergeManifests man1 man3
		else do
			man1 <- lift $ forestM $ removePathFromManifestInTree path tree man -- remove the archive
			let testm = do
				status1 <- if isRepairMd then return Valid else liftM (boolStatus $ ConflictingMdValidity) $ forestO $ inside (get_errors rep) >>= sameValidity' fmd
				status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
				return $ status1 `mappend` status2
			Writer.tell $ Prelude.const $ replaceForestMDErrorsWith fmd $ liftM (:[]) $ liftM (missingPathForestErr path `mergeForestErrs`) $ get_errors rep
			return $ addTestToManifest testm man1

doZManifestSymLink :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,IncK (IncForest fs) ((Forest_md fs, Base_md), FilePath),ICRep fs) =>
	FilePath -> FSTree fs
	-> ForestFSThunkI fs ((Forest_md fs,Base_md),FilePath)
	-> Manifest fs -> MManifestForestO fs
doZManifestSymLink path tree (rep_t) man = do
	((fmd,base_md), tgt) <- lift $ Inc.getOutside rep_t
	path_fmd <- lift $ inside $ get_fullpath fmd
	exists <- lift $ doesLinkExistInMD fmd
	case exists of
		Just sym -> do
			let testm = do
				status1 <- liftM (boolStatus $ ConflictingLink path tgt $ Just sym) $ return $ sym == tgt && base_md == cleanBasePD
				status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
				return $ status1 `mappend` status2
			Writer.tell $ Prelude.const $ replaceForestMDErrorsWith fmd $ return [cleanForestErr]
			lift $ forestM $ addLinkToManifestInTree path tree tgt $ addTestToManifest testm man
		Nothing -> do
			let testm = do
				status1 <- liftM (boolStatus $ ConflictingLink path tgt Nothing) $ return $ tgt == "" && base_md == cleanBasePD
				status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
				return $ status1 `mappend` status2
			Writer.tell $ Prelude.const $ replaceForestMDErrorsWith fmd $ return [ioExceptionForestErr]
			lift $ forestM $ removePathFromManifestInTree path tree $ addTestToManifest testm man

doZManifestSymLinkInner :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,IncK (IncForest fs) ((Forest_md fs, Base_md), FilePath),ICRep fs) =>
	FilePath -> FSTree fs
	-> ((Forest_md fs,Base_md),FilePath)
	-> Manifest fs -> MManifestForestO fs
doZManifestSymLinkInner path tree ((fmd,base_md), tgt) man = do
	path_fmd <- lift $ inside $ get_fullpath fmd
	exists <- lift $ doesLinkExistInMD fmd
	case exists of
		Just sym -> do
			let testm = do
				status1 <- liftM (boolStatus $ ConflictingLink path tgt $ Just sym) $ return $ sym == tgt && base_md == cleanBasePD
				status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
				return $ status1 `mappend` status2
			Writer.tell $ Prelude.const $ replaceForestMDErrorsWith fmd $ return [cleanForestErr]
			lift $ forestM $ addLinkToManifestInTree path tree tgt $ addTestToManifest testm man
		Nothing -> do
			let testm = do
				status1 <- liftM (boolStatus $ ConflictingLink path tgt Nothing) $ return $ tgt == "" && base_md == cleanBasePD
				status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
				return $ status1 `mappend` status2
			Writer.tell $ Prelude.const $ replaceForestMDErrorsWith fmd $ return [ioExceptionForestErr]
			lift $ forestM $ removePathFromManifestInTree path tree $ addTestToManifest testm man

doZManifestConstraint :: (ForestContent fs rep,IncK (IncForest fs) (ForestFSThunkI fs Forest_err, rep),ForestMD fs rep,ICRep fs) =>
	(ForestContentTy fs rep -> ForestI fs Bool) -> ForestFSThunkI fs (ForestFSThunkI fs Forest_err,rep)
	-> (rep -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZManifestConstraint pred t manifestContent man = lift (Inc.getOutside t) >>= \v -> doZManifestConstraintInner pred v manifestContent man

doZManifestConstraintInner :: (ForestContent fs rep,ForestMD fs rep,ICRep fs) =>
	(ForestContentTy fs rep -> ForestI fs Bool) -> (ForestFSThunkI fs Forest_err,rep)
	-> (rep -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZManifestConstraintInner pred (err_t,rep) manifestContent man = do
	isRepairMd <- Reader.ask
	let testm = do -- constraint errors need to be accounted for
		if isRepairMd then return Valid else liftM (boolStatus $ ConflictingMdValidity) $ forestO $ inside $ do
			err_cond <- predForestErr . pred =<< BX.getM lens_content (return rep)
			err_inner <- get_errors rep
			errors <- Inc.get err_t
			return $ isValidForestErr errors == isValidForestErr (mergeForestErrs err_cond err_inner)
	
	Writer.tell $ \latest -> do
		overwrite err_t $ do
			err_cond <- predForestErr . pred =<< BX.getM lens_content (return rep)
			err_inner <- get_errors rep
			return $ mergeForestErrs err_cond err_inner
	
	manifestContent rep $ addTestToManifest testm man

-- constraint satisfaction is mandatory: the result of loading never contains elements that don't satisfy the filtering constraint
doZManifestConstraintCompound :: (ForestMD fs rep,ICRep fs) =>
	ForestI fs Bool -> rep
	-> (rep -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZManifestConstraintCompound pred rep manifestContent man = do
	let testm = do -- constraint errors need to be accounted for
		liftM (boolStatus $ ConflictingMdValidity) $ forestO $ inside $ pred
	
	manifestContent rep $ addTestToManifest testm man

-- on storing we make sure that the error thunk is a computation on the representation, instead of an arbitrary user-defined value.
-- we need to preserve this invariant to enable incremental storing
-- field manifest functions take the directory path
doZManifestDirectory :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,IncK (IncForest fs) (Forest_md fs, rep),ICRep fs) => 
	FilePath -> FSTree fs -> (rep -> ForestI fs Forest_err)
	-> ForestFSThunkI fs (Forest_md fs,rep)
	-> (rep -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZManifestDirectory path tree collectMDErrors rep_t manifestContent man = lift (Inc.getOutside rep_t) >>= \v -> doZManifestDirectoryInner path tree collectMDErrors v manifestContent man

doZManifestDirectoryInner :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ICRep fs) => 
	FilePath -> FSTree fs -> (rep -> ForestI fs Forest_err)
	-> (Forest_md fs,rep)
	-> (rep -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZManifestDirectoryInner path tree collectMDErrors (fmd,rep) manifestContent man = do
	isRepairMd <- Reader.ask
	path_fmd <- lift $ inside $ get_fullpath fmd
	
	exists <- lift $ doesDirectoryExistInMD fmd
	if exists
		then do
			man1 <- lift $ forestM $ addDirToManifestInTree path tree man -- adds a new directory
			let testm = do
				status1 <- if isRepairMd then return Valid else liftM (boolStatus $ ConflictingMdValidity) $ forestO $ inside (collectMDErrors rep) >>= sameValidity' fmd
				status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
				return $ status1 `mappend` status2
			Writer.tell $ \latest -> replaceForestMDErrorsWith fmd $ liftM (:[]) $ collectMDErrors rep
			manifestContent rep $ addTestToManifest testm man1
		else do
			man1 <- lift $ forestM $ removePathFromManifestInTree path tree man -- remove the directory
			let testm = do
				status1 <- if isRepairMd then return Valid else liftM (boolStatus $ ConflictingMdValidity) $ forestO $ inside (collectMDErrors rep) >>= sameValidity' fmd
				status2 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
				return $ status1 `mappend` status2
			Writer.tell $ \latest -> replaceForestMDErrorsWith fmd $ liftM (:[]) $ liftM (missingDirForestErr path `mergeForestErrs`) $ collectMDErrors rep
			return $ addTestToManifest testm man1

doZManifestMaybe :: (IncK (IncForest fs) (Forest_md fs, Maybe rep),ForestMD fs rep,ICRep fs) =>
	FilePath -> FSTree fs
	-> ForestFSThunkI fs (Forest_md fs,Maybe rep)
	-> (rep -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZManifestMaybe path tree rep_t manifestContent man = doZManifestMaybe' path tree rep_t (\t -> Inc.get t >>= \(fmd,rep) -> return (Just fmd,rep)) manifestContent man

doZManifestMaybeInner :: (ForestMD fs rep,ICRep fs) =>
	FilePath -> FSTree fs
	-> Maybe rep
	-> (rep -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZManifestMaybeInner path tree rep manifestContent man = doZManifestMaybe' path tree rep (\rep -> return (Nothing,rep)) manifestContent man

doZManifestMaybe' :: (ForestMD fs rep,ICRep fs) =>
	FilePath -> FSTree fs
	-> toprep -> (toprep -> ForestI fs (Maybe (Forest_md fs),Maybe rep))
	-> (rep -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZManifestMaybe' path tree toprep getRep manifestContent man = do
	isRepairMd <- Reader.ask
	(mb_fmd,rep_mb) <- lift $ inside $ getRep toprep
	
	-- guarantee that the error thunk is being updated on inner changes 
	case mb_fmd of
		Nothing -> return ()
		Just fmd -> Writer.tell $ Prelude.const $ get_errors_thunk fmd >>= flip overwrite (maybe (return cleanForestErr) get_errors rep_mb)
	
	case rep_mb of
		Just rep -> do
			let testm = do
				status1 <- case mb_fmd of
					Just fmd -> do
						path_fmd <- forestO $ get_fullpath fmd
						status11 <- if isRepairMd then return Valid else liftM (boolStatus ConflictingMdValidity) $ forestO $ sameValidity fmd rep
						status12 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
						return $ status11 `mappend` status12
					otherwise -> return Valid
				status2 <- liftM (boolStatus $ NonExistingPath path) $ latestTree >>= doesExistInTree path
				return $ status1 `mappend` status2
			manifestContent rep $ addTestToManifest testm man -- the path will be added recursively
		Nothing -> do
			let testm = do
				status1 <- liftM (boolStatus $ ExistingPath path) $ latestTree >>= liftM not . doesExistInTree path
				status2 <- case mb_fmd of
					Just fmd -> do
						path_fmd <- forestO $ get_fullpath fmd
						status21 <- if isRepairMd then return Valid else liftM (boolStatus ConflictingRepMd) $ forestO $ inside $ liftM (==fmd) $ cleanForestMDwithFile path
						status22 <- liftM (boolStatus $ ConflictingPath path path_fmd) $ latestTree >>= sameCanonicalFullPathInTree path_fmd path
						return $ status21 `mappend` status22
					otherwise -> return Valid
				return $ status1 `mappend` status2
			return $ addTestToManifest testm man

doZManifestFocus :: (ICRep fs,Matching fs a) =>
	FilePath -> a -> FSTree fs
	-> (FilePath -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZManifestFocus parentPath matching tree manifestUnder man = do
	files <- lift $ forestM $ getMatchingFilesInTree parentPath matching tree
	let name = pickFile files
	let testm = testFocus parentPath name (\file tree -> return True) [name]
	child_path <- lift $ forestM $ stepPathInTree tree parentPath name
	manifestUnder child_path  $ addTestToManifest testm man

doZManifestSimple :: (ICRep fs,Matching fs a) =>
	FilePath -> ForestI fs a -> FSTree fs -> rep
	-> (FilePath -> rep -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZManifestSimple parentPath matching tree dta manifestUnder man = lift (inside matching) >>= \m -> doZManifestFocus parentPath m tree (\p -> manifestUnder p dta) man

doZManifestSimpleWithConstraint :: (ForestContent fs rep,ForestMD fs rep,ICRep fs,Matching fs a) =>
	FilePath -> ForestI fs a -> FSTree fs
	-> (ForestContentTy fs rep -> ForestI fs Bool)
	-> (ForestFSThunkI fs Forest_err,rep)
	-> (FilePath -> rep -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZManifestSimpleWithConstraint parentPath matching tree pred dta manifestUnder = doZManifestConstraintInner pred dta $ \dta' man1 ->
	lift (inside matching) >>= \m -> doZManifestFocus parentPath m tree (\p -> manifestUnder p dta') man1

-- to enforce consistency while allowing the list to change, we delete all files in the directory that do not match the values
doZManifestCompound :: (Pads1 key_arg key key_md,IncK (IncForest fs) FileInfo,ForestMD fs rep',Matching fs a) =>
	FilePath -> ForestI fs a -> FSTree fs
	-> ForestI fs key_arg -> (container_rep -> [(key,rep')])
	-> container_rep
	-> (key -> ForestFSThunkI fs FileInfo -> FilePath -> rep' -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZManifestCompound parentPath matchingM tree mkeyarg toListRep c_rep manifestUnder man = do
	key_arg <- lift $ inside mkeyarg
	matching <- lift $ inside matchingM
	old_files <- lift $ forestM $ getMatchingFilesInTree parentPath matching tree
	let (new_keys,reps') = unzip $ toListRep c_rep
	let new_files = map (\key -> Pads.printS1 key_arg (key,Pads.defaultMd1 key_arg key)) new_keys
	let new_fileskeys = zip new_files new_keys
	repinfos' <- lift $ inside $ mapM (\rep -> mod (get_info rep) >>= \fileInfo_t -> return (rep,fileInfo_t)) reps'
	
	let rem_files = old_files \\ new_files -- files to be removed
	man1 <- lift $ forestM $ foldr (\rem_path man0M -> man0M >>= removePathFromManifestInTree rem_path tree) (return man) $ map (parentPath </>) rem_files -- remove deprecated files
	
	let manifestEach ((n,key),(rep',fileInfo_t)) man0M = do
		man0M >>= doZManifestFocus parentPath n tree (\p -> manifestUnder key fileInfo_t p rep')
	let testm = testFocus parentPath matching (\file tree -> return True) new_files
	liftM (addTestToManifest testm) $ foldr manifestEach (return man1) (zip new_fileskeys repinfos')
	

doZManifestCompoundWithConstraint :: (Pads1 key_arg key key_md,IncK (IncForest fs) FileInfo,ForestMD fs rep',Matching fs a) =>
	FilePath -> ForestI fs a -> FSTree fs
	-> ForestI fs key_arg -> (container_rep -> [(key,rep')])
	-> (key -> ForestFSThunkI fs FileInfo -> ForestI fs Bool)
	-> container_rep
	-> (key -> ForestFSThunkI fs FileInfo -> FilePath -> rep' -> Manifest fs -> MManifestForestO fs)
	-> Manifest fs -> MManifestForestO fs
doZManifestCompoundWithConstraint parentPath matchingM tree mkeyarg toListRep pred c_rep manifestUnder man = do
	key_arg <- lift $ inside mkeyarg
	matching <- lift $ inside matchingM
	old_files <- lift $ forestM $ getMatchingFilesInTree parentPath matching tree
	
	let (new_keys,reps') = unzip $ toListRep c_rep
	let new_files = map (\key -> Pads.printS1 key_arg (key,Pads.defaultMd1 key_arg key)) new_keys
	let new_fileskeys = zip new_files new_keys
	repinfos' <- lift $ inside $ mapM (\rep -> mod (get_info rep) >>= \fileInfo_t -> return (rep,fileInfo_t)) reps'
	
	let old_files' = old_files \\ new_files -- old files that are not in the view
	let old_fileskeys' = map (\file -> (file,fst $ Pads.parseString1 key_arg file)) old_files'
	old_metadatas' <- lift $ mapM (getRelForestMDInTree parentPath tree) old_files'
	-- we need to check which old files satisfy the predicate
	old_values' <- lift $ inside $ filterM (\((file,key),fmd) -> pred key (fileInfo fmd)) $ zip old_fileskeys' old_metadatas'
	let rem_fileskeys = map fst old_values'
	-- and delete them
	man1 <- lift $ forestM $ foldr (\rem_path man0M -> man0M >>= removePathFromManifestInTree rem_path tree) (return man) $ map ((parentPath </>) . fst) rem_fileskeys -- remove deprecated files
	
	let manifestEach ((n,key),(rep',fileInfo_t)) man0M = man0M >>= doZManifestConstraintCompound (pred key fileInfo_t) rep'
		(\rep' man -> doZManifestFocus parentPath n tree (\p -> manifestUnder key fileInfo_t p rep') man)
	let testm = testZFocus key_arg parentPath matching (\(file,key) tree -> forestO $ inside $ getRelForestMDInTree parentPath tree file >>= return . fileInfo >>= pred key) new_files
	liftM (addTestToManifest testm) $ foldr manifestEach (return man1) (zip new_fileskeys repinfos')

testZFocus :: (Pads1 key_arg key key_md,FSRep fs,Matching fs a) => key_arg -> FilePath -> a -> ((FileName,key) -> FSTree fs -> ForestM fs Bool) -> [FileName] -> ForestM fs Status
testZFocus key_arg root matching pred new_files = do
	tree <- latestTree
	files <- filterM (\file -> let key = fst (Pads.parseString1 key_arg file) in pred (file,key) tree) =<< getMatchingFilesInTree root matching tree
	let testFile (new_file,file) = do
		canpath <- canonalizePathInTree (root </> file) tree
		new_canpath <- canonalizePathInTree (root </> new_file) tree
		return $ canpath == new_canpath
	same <- liftM and $ mapM testFile $ zip (List.sort new_files) (List.sort files)
	return $ boolStatus (ConflictingMatching root (show matching) new_files files) $ (length files == length new_files) && same

testFocus :: (FSRep fs,Matching fs a) => FilePath -> a -> (FileName -> FSTree fs -> ForestM fs Bool) -> [FileName] -> ForestM fs Status
testFocus root matching pred new_files = do
	tree <- latestTree
	files <- filterM (flip pred tree) =<< getMatchingFilesInTree root matching tree
	let testFile (file,new_file) = do
		canpath <- canonalizePathInTree (root </> file) tree
		new_canpath <- canonalizePathInTree (root </> new_file) tree
		return $ canpath == new_canpath
	same <- liftM and $ mapM testFile $ zip (List.sort new_files) (List.sort files)
	return $ boolStatus (ConflictingMatching root (show matching) new_files files) $ (length files == length new_files) && same

