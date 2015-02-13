{-# LANGUAGE ConstraintKinds, Rank2Types, DataKinds, KindSignatures, TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}

module Language.Forest.IC.IO.ZDeltaLoading where
	
import Language.Pads.Generic as Pads
import Language.Forest.Syntax
import Language.Forest.IC.Default
import Language.Forest.IC.IO.ZLoading
import Language.Forest.IC.IO.Loading
import Language.Forest.IC.IO.DeltaLoading
import Language.Forest.IO.Utils
import Language.Forest.IC.ValueDelta
import Language.Pads.Padsc
import Language.Forest.IC.MetaData
import Language.Forest.IC.Generic
import Language.Forest.Errors
import Language.Forest.IC.ICRep
import Language.Forest.FS.FSDelta
import qualified Language.Forest.Pure.MetaData as Pure
import Language.Forest.Pure.MetaData (FileInfo(..),FileType(..),(:*:)(..),Arg(..))
import Data.List.Diff
import Data.List as List
import Data.IORef
import Prelude hiding (const,read,mod)
import Language.Forest.FS.Diff
import Language.Forest.IO.Shell
import qualified Prelude

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
import System.FilePath.Canonical
--import Data.Functor.Identity
import Data.Either
--import Language.Forest.Show
import Language.Forest.IC.Draw
import Data.Unique
import Control.Monad.Incremental as Inc

import qualified Control.Exception as CE

import Data.Data
import Data.Maybe
import System.Random
import Data.Proxy
import Data.Map (Map(..))
import qualified Data.Map as Map
import Language.Forest.IC.IO.Memo
import Language.Forest.IC.BX as BX

doZLoadDeltaFile1 :: (IncK (IncForest fs) Forest_err,IncK (IncForest fs) ((Forest_md fs, md), pads),rept ~ ForestFSThunkI fs ((Forest_md fs,md),pads),ZippedICMemo fs,MData NoCtx (ForestI fs) arg,ForestInput fs FSThunk Inside,Eq arg,Typeable arg,ICRep fs,Pads1 arg pads md)
	=> Bool -> ForestI fs arg -> ForestI fs FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rept -> (rept,GetForestMD fs)
	-> ForestO fs (SValueDelta rept)
doZLoadDeltaFile1 isEmptyDArg (marg' :: ForestI fs arg) mpath path' oldtree df tree' dv (rep_thunk,getMD) = do
	let argProxy = Proxy :: Proxy (Pure.Arg arg)
	path <- inside mpath
	case (isEmptyDArg,path == path',isIdValueDelta dv,df) of
		(True,True,True,(isEmptyFSTreeDeltaNodeMay -> True)) -> debug "constant1 unchanged" $ do
			inside $ addZippedMemo path' argProxy marg' rep_thunk (Just tree')
			return Id
		(True,True,True,Just (FSTreeChg _ _)) -> debug "constant1 attrs" $ do
			modify rep_thunk $ \((_,bmd),rep) -> getMD path' tree' >>= \fmd' -> return ((fmd',bmd),rep)
			inside $ addZippedMemo path' argProxy marg' rep_thunk (Just tree')
			return Delta
		otherwise -> debug "constant1 changed" $ do
			rep_thunk' <- inside $ doZLoadFile1 (Proxy::Proxy pads) marg' (fsTreeDeltaPathFilter df path') path' tree' getMD
			overwrite rep_thunk $ Inc.get rep_thunk'
			inside $ addZippedMemo path' argProxy marg' rep_thunk (Just tree')
			return Delta

doZLoadDeltaFileInner1 :: (IncK (IncForest fs) Forest_err,IncK (IncForest fs) ((Forest_md fs, md), pads),rept ~ ((Forest_md fs,md),pads),ZippedICMemo fs,MData NoCtx (ForestI fs) arg,ForestInput fs FSThunk Inside,Eq arg,Typeable arg,ICRep fs,Pads1 arg pads md)
	=> Bool -> ForestI fs arg -> ForestI fs FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rept -> (rept,GetForestMD fs)
	-> ForestO fs (NSValueDelta rept)
doZLoadDeltaFileInner1 isEmptyDArg (marg' :: ForestI fs arg) mpath path' oldtree df tree' dv (rep_thunk,getMD) = do
	let argProxy = Proxy :: Proxy (Pure.Arg arg)
	path <- inside mpath
	case (isEmptyDArg,path == path',isIdValueDelta dv,df) of
		(True,True,True,(isEmptyFSTreeDeltaNodeMay -> True)) -> debug "constant1 unchanged" $ do
			return $ StableVD Id
		(True,True,True,Just (FSTreeChg _ _)) -> debug "constant1 attrs" $ do
			fmd' <- inside $ getMD path' tree'
			return $ Modify $ \((_,bmd),rep) -> ((fmd',bmd),rep)
		otherwise -> debug "constant1 changed" $ do
			((fmd',bmd'),rep') <- inside $ doZLoadFile1' (Proxy::Proxy pads) marg' (fsTreeDeltaPathFilter df path') path' tree' getMD
			return $ Modify $ Prelude.const ((fmd',bmd'),rep')

doZLoadDeltaArchiveInner :: (DeltaClass d,ForestRep rep (ForestFSThunkI fs content0),ZippedICMemo fs
		,ForestInput fs FSThunk Inside,ForestMD fs rep) =>
	Bool -> [ArchiveType] -> ForestI fs FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs (Forest_md fs,rep)
	-> ((Forest_md fs,rep),GetForestMD fs)
	-> (FilePath -> GetForestMD fs -> FSTree fs -> ForestI fs rep)
	-> (FilePath -> ForestI fs rep)
	-> (ForestI fs FilePath -> FilePath -> (rep,GetForestMD fs) -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep -> ForestO fs (d rep))
	-> ForestO fs (NSValueDelta (Forest_md fs,rep))
doZLoadDeltaArchiveInner isClosed exts mpath path' oldtree df tree' dv ((fmd,rep),getMD) loadGood loadBad loadD = do
	let argsProxy = Proxy :: Proxy ()
	path <- inside mpath
	case (path == path',isIdValueDelta dv,df) of
		(True,True,isEmptyFSTreeDeltaNodeMay -> True) -> return $ fromSValueDelta Id
		(True,True,Just (FSTreeChg _ _)) -> do
			fmd' <- inside $ getMD path' tree'
			updateForestMDErrorsWith fmd' $ liftM (:[]) $ get_errors rep
			return $ Modify $ Prelude.const (fmd',rep)
		
		-- if the archive file has been moved, try to reuse originally loaded archive data
		(_,_,Just (FSTreeNew _ (Just from) _ _)) -> do
			rep' <- inside $ doZLoadArchiveInner exts (fsTreeDeltaPathFilter df path') path' tree' getMD loadGood loadBad
			return $ Modify $ Prelude.const rep'
		
		-- compute a diff for the archive's content, and continue
		otherwise -> do
			-- compute the difference for the archive's content
			avfsTree' <- forestM $ virtualTree tree'
			avfsOldTree <- forestM $ virtualTree oldtree
			let pathC = cardinalPath path
			let pathC' = cardinalPath path'
			archiveDf <- forestM $ focusDiffFSTree oldtree pathC tree' pathC'
			
			drep <- liftM toNSValueDelta $ loadD (return pathC) pathC' (rep,getForestMDInTree) avfsOldTree archiveDf avfsTree' (mapValueDelta Proxy dv)
			
			case (path==path',drep) of
				(True,StableVD d) -> do
					replaceForestMDErrorsWith fmd $ liftM (:[]) $ get_errors rep
					return $ StableVD Delta
				otherwise -> do
					fmd' <- inside $ getMD path tree'
					let rep' = applyDelta drep rep
					updateForestMDErrorsWith fmd' $ liftM (:[]) $ get_errors rep' -- like a directory
					return $ Modify $ Prelude.const (fmd',rep')

doZLoadDeltaArchive :: (Typeable rep,IncK (IncForest fs) (Forest_md fs, rep),DeltaClass d,ForestRep rep (ForestFSThunkI fs content0),ZippedICMemo fs
		,ForestInput fs FSThunk Inside,ForestMD fs rep) =>
	Bool -> [ArchiveType] -> ForestI fs FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs (ForestFSThunkI fs (Forest_md fs,rep))
	-> (ForestFSThunkI fs (Forest_md fs,rep),GetForestMD fs)
	-> (FilePath -> GetForestMD fs -> FSTree fs -> ForestI fs rep)
	-> (FilePath -> ForestI fs rep)
	-> (ForestI fs FilePath -> FilePath -> (rep,GetForestMD fs) -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep -> ForestO fs (d rep))
	-> (FSTree fs -> rep -> ForestO fs (ValueDelta fs rep))
	-> ForestO fs (SValueDelta (ForestFSThunkI fs (Forest_md fs,rep)))
doZLoadDeltaArchive isClosed exts mpath path' oldtree df tree' dv (rep_thunk,getMD) loadGood loadBad loadD diffValue = do
	let argsProxy = Proxy :: Proxy ()
	path <- inside mpath
	case (path == path',isIdValueDelta dv,df) of
		(True,True,isEmptyFSTreeDeltaNodeMay -> True) -> do
			when isClosed $ inside $ addZippedMemo path' argsProxy () rep_thunk (Just tree')
			return Id
		(True,True,Just (FSTreeChg _ _)) -> do
			modify rep_thunk $ \(_,irep') -> do
				fmd' <- getMD path' tree'
				fmd'' <- updateForestMDErrorsInsideWith fmd' $ liftM (:[]) $ get_errors irep'
				return (fmd'',irep')
			when isClosed $ inside $ addZippedMemo path' argsProxy () rep_thunk (Just tree')
			return Delta
		
		-- if the archive file has been moved, try to reuse originally loaded archive data
		(_,_,Just (FSTreeNew _ (Just from) _ _)) -> do
			rep' <- inside $ doZLoadArchive isClosed (Proxy::Proxy rep) exts (fsTreeDeltaPathFilter df path') path' tree' getMD loadGood loadBad loadD diffValue
			overwrite rep_thunk $ Inc.get rep'
			return Delta
		
		-- compute a diff for the archive's content, and continue
		otherwise -> do
			rep@(fmd,irep) <- Inc.getOutside rep_thunk
				
			-- compute the difference for the archive's content
			avfsTree' <- forestM $ virtualTree tree'
			avfsOldTree <- forestM $ virtualTree oldtree
			let pathC = cardinalPath path
			let pathC' = cardinalPath path'
			archiveDf <- forestM $ focusDiffFSTree oldtree pathC tree' pathC'
			
			archiveDv <- diffValue oldtree irep
			direp <- liftM toNSValueDelta $ loadD (return pathC) pathC' (irep,getForestMDInTree) avfsOldTree archiveDf avfsTree' archiveDv
			case (path==path',direp) of
				(True,StableVD d) -> do
					replaceForestMDErrorsWith fmd $ liftM (:[]) $ get_errors irep
				otherwise -> do
					fmd' <- inside $ getMD path tree'
					let irep' = applyNSValueDelta direp irep
					updateForestMDErrorsWith fmd' $ liftM (:[]) $ get_errors irep' -- like a directory
					set rep_thunk (fmd',irep')
			
			when isClosed $ inside $ addZippedMemo path' argsProxy () rep_thunk (Just tree')
			return Delta
				

doZLoadDeltaSymLink :: (sym ~ ForestFSThunkI fs ((Forest_md fs,Base_md),FilePath),IncK (IncForest fs) Forest_err,IncK (IncForest fs) ((Forest_md fs, Base_md), FilePath),ForestInput fs FSThunk Inside,ICRep fs)
	=> ForestI fs FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs sym
	-> (sym,GetForestMD fs)
	-> ForestO fs (SValueDelta sym)
doZLoadDeltaSymLink mpath path' oldtree df tree' dv (rep_thunk,getMD) = do
	path <- inside mpath
	case (path == path',isIdValueDelta dv,df) of
		(True,True,isEmptyFSTreeDeltaNodeMay -> True) -> debug "symlink unchanged" $ return Id
		(True,True,Just (FSTreeChg _ _)) -> do
			modify rep_thunk $ \((_,bmd),rep) -> getMD path' tree' >>= \fmd' -> return ((fmd',bmd),rep)
			return Delta
		otherwise -> debug "symlink changed" $ do
			rep' <- inside $ doZLoadSymLink' path' tree' getMD
			set rep_thunk rep'
			return Delta

doZLoadDeltaSymLinkInner :: (sym ~ ((Forest_md fs,Base_md),FilePath),IncK (IncForest fs) Forest_err,IncK (IncForest fs) ((Forest_md fs, Base_md), FilePath),ForestInput fs FSThunk Inside,ICRep fs)
	=> ForestI fs FilePath -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs sym
	-> (sym,GetForestMD fs)
	-> ForestO fs (NSValueDelta sym)
doZLoadDeltaSymLinkInner mpath path' oldtree df tree' dv (rep_thunk,getMD) = do
	path <- inside mpath
	case (path == path',isIdValueDelta dv,df) of
		(True,True,isEmptyFSTreeDeltaNodeMay -> True) -> debug "symlink unchanged" $ return $ StableVD Id
		(True,True,Just (FSTreeChg _ _)) -> do
			fmd' <- inside $ getMD path' tree'
			return $ Modify $ \((_,bmd),rep) -> ((fmd',bmd),rep)
		otherwise -> debug "symlink changed" $ do
			rep' <- inside $ doZLoadSymLinkInner path' tree' getMD
			return $ Modify $ Prelude.const rep'

doZLoadDeltaConstraint :: (ForestContent rep content,IncK (IncForest fs) (ForestFSThunkI fs Forest_err, rep),err_rep ~ ForestFSThunkI fs (ForestFSThunkI fs Forest_err,rep),DeltaClass d,ForestMD fs rep,ForestOutput fs ICThunk Inside,ForestInput fs FSThunk Outside) =>
	Bool -> ValueDelta fs err_rep -> (err_rep,GetForestMD fs) -> (content -> ForestI fs Bool)
	-> (ValueDelta fs rep -> (rep,GetForestMD fs) -> ForestO fs (d rep))
	-> ForestO fs (SValueDelta err_rep)
doZLoadDeltaConstraint emptyDArgs dv (t,getMD) pred loadD = do
	v <- Inc.getOutside t
	drep <- liftM toNSValueDelta $ doZLoadDeltaConstraintInner emptyDArgs (mapValueDelta Proxy dv) (v,getMD) pred loadD
	case drep of
		StableVD d -> return $ mapSValueDelta d
		Modify f -> do
			modify t $ return . applyNSValueDelta drep
			return Delta

doZLoadDeltaConstraintInner :: (ForestContent rep content,err_rep ~ (ForestFSThunkI fs Forest_err,rep),DeltaClass d,ForestMD fs rep,ForestOutput fs ICThunk Inside,ForestInput fs FSThunk Outside) =>
	Bool -> ValueDelta fs err_rep -> (err_rep,GetForestMD fs) -> (content -> ForestI fs Bool)
	-> (ValueDelta fs rep -> (rep,GetForestMD fs) -> ForestO fs (d rep))
	-> ForestO fs (d err_rep)
doZLoadDeltaConstraintInner emptyDArgs dv ((err_t,rep),getMD) pred loadD = debug ("doLoadDeltaConstraint: ") $ do
	direp <- loadD (mapValueDelta Proxy dv) (rep,getMD)
	case (emptyDArgs,isIdValueDelta dv,isEmptyDelta direp) of
		(True,True,True) -> return $ mapDelta sndLens direp
		otherwise -> do
			overwrite err_t $ do
				err_cond <- predForestErr $ pred $ BX.get lens_content rep
				err_inner <- get_errors rep
				return $ Pure.mergeForestErrs err_cond err_inner
			if isStableDelta direp
				then return $ fromSValueDelta Delta
				else return $ mapDelta sndLens direp

-- updates the thunks that keep track of the arguments of a top-level declaration
doZLoadDeltaArgs :: (ForestArgs fs args,ICRep fs) =>
	Proxy args -> LoadDeltaArgs ICData fs args -> (rep,GetForestMD fs) -> FSTree fs
	-> (ForestICThunksI fs args -> (rep,GetForestMD fs) -> ForestO fs (SValueDelta rep))
	-> ForestO fs (SValueDelta rep)
doZLoadDeltaArgs proxy (margs,_) ((rep,getMD)) (tree' :: FSTree fs) loadD = debug ("doLoadDeltaArgs") $ do
	arg_thunks <- inside $ newArgs (Proxy :: Proxy fs) proxy margs -- creates new thunks to hold the new expressions
	(drep) <- loadD arg_thunks ((rep),getMD)
	return (mapSValueDelta drep) 

doZLoadDeltaDirectory :: (IncK (IncForest fs) Forest_err,IncK (IncForest fs) (Forest_md fs, rep),DeltaClass d,ICRep fs) =>
	ForestI fs FilePath -> (ForestFSThunkI fs (Forest_md fs,rep),GetForestMD fs) -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> ValueDelta fs (ForestFSThunkI fs (Forest_md fs,rep))
	-> (rep -> ForestI fs Forest_err)
	-> (GetForestMD fs -> ForestI fs rep)
	-> ForestI fs rep
	-> (ValueDelta fs rep -> (rep,GetForestMD fs) -> ForestO fs (d rep))
	-> ForestO fs (SValueDelta (ForestFSThunkI fs (Forest_md fs,rep)))
doZLoadDeltaDirectory mpath (rep_thunk,getMD) path' oldtree df tree' dv collectMDErrors loadGood loadBad loadD = do
	path <- inside mpath
	exists <- forestM $ doesDirectoryExistInTree path oldtree
	exists' <- forestM $ doesDirectoryExistInTree path' tree'
	debug ("doLoadDeltaDirectory: " ++ show (path,exists,path',exists')) $ case (exists,exists') of
		(False,False) -> case (path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay df) of
			(True,True,True) -> return Id
			otherwise -> do
				modify rep_thunk $ \(_,rep1) -> do
					fmd' <- missingDirForestMD path'
					return (fmd',rep1)
				return Delta
		(True,False) -> do
			modify rep_thunk $ \(_,rep1) -> do
				fmd' <- missingDirForestMD path'
				return (fmd',rep1)
			return Delta
		(True,True) -> do
			rep@(fmd,irep) <- Inc.getOutside rep_thunk
			let idv = mapValueDelta Proxy dv
			direp <- liftM toNSValueDelta $ loadD idv (irep,getMD) -- load recursively
			drep <- case (path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay df) of 
				(True,True,True) -> do -- if the current path stayed the same and no attributes changed, we can reuse its @Forest_md@ but replace its errors; all the changes are stable
					case direp of
						StableVD Id -> return Id
						StableVD Delta -> debug ("dir1") $ do
							replaceForestMDErrorsWith fmd $ liftM (:[]) $ collectMDErrors irep
							return Delta
						Modify f -> do
							fmd' <- inside $ getMD path' tree'
							let irep' = applyNSValueDelta direp irep
							updateForestMDErrorsWith fmd' $ liftM (:[]) $ collectMDErrors irep' -- this forces the value after the update
							set rep_thunk (fmd',f irep')
							return Delta -- modify the directory thunk, since underlying non-stable changes did not occur inside a modifiable
				otherwise -> do -- otherwise we compute a new @Forest_md@ and add new errors; all the changes are stable
					case (path==path',direp) of
						(True,StableVD d) -> do
							replaceForestMDErrorsWith fmd $ liftM (:[]) $ collectMDErrors irep
							return Delta
						otherwise -> do
							fmd' <- inside $ getMD path' tree'
							let irep' = applyNSValueDelta direp irep
							updateForestMDErrorsWith fmd' $ liftM (:[]) $ collectMDErrors irep' -- this forces the value after the update
							set rep_thunk (fmd',irep')
							return Delta
			debug ("doLoadDeltaDirectoryReturn: "++show (path,exists,path',exists')++show direp) $ return drep
		(False,True) -> do -- load from scratch
			overwrite rep_thunk $ doZLoadDirectory' path' tree' collectMDErrors getMD (loadGood getMD) loadBad
			return Delta

doZLoadDeltaDirectoryInner :: (IncK (IncForest fs) Forest_err,DeltaClass d,ICRep fs) =>
	ForestI fs FilePath -> ((Forest_md fs,rep),GetForestMD fs) -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> ValueDelta fs (Forest_md fs,rep)
	-> (rep -> ForestI fs Forest_err)
	-> (GetForestMD fs -> ForestI fs rep)
	-> ForestI fs rep
	-> (ValueDelta fs rep -> (rep,GetForestMD fs) -> ForestO fs (d rep))
	-> ForestO fs (NSValueDelta (Forest_md fs,rep))
doZLoadDeltaDirectoryInner mpath ((fmd,irep),getMD) path' oldtree df tree' dv collectMDErrors loadGood loadBad loadD = do
	path <- inside mpath
	exists <- forestM $ doesDirectoryExistInTree path oldtree
	exists' <- forestM $ doesDirectoryExistInTree path' tree'
	debug ("doLoadDeltaDirectory: " ++ show (path,exists,path',exists')) $ case (exists,exists') of
		(False,False) -> case (path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay df) of
			(True,True,True) -> return $ StableVD Id
			otherwise -> do
				fmd' <- inside $ missingDirForestMD path'
				return $ Modify $ \(_,rep1) -> (fmd',rep1)
		(True,False) -> do
			fmd' <- inside $ missingDirForestMD path'
			return $ Modify $ \(_,rep1) -> (fmd',rep1)
		(True,True) -> do
			let idv = mapValueDelta Proxy dv
			direp <- liftM toNSValueDelta $ loadD idv (irep,getMD) -- load recursively
			drep <- case (path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay df) of 
				(True,True,True) -> do -- if the current path stayed the same and no attributes changed, we can reuse its @Forest_md@ but replace its errors; all the changes are stable
					case direp of
						StableVD Id -> return $ StableVD Id
						StableVD Delta -> debug ("dir1") $ do
							replaceForestMDErrorsWith fmd $ liftM (:[]) $ collectMDErrors irep
							return $ StableVD Delta
						Modify f -> do
							fmd' <- inside $ getMD path' tree'
							let irep' = applyNSValueDelta direp irep
							updateForestMDErrorsWith fmd' $ liftM (:[]) $ collectMDErrors irep' -- this forces the value after the update
							return $ Modify $ Prelude.const (fmd',f irep')
				otherwise -> do -- otherwise we compute a new @Forest_md@ and add new errors; all the changes are stable
					case (path==path',direp) of
						(True,StableVD d) -> do
							replaceForestMDErrorsWith fmd $ liftM (:[]) $ collectMDErrors irep
							return $ StableVD Delta
						otherwise -> do
							fmd' <- inside $ getMD path' tree'
							let irep' = applyNSValueDelta direp irep
							updateForestMDErrorsWith fmd' $ liftM (:[]) $ collectMDErrors irep' -- this forces the value after the update
							return $ Modify $ Prelude.const (fmd',irep')
			debug ("doLoadDeltaDirectoryReturn: "++show (path,exists,path',exists')++show direp) $ return drep
		(False,True) -> do -- load from scratch
			rep' <- inside $ doZLoadDirectory' path' tree' collectMDErrors getMD (loadGood getMD) loadBad
			return $ Modify $ Prelude.const rep'

doZLoadDeltaMaybeInner :: (DeltaClass d,ForestMD fs rep) =>
	ForestI fs FilePath -> (Maybe rep,GetForestMD fs) -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> ValueDelta fs (Maybe rep)
	-> (GetForestMD fs -> ForestI fs rep)
	-> (ValueDelta fs rep -> (rep,GetForestMD fs) -> ForestO fs (d rep))
	-> (FSTree fs -> rep -> ForestO fs (ValueDelta fs rep))
	-> ForestO fs (NSValueDelta (Maybe rep))
doZLoadDeltaMaybeInner mpath (mb_rep,getMD) path' oldtree df tree' dv load loadD diffValue = do
	path <- inside mpath
	exists <- forestM $ doesExistInTree path oldtree
	exists' <- forestM $ doesExistInTree path' tree'
	case (exists,mb_rep,exists') of
		(False,Nothing,False) -> do
			case (path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay df) of
				(True,True,True) -> return $ StableVD Id
				otherwise -> do
					return $ Modify $ Prelude.const Nothing
		(_,_,False) -> do
			return $ Modify $ Prelude.const Nothing
		(True,Just irep,True) -> do
			idv <- diffValue oldtree irep
			direp <- liftM toNSValueDelta $ loadD idv (irep,getMD) -- load recursively
			return $ maybeNSValueDelta direp
		(_,_,True) -> do
			mb_rep' <- inside $ doZLoadMaybeInner (fsTreeDeltaPathFilter df path') path' tree' $ load getMD
			return $ Modify $ Prelude.const mb_rep'

doZLoadDeltaMaybe :: (IncK (IncForest fs) (Forest_md fs, Maybe rep),DeltaClass d,ForestMD fs rep) =>
	ForestI fs FilePath -> (ForestFSThunkI fs (Forest_md fs,Maybe rep),GetForestMD fs) -> FilePath -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> ValueDelta fs (ForestFSThunkI fs (Forest_md fs,Maybe rep))
	-> (GetForestMD fs -> ForestI fs rep)
	-> (ValueDelta fs rep -> (rep,GetForestMD fs) -> ForestO fs (d rep))
	-> (FSTree fs -> rep -> ForestO fs (ValueDelta fs rep))
	-> ForestO fs (SValueDelta (ForestFSThunkI fs (Forest_md fs,Maybe rep)))
doZLoadDeltaMaybe mpath (rep_thunk,getMD) path' oldtree df tree' dv load loadD diffValue = do
	path <- inside mpath
	exists <- forestM $ doesExistInTree path oldtree
	exists' <- forestM $ doesExistInTree path' tree'
	rep@(fmd,mb_rep) <- Inc.getOutside rep_thunk
	case (exists,mb_rep,exists') of
		(False,Nothing,False) -> do
			case (path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay df) of
				(True,True,True) -> return Id
				otherwise -> do
					modify rep_thunk $ \(_,rep1) -> cleanForestMDwithFile path' >>= \fmd' -> return (fmd',rep1)
					return Delta
		(_,_,False) -> do
			overwrite rep_thunk $ cleanForestMDwithFile path' >>= \fmd' -> return (fmd',Nothing)
			return Delta
		(True,Just irep,True) -> do
			idv <- diffValue oldtree irep
			direp <- liftM toNSValueDelta $ loadD idv (irep,getMD) -- load recursively
			case (path==path',direp) of
				(True,StableVD direp) -> do -- we don't need to update the maybe thunk
					replaceForestMDErrorsWith fmd $ liftM (:[]) $ get_errors rep
					return Delta
				otherwise -> do
					let irep' = applyNSValueDelta direp irep
					fmd' <- cleanForestMDwithFile path
					updateForestMDErrorsWith fmd' $ liftM (:[]) $ get_errors rep
					set rep_thunk $ (fmd',Just irep') -- always update the metadata with the inner @Forest_md@ header
					return Delta
		(_,_,True) -> do
			overwrite rep_thunk $ doZLoadMaybe' (fsTreeDeltaPathFilter df path') path' tree' $ load getMD
			return Delta

-- note that in the implementation we do not incrementalize the path-focusing expressions as they (typically) depend on the FS
-- changes the current path, the original data already corresponds to the focused path
doZLoadDeltaFocus :: (DeltaClass d,Matching fs a,ForestMD fs rep) =>
	ForestI fs FilePath -> FilePath -> (rep,GetForestMD fs) -> a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> ValueDelta fs rep
	-> (ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep -> ForestO fs (d rep))
	-> ForestO fs (d rep)
doZLoadDeltaFocus mpath path' (rep,getMD) matching oldtree df tree' dv loadD = do
	
	let mfile = do
		path <- inside mpath
		files <- forestM $ getMatchingFilesInTree path matching oldtree
		return $ pickFile files
	
	files' <- forestM $ getMatchingFilesInTree path' matching tree'
	case files' of
		[file'] -> doZLoadDeltaNewPath mpath path' mfile file' oldtree df tree' $ \x y z -> loadD x y z dv
		files' -> do
			doZLoadDeltaNewPath mpath path' mfile (pickFile files') oldtree df tree' $ \mnewpath newdpath df -> do
				drep <- loadD mnewpath newdpath df dv -- changes the delta
				if length files' == 0
					then return drep
					else if isEmptyDelta drep
						then return drep
						else do
							newpath <- inside mnewpath
							addMultipleMatchesErrorMD newpath files' rep
							return drep

-- changes the delta
doZLoadDeltaNewPath :: (DeltaClass d,ICRep fs) => ForestI fs FilePath -> FilePath -> ForestI fs FileName -> FileName -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs
	-> (ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ForestO fs (d rep))
	-> ForestO fs (d rep)
doZLoadDeltaNewPath mpath path' mfile file' oldtree df tree' loadD = debug ("doLoadDeltaNewPath: "++show (path',file',df)) $ do
	let mnewpath = do
		path <- mpath
		file <- mfile
		forestM $ stepPathInTree oldtree path file
	newpath' <- forestM $ stepPathInTree tree' path' file'
	let newdf = focusFSTreeDeltaNodeMayByRelativePath df file' -- focusing the tree deltas is important for the skipping conditions to fire for unchanged branches of the FS
	debug ("changed FSDelta: " ++ show newdf) $ loadD mnewpath newpath' newdf -- load recursively

doZLoadDeltaSimple :: (DeltaClass d,ForestMD fs rep',Matching fs a,MData NoCtx (ForestO fs) rep') =>
	Lens dir_rep rep'
	-> ForestI fs FilePath -> FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs dir_rep -> (dir_rep,GetForestMD fs)
	-> ((rep',GetForestMD fs) -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep' -> ForestO fs (d rep'))
	-> ForestO fs (NSValueDelta dir_rep)
doZLoadDeltaSimple lens mpath path' matchingM oldtree df tree' dv (dir_rep,getMD) loadD = debug ("doLoadDeltaSimple: "++show path') $ do
	matching <- inside $ matchingM
	let irep = BX.get lens dir_rep
	let idata = (irep,getForestMDInTree) -- we need to discard any previously loaded forest metadata
	direp <- liftM toNSValueDelta $ doZLoadDeltaFocus mpath path' idata matching oldtree df tree' (mapValueDelta Proxy dv) (loadD idata)
	case direp of
		StableVD d -> return $ StableVD $ liftSValueDelta d
		otherwise -> return $ Modify $ \s -> BX.put lens s $ applyNSValueDelta direp irep

doZLoadDeltaSimpleWithConstraint :: (ForestContent rep' content',err_rep' ~ (ForestFSThunkI fs Forest_err, rep'),DeltaClass d,ForestMD fs rep',ForestOutput fs ICThunk Inside,Matching fs a,MData NoCtx (ForestO fs) rep') =>
	Lens dir_rep err_rep'
	-> Bool -> ForestI fs FilePath -> FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs dir_rep -> (dir_rep,GetForestMD fs)
	-> (content' -> ForestI fs Bool)
	-> ((rep',GetForestMD fs) -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep' -> ForestO fs (d rep'))
	-> ForestO fs (NSValueDelta dir_rep)
doZLoadDeltaSimpleWithConstraint lens emptyDArgs mpath path' matchingM oldtree df tree' dv (dir_rep,getMD) pred loadD = debug ("doLoadDeltaSimpleWithConstraint: "++show (path')) $ do
	matching <- inside $ matchingM
	let irep = BX.get lens dir_rep
	let idata = (irep,getForestMDInTree) -- we need to discard any previously loaded forest metadata
	direp <- liftM toNSValueDelta $ doZLoadDeltaConstraintInner emptyDArgs (mapValueDelta Proxy dv) idata pred $ \dv idata -> doZLoadDeltaFocus mpath path' idata matching oldtree df tree' dv (loadD idata)
	case direp of
		StableVD d -> do
			return $ StableVD $ liftSValueDelta d
		otherwise -> return $ Modify $ \s -> BX.put lens s $ applyNSValueDelta direp irep
	

doZLoadDeltaCompound :: (Ord key,Pads1 key_arg key key_md,IncK (IncForest fs) FileInfo,DeltaClass d,ForestMD fs rep',Matching fs a,ICRep fs,MData NoCtx (ForestO fs) list_rep',list_rep' ~ [(key,rep')]) =>
		Lens dir_rep container_rep' -> Iso container_rep' list_rep' -> ForestI fs key_arg
		-> ForestI fs FilePath -> FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs dir_rep -> (dir_rep,GetForestMD fs)
		-> (key -> ForestFSThunkI fs FileInfo -> FilePath -> GetForestMD fs -> ForestI fs rep')
		-> (key -> key -> ForestFSThunkI fs FileInfo -> SValueDelta (ForestICThunkI fs FileInfo) -> (rep',GetForestMD fs) -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep' -> ForestO fs (d rep'))
		-> (FSTree fs -> rep' -> ForestO fs (ValueDelta fs rep'))
		-> ForestO fs (NSValueDelta dir_rep)
doZLoadDeltaCompound lens isoRep mkeyarg mpath path' matchingM oldtree df tree' dv (dir_rep,getMD) load loadD diffValue = debug ("doLoadDeltaCompound: "++show (path')) $ do
	key_arg <- inside mkeyarg
	path <- inside mpath
	matching <- inside $ matchingM -- matching expressions are always recomputed; since they depend on the FS we cannot incrementally reuse them
	let crep = BX.get lens dir_rep
	let oldreplist = to isoRep crep
	let (oldkeys,oldreps) = unzip oldreplist
	newfiles <- forestM $ getMatchingFilesInTree path matching tree'
	let newkeys = map (fst . Pads.parseString1 key_arg) newfiles
	(newreps,noDeletions) <- forestM $ forestIO $ alignWithLookup newkeys oldkeys oldreps -- alignment by filename; newfiles first for the association list to be sorted by newfile positions
	
	-- load each file in the new tree (note that filtering is only done later)
	let loadEachFile (newfile,(newkey,mbrep)) = do
		idv <- maybe (return chgValueDelta) (diffValue oldtree) mbrep
		doZLoadDeltaCompoundFile mpath path' newfile newkey mbrep oldtree df tree' idv load loadD
	(mergeCompoundSValueDeltas -> (repchanges,repkind)) <- mapM loadEachFile $ zip newfiles newreps
	
	-- return the new container values and deltas on the directory
	drep <- case (noDeletions,isIdValueDelta dv,repkind) of
			(True,True,NoOp) -> return $ StableVD Id -- this is OK because the alignment algorithm is stable
			(True,True,Stable) -> return $ StableVD Delta
			otherwise -> return $ Modify $ \s -> put lens s (from isoRep repchanges)
	return $ debug ("doLoadDeltaCompoundReturn: "++show path') drep

-- returns the new values and a boolean indicating whether it has changed
doZLoadDeltaCompoundFile :: (IncK (IncForest fs) FileInfo,DeltaClass d,ForestMD fs rep) => 
	ForestI fs FilePath -> FilePath -> FileName -> key -> Maybe rep -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep
	-> (key -> ForestFSThunkI fs FileInfo -> FilePath -> GetForestMD fs -> ForestI fs rep)
	-> (key -> key -> ForestFSThunkI fs FileInfo -> SValueDelta (ForestICThunkI fs FileInfo) -> (rep,GetForestMD fs) -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep -> ForestO fs (d rep))
	-> ForestO fs ((Maybe (key,rep),ValueDeltaKind))
doZLoadDeltaCompoundFile mpath path' file' key Nothing oldtree df tree' dv load loadD = debug ("doLoadDeltaCompoundFileNothing: "++show (path',file')) $ do
	-- try to reuse the original metadata (in this case it is not reused)
	(newGetMD,info) <- inside $ getRelForestMDInTree path' tree' file' >>= \fmd -> return (const2 $ return fmd,fileInfo fmd)
	-- filter new files according to the predicate
	fileInfo_thunk <- inside $ ref info
	rep' <- inside $ doZLoadFocus (fsTreeDeltaPathFilter df path') path' file' tree' newGetMD $ \newpath newGetMD -> do
		load key fileInfo_thunk newpath newGetMD
	return ((Just (key,rep'),NonStable))
doZLoadDeltaCompoundFile mpath path' file' key (Just rep) oldtree df tree' dv load loadD = debug ("doLoadDeltaCompoundFile: "++show (path',file')) $ do
	fileInfo_thunk <- getRelForestMDInTree path' tree' file' >>= \fmd -> inside $ ref $ fileInfo fmd
	path <- inside mpath
	-- try to reuse the original metadata
	(newGetMD,dFileInfo) <- do
		isUnevaluated <- isUnevaluatedMDThunk rep -- just an optimization to preserve the evaluation status of the thunk and avoid the unnecessary loading of old data
		case (isUnevaluated,path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay $ focusFSTreeDeltaNodeMayByRelativePath df file') of
			(False,True,True,True) -> get_fmd_header rep >>= \fmd -> return (getForestMDInTree,Id) -- we keep the original data
			otherwise -> getRelForestMDInTree path' tree' file' >>= \fmd -> set fileInfo_thunk (fileInfo fmd) >> return (const2 $ return fmd,Delta)
	-- the filename has not changed as long as there was a match in the alignment operation, i.e., doLoadDeltaCompoundFile is called with original data
	drep <- liftM toNSValueDelta $ doZLoadDeltaFocus mpath path' (rep,newGetMD) file' oldtree df tree' dv $ loadD key key fileInfo_thunk dFileInfo (rep,newGetMD) -- load recursively under a different focus
	-- apply the field deltas so that the post-values can be made available in the environment
	let (rep',repkind) = (applyNSValueDelta drep rep,valueDeltaKind drep `andValueDeltaKinds` (valueDeltaKind dFileInfo))
	return (Just (key,rep'),repkind)

doZLoadDeltaCompoundWithConstraint :: (Ord key,Pads1 key_arg key key_md,IncK (IncForest fs) FileInfo,DeltaClass d,ForestMD fs rep',ForestOutput fs ICThunk Inside,Matching fs a,ICRep fs,MData NoCtx (ForestO fs) list_rep',list_rep' ~ [(key,rep')]) =>
		Lens dir_rep container_rep' -> Iso container_rep' list_rep' -> ForestI fs key_arg
		-> ForestI fs FilePath -> FilePath -> ForestI fs a -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs dir_rep -> (dir_rep,GetForestMD fs)
		-> (key -> ForestFSThunkI fs FileInfo -> ForestI fs Bool)
		-> (key -> ForestFSThunkI fs FileInfo -> FilePath -> GetForestMD fs -> ForestI fs rep')
		-> (key -> key -> ForestFSThunkI fs FileInfo -> SValueDelta (ForestICThunkI fs FileInfo) -> (rep',GetForestMD fs) -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep' -> ForestO fs (d rep'))
		-> (FSTree fs -> rep' -> ForestO fs (ValueDelta fs rep'))
		-> ForestO fs (NSValueDelta dir_rep)
doZLoadDeltaCompoundWithConstraint lens isoRep mkeyarg mpath path' matchingM oldtree df tree' dv (dir_rep,getMD) pred load loadD diffValue = debug ("doLoadDeltaCompoundC: "++show (path')) $ do
	key_arg <- inside mkeyarg
	path <- inside mpath
	matching <- inside matchingM
	let crep = BX.get lens dir_rep
	let oldreplist = to isoRep crep
	let (oldkeys,oldreps) = unzip oldreplist
	newfiles <- forestM $ getMatchingFilesInTree path matching tree'
	let newkeys = map (fst . Pads.parseString1 key_arg) newfiles
	(newreps,noDeletions) <- forestM $ forestIO $ alignWithLookup newkeys oldkeys oldreps -- alignment by filename; newfiles first for the association list to be sorted by newfile positions
	
	-- load each file in the new tree (note that filtering is only done later)
	let loadEachFile (newfile,(newkey,mbrep)) = do
		idv <- maybe (return chgValueDelta) (diffValue oldtree) mbrep
		doZLoadDeltaCompoundFileWithConstraint mpath path' newfile newkey mbrep oldtree df tree' idv pred load loadD
	(mergeCompoundSValueDeltas -> (repchanges,repkind)) <- mapM loadEachFile $ zip newfiles newreps
	
	-- return the new container values and deltas on the directory
	drep <- case (noDeletions,isIdValueDelta dv,repkind) of
			(True,True,NoOp) -> return $ StableVD Id -- this is OK because the alignment algorithm is stable
			(True,True,Stable) -> return $ StableVD Delta
			otherwise -> return $ Modify $ \s -> put lens s (from isoRep repchanges)
	return $ debug ("doLoadDeltaCompoundCReturn: "++show (path')) drep

-- returns the new values and a boolean indicating whether it has changed
doZLoadDeltaCompoundFileWithConstraint :: (IncK (IncForest fs) FileInfo,DeltaClass d,ForestMD fs rep,ForestOutput fs ICThunk Inside) => 
	ForestI fs FilePath -> FilePath -> FileName -> key -> Maybe rep -> FSTree fs -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep
	-> (key -> ForestFSThunkI fs FileInfo -> ForestI fs Bool)
	-> (key -> ForestFSThunkI fs FileInfo -> FilePath -> GetForestMD fs -> ForestI fs rep)
	-> (key -> key -> ForestFSThunkI fs FileInfo -> SValueDelta (ForestICThunkI fs FileInfo) -> (rep,GetForestMD fs) -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> ValueDelta fs rep -> ForestO fs (d rep))
	-> ForestO fs (Maybe (key,rep),ValueDeltaKind)
doZLoadDeltaCompoundFileWithConstraint mpath path' file' key Nothing oldtree df tree' dv pred load loadD = debug ("doLoadDeltaCompoundFileNothing: "++show (path',file')) $ do
	path <- inside mpath
	-- try to reuse the original metadata (in this case it is not reused)
	(newGetMD,info) <- inside $ getRelForestMDInTree path' tree' file' >>= \fmd -> return (const2 $ return fmd,fileInfo fmd)
	fileInfo_thunk <- inside $ ref info
	-- filter new files according to the predicate
	cond <- inside $ pred key fileInfo_thunk
	if cond
		then do
			rep' <- inside $ doZLoadFocus (fsTreeDeltaPathFilter df path') path' file' tree' newGetMD $ \newpath newGetMD -> do
				load key fileInfo_thunk newpath newGetMD
			return (Just (key,rep'),NonStable)
		else return (Nothing,NonStable)
doZLoadDeltaCompoundFileWithConstraint mpath path' file' key (Just rep) oldtree df tree' dv pred load loadD = debug ("doLoadDeltaCompoundFile: "++show (path',file')) $ do
	path <- inside mpath
	fmd <- getRelForestMDInTree path' tree' file'
	fileInfo_thunk <- inside $ ref $ fileInfo fmd
	-- try to reuse the original metadata
	(newGetMD,dFileInfo) <- do
		isUnevaluated <- isUnevaluatedMDThunk rep -- just an optimization to preserve the evaluation status of the thunk and avoid the unnecessary loading of old data
		case (isUnevaluated,path == path',isIdValueDelta dv,isEmptyTopFSTreeDeltaNodeMay $ focusFSTreeDeltaNodeMayByRelativePath df file') of
			(False,True,True,True) -> get_fmd_header rep >>= \fmd -> return (getForestMDInTree,Id) -- we keep the original data
			otherwise -> getRelForestMDInTree path' tree' file' >>= \fmd -> set fileInfo_thunk (fileInfo fmd) >> return (const2 $ return fmd,Delta)
	-- filter new files according to the predicate
	cond <- inside $ pred key fileInfo_thunk
	if cond
		then do -- the filename has not changed as long as there was a match in the alignment operation, i.e., doLoadDeltaCompoundFile is called with original data
			drep <- liftM toNSValueDelta $ doZLoadDeltaFocus mpath path' (rep,newGetMD) file' oldtree df tree' dv $ loadD key key fileInfo_thunk dFileInfo (rep,newGetMD) -- load recursively under a different focus
			-- apply the field deltas so that the post-values can be made available in the environment
			let (rep',repkind) = (applyNSValueDelta drep rep,valueDeltaKind drep `andValueDeltaKinds` (valueDeltaKind dFileInfo)) 
			return (Just (key,rep'),repkind)
		else return (Nothing,NonStable)

-- we try @skipUnevaluated@ first to avoid forcing unecessary original data
zstopLoadIf :: (IncK (IncForest fs) irep,ForestMD fs rep,ICRep fs,ForestRep rep (ForestFSThunkI fs irep),StableMD fs rep) =>
	String -> Bool -> ForestI fs FilePath -> FilePath -> FSTreeDeltaNodeMay -> FSTree fs -> ValueDelta fs rep -> (rep,GetForestMD fs)
	-> (GetForestMD fs -> ForestI fs rep) -- static loading function
	-> (ValueDelta fs rep -> (rep,GetForestMD fs) -> ForestO fs (SValueDelta rep)) -- delta loading function
	-> ForestO fs (SValueDelta rep)
zstopLoadIf str isEmptyEnv mpath path' df tree' dv repmd load loadD = zskipLoadUnevaluated str tree' repmd load $ \newrepmd -> do
	path <- inside mpath
	zskipLoadIf3 str (isEmptyEnv && isIdValueDelta dv) (path == path') (isEmptyFSTreeDeltaNodeMay df) $ loadD dv newrepmd

zskipLoadIf3 :: ICRep fs => String -> Bool -> Bool -> Bool -> ForestO fs (SValueDelta rep) -> ForestO fs (SValueDelta rep)
zskipLoadIf3 str b1 b2 b3 io = if b1 && b2 && b3
	then debug ("skippedUpdate "++str) $ return Id
	else io
	
zskipLoadUnevaluated :: (IncK (IncForest fs) irep,ICRep fs,ForestRep rep (ForestFSThunkI fs irep),ForestMD fs rep,StableMD fs rep) =>
	String -> FSTree fs -> (rep,GetForestMD fs)
	-> (GetForestMD fs -> ForestI fs rep) -- static loading function
	-> ((rep,GetForestMD fs) -> ForestO fs (SValueDelta rep)) -- delta loading function
	-> ForestO fs (SValueDelta rep)
zskipLoadUnevaluated str tree' olddata@(rep,getMD) load loadD = do
	let isoRep = iso_rep_thunk
	let rep_thunk = to isoRep rep
	cond1 <- inside $ isUnevaluatedFSThunk rep_thunk
	if cond1
		then do
			overwrite rep_thunk $ Inc.get =<< liftM (to isoRep) (load getMD)
			debug ("skippedUnevaluated "++str++" ") $ return Delta
		else do
			{-debug ("NOT UNEVALUATED "++str++" "++show cond1++" "++show cond2) $-} loadD olddata
