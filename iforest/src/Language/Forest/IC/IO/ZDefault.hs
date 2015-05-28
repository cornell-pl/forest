{-# LANGUAGE ConstraintKinds, TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}

module Language.Forest.IC.IO.ZDefault where

import Language.Forest.IO.Utils
import Language.Forest.Syntax
import Language.Pads.Padsc as Pads
import Language.Forest.IC.MetaData
import Language.Forest.Errors
--import qualified Language.Forest.MetaData as Pure
import Language.Forest.IO.Shell
import Data.IORef
import Control.Monad.Reader

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
import Language.Forest.IC.BX as BX

import qualified Control.Exception as CE

import Data.Data
import Data.Maybe
import System.Random
import Data.Proxy

import Language.Forest.IC.Generic
import Language.Forest.IC.ICRep
import Control.Monad.Incremental
import Language.Forest.IC.MetaData
import Language.Forest.IC.Default
import Data.WithClass.MData

doZDefaultNamed :: (FTK fs rep,ZippedICForest fs rep) =>
	Proxy rep -> ForestIs fs (ForestArgs rep) -> FilePath -> FSTree fs -> ForestI fs (ForestFSThunkI fs rep)
doZDefaultNamed proxy args path tree = fsThunk tree $ zdefaultScratchGeneric proxy Proxy args path tree

doZDefaultFile1 :: (
	((Forest_md fs,md),pads) ~ ForestRepTy fs (ForestFSThunkI fs ((Forest_md fs,md),pads))
	,((FileInfo,md),padsc) ~ ForestContentTy fs ((Forest_md fs,md),pads)
	,ForestArgs (ForestFSThunkI fs ((Forest_md fs,md),pads)) ~ Arg arg,IncK (IncForest fs) FileInfo,FTK fs (ForestFSThunkI fs ((Forest_md fs,md),pads))
	,IncK (IncForest fs) Forest_err,IncK (IncForest fs) ((Forest_md fs, md), pads),Pads1 arg pads md,Typeable arg,ZippedICMemo fs)
	=> Proxy fs -> ForestI fs arg -> FilePath -> FSTree fs -> ForestI fs (ForestFSThunkI fs ((Forest_md fs,md),pads))
doZDefaultFile1 fs (marg :: ForestI fs arg) path tree = do
	let argProxy = Proxy :: Proxy (Arg arg)
	rep_thunk <- fsThunk tree $ doZDefaultFile1' marg path tree
	addZippedMemo path marg rep_thunk Nothing
	return rep_thunk

doZDefaultFileInner1 :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,Pads1 arg pads md,Typeable arg,ZippedICMemo fs) => ForestI fs arg -> FilePath -> FSTree fs -> ForestI fs ((Forest_md fs,md),pads)
doZDefaultFileInner1 marg path tree = doZDefaultFile1' marg path tree

doZDefaultFile1' :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,Pads1 arg pads md,Typeable arg,ZippedICMemo fs) => ForestI fs arg -> FilePath -> FSTree fs -> ForestI fs ((Forest_md fs,md),pads)
doZDefaultFile1' (marg) path tree = do
	arg <- marg
	let rep = forestdefault
	let md = Pads.defaultMd1 arg rep
	fmd <- missingPathForestMD tree path
	fmd' <- updateForestMDErrorsInsideWithPadsMD tree fmd $ return md
	debug ("doZDefaultFile1 ") $ return $ ((fmd',md),rep)

doZDefaultArchive :: (IncK (IncForest fs) (Forest_md fs, rep),Typeable rep,ForestMD fs rep,ZippedICMemo fs) => Bool -> FilePath -> FSTree fs -> (FilePath -> FSTree fs -> ForestI fs rep) -> ForestI fs (ForestFSThunkI fs (Forest_md fs,rep))
doZDefaultArchive isClosed path tree doContent = do
	let argsProxy = Proxy :: Proxy ()
	rep_thunk <- fsThunk tree $ doZDefaultArchiveInner path tree doContent
--	when isClosed $ addZippedMemo path argsProxy () rep_thunk Nothing
	return rep_thunk

doZDefaultArchiveInner :: (ForestMD fs rep,ForestInput fs FSThunk Inside) => FilePath -> FSTree fs -> (FilePath -> FSTree fs -> ForestI fs rep) -> ForestI fs (Forest_md fs,rep)
doZDefaultArchiveInner path tree doContent = do
	fmd <- missingPathForestMD tree path
	avfsTree <- forestM $ virtualTree tree
	rep <- doContent (cardinalPath path) avfsTree
	fmd' <- updateForestMDErrorsInsideWith tree fmd $ liftM (:[]) $ get_errors rep -- like a directory
	return (fmd',rep)

doZDefaultSymLink :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,IncK (IncForest fs) ((Forest_md fs, Base_md), FilePath),ICRep fs,ForestInput fs FSThunk Inside) => FilePath -> FSTree fs -> ForestI fs (ForestFSThunkI fs ((Forest_md fs,Base_md),FilePath))
doZDefaultSymLink path tree = fsThunk tree $ doZDefaultSymLink' path tree

doZDefaultSymLinkInner :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,IncK (IncForest fs) ((Forest_md fs, Base_md), FilePath),ICRep fs,ForestInput fs FSThunk Inside) => FilePath -> FSTree fs -> ForestI fs ((Forest_md fs,Base_md),FilePath)
doZDefaultSymLinkInner path tree = doZDefaultSymLink' path tree

doZDefaultSymLink' :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,IncK (IncForest fs) ((Forest_md fs, Base_md), FilePath),ICRep fs,ForestInput fs FSThunk Inside) => FilePath -> FSTree fs -> ForestI fs ((Forest_md fs,Base_md),FilePath)
doZDefaultSymLink' path tree = debug ("defaultSymLink "++path) $ do
	fmd <- missingPathForestMD tree path
	return ((fmd,errorBasePD path),"")

doZDefaultDirectory :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,IncK (IncForest fs) (Forest_md fs, rep),ICRep fs,ForestInput fs FSThunk Inside)
	=> FilePath -> FSTree fs -> (rep -> ForestI fs Forest_err) -> ForestI fs rep -> ForestI fs (ForestFSThunkI fs (Forest_md fs,rep))
doZDefaultDirectory path tree collectMDErrors doContent = fsThunk tree $ doZDefaultDirectory' path tree collectMDErrors doContent

doZDefaultDirectory' :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ICRep fs,ForestInput fs FSThunk Inside)
	=> FilePath -> FSTree fs -> (rep -> ForestI fs Forest_err) -> ForestI fs rep -> ForestI fs (Forest_md fs,rep)
doZDefaultDirectory' path tree collectMDErrors doContent = do
	rep <- doContent
	fmd <- missingDirForestMD tree path
	fmd' <- updateForestMDErrorsInsideWith tree fmd $ liftM (:[]) $ collectMDErrors rep
	return (fmd',rep)

doZDefaultMaybe :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,IncK (IncForest fs) (Forest_md fs, Maybe rep),ICRep fs) => FilePath -> FSTree fs -> ForestI fs (ForestFSThunkI fs (Forest_md fs,Maybe rep))
doZDefaultMaybe path tree = fsThunk tree $ doZDefaultMaybe' path tree

doZDefaultMaybeInner :: (ICRep fs) => FilePath -> FSTree fs -> ForestI fs (Maybe rep)
doZDefaultMaybeInner path tree = return Nothing

doZDefaultMaybe' :: (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ICRep fs) => FilePath -> FSTree fs -> ForestI fs (Forest_md fs,Maybe rep)
doZDefaultMaybe' path tree = do
	fmd <- cleanForestMDwithFile tree path
	return (fmd,Nothing)

doZDefaultConstraint :: (IncK (IncForest fs) FileInfo,ForestMD fs rep,ForestMD fs (ForestContentTy fs rep),ForestContent fs rep,IncK (IncForest fs) (ForestFSThunkI fs Forest_err, rep),ForestOutput fs ICThunk Inside,MData NoCtx (ForestI fs) rep) =>
	(ForestContentTy fs rep -> ForestI fs Bool) -> FSTree fs -> ForestI fs rep -> ForestI fs (ForestFSThunkI fs (ForestFSThunkI fs Forest_err,rep))
doZDefaultConstraint pred tree load = fsThunk tree $ doZDefaultConstraintInner pred tree load

doZDefaultConstraintInner :: (IncK (IncForest fs) FileInfo,ForestMD fs rep,ForestMD fs (ForestContentTy fs rep),ForestContent fs rep,ForestOutput fs ICThunk Inside,MData NoCtx (ForestI fs) rep) =>
	(ForestContentTy fs rep -> ForestI fs Bool) -> FSTree fs -> ForestI fs rep -> ForestI fs (ForestFSThunkI fs Forest_err,rep)
doZDefaultConstraintInner pred tree load = do -- note that constraints do not consider the current path
	rep <- load
	err_t <- fsThunk tree $ do
		err_cond <- predForestErr . pred =<< runReaderT (BX.getM lens_content (return rep)) tree
		err_inner <- get_errors rep
		return $ mergeForestErrs err_cond err_inner
	return (err_t,rep)

doZDefaultFocus :: (ICRep fs,Matching fs a,ForestMD fs rep) => FilePath -> FSTree fs -> a -> (FilePath -> ForestI fs rep) -> ForestI fs rep
doZDefaultFocus path tree matching load = do
	files <- forestM $ defaultMatch Proxy matching
	case files of
		[file] -> load (path </> file)
		files -> do
			let newpath = (path </> pickFile files)
			rep <- load newpath
			rep' <- if length files == 0
				then return rep -- if there is no match then an error will pop from the recursive load
				else addMultipleMatchesErrorMDInside tree newpath files rep
			return rep'

doZDefaultSimple :: (ForestMD fs rep,Matching fs a,MData NoCtx (ForestI fs) rep) =>
	FilePath -> FSTree fs -> ForestI fs a
	-> (FilePath -> ForestI fs rep)
	-> ForestI fs rep
doZDefaultSimple path tree matching load = matching >>= \m -> doZDefaultFocus path tree m load

doZDefaultSimpleWithConstraint :: (ForestMD fs rep,ForestMD fs (ForestContentTy fs rep),ForestContent fs rep,ForestOutput fs ICThunk Inside,Matching fs a,MData NoCtx (ForestI fs) rep) =>
	FilePath -> FSTree fs -> ForestI fs a -> (ForestContentTy fs rep -> ForestI fs Bool)
	-> (FilePath -> ForestI fs rep)
	-> ForestI fs (ForestFSThunkI fs Forest_err,rep)
doZDefaultSimpleWithConstraint path tree matching pred load = doZDefaultConstraintInner pred tree $ matching >>= \m -> doZDefaultFocus path tree m load

doZDefaultCompound :: (Pads1 key_arg key key_md,IncK (IncForest fs) FileInfo,Matching fs a,MData NoCtx (ForestI fs) rep',ForestMD fs rep') =>
	FilePath -> FSTree fs -> ForestI fs a
	-> ForestI fs key_arg -> ([(key,rep')] -> container_rep)
	-> (key -> ForestFSThunkI fs FileInfo -> FilePath -> ForestI fs rep')
	-> ForestI fs container_rep
doZDefaultCompound path tree matchingM mkeyarg buildContainerRep load = debug ("doLoadCompoundDef: "++show path) $ do
	key_arg <- mkeyarg
	matching <- matchingM
	files <- forestM $ defaultMatch Proxy matching
	let loadEach n = do
		let key = fst $ Pads.parseString1 key_arg n
		liftM (key,) $ doZDefaultFocus path tree n $ \newpath -> do
			fileInfo_thunk <- fsRef tree $ mkErrorFileInfo (path </> n)
			load key fileInfo_thunk newpath
	loadlist <- mapM loadEach files
	return $ buildContainerRep loadlist

doZDefaultCompoundWithConstraint :: (Pads1 key_arg key key_md,IncK (IncForest fs) FileInfo,IncK (IncForest fs) Bool,ForestOutput fs ICThunk Inside,Matching fs a,MData NoCtx (ForestI fs) rep',ForestMD fs rep') =>
	FilePath -> FSTree fs -> ForestI fs a
	-> (key -> ForestFSThunkI fs FileInfo -> ForestI fs Bool)
	-> ForestI fs key_arg -> ([(key,rep')] -> container_rep)
	-> (key -> ForestFSThunkI fs FileInfo -> FilePath -> ForestI fs rep')
	-> ForestI fs container_rep
doZDefaultCompoundWithConstraint path tree matchingM pred mkeyarg buildContainerRep load = debug ("doLoadCompoundDefK: "++show path) $ do
	key_arg <- mkeyarg
	matching <- matchingM -- matching expressions are not saved for incremental reuse
	files <- forestM $ defaultMatch Proxy matching
	let makeInfo n = do
		let key = fst $ Pads.parseString1 key_arg n
		t <- fsRef tree $ mkErrorFileInfo (path </> n) -- we store the @FileInfo@ in a @FSThunk@ to allow incremental evaluation of the constraint expression
		u <- icThunk $ pred key t --the filename is a constant. during delta loading, whenever it changes we will load from scratch
		return ((n,key),(t,u))
	filesmetasInfo <- mapM makeInfo files
	filesmetasInfo' <- filterM (force . snd . snd) filesmetasInfo
	let loadEach ((n,key),(t,u)) = do
		rep <- doZDefaultFocus path tree n $ load key t
		return (key,rep)
	loadlist <- mapM loadEach filesmetasInfo'
	return (buildContainerRep loadlist)

