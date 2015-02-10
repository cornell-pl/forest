{-# LANGUAGE ConstraintKinds, TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}

module Language.Forest.IC.IO.ZDefault where

import Language.Forest.IO.Utils
import Language.Forest.Syntax
import Language.Pads.Padsc as Pads
import Language.Forest.IC.MetaData
import Language.Forest.Errors
import qualified Language.Forest.Pure.MetaData as Pure
import Language.Forest.IO.Shell
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
import Language.Forest.IC.BX as BX

import qualified Control.Exception as CE

import Data.Data
import Data.Maybe
import System.Random
import Data.Proxy
import Control.Monad.Lazy
import Language.Forest.IC.Generic
import Language.Forest.IC.ICRep
import Control.Monad.Incremental
import Language.Forest.IC.MetaData
import Data.WithClass.MData

doZDefaultFile1 :: (IncK (IncForest fs) Forest_err,IncK (IncForest fs) ((Forest_md fs, md), pads),Pads1 arg pads md,Typeable arg,ZippedICMemo fs) => Pure.Arg arg -> FilePath -> ForestI fs (ForestFSThunkI fs ((Forest_md fs,md),pads))
doZDefaultFile1 (Pure.Arg arg :: Pure.Arg arg) path = do
	let argProxy = Proxy :: Proxy (Pure.Arg arg)
	rep_thunk <- fsThunk $ doZDefaultFile1' (Pure.Arg arg) path
	addZippedMemo path argProxy (return arg) rep_thunk Nothing
	return rep_thunk

doZDefaultFileInner1 :: (IncK (IncForest fs) Forest_err,Pads1 arg pads md,Typeable arg,ZippedICMemo fs) => Pure.Arg arg -> FilePath -> ForestI fs ((Forest_md fs,md),pads)
doZDefaultFileInner1 arg path = doZDefaultFile1' arg path

doZDefaultFile1' :: (IncK (IncForest fs) Forest_err,Pads1 arg pads md,Typeable arg,ZippedICMemo fs) => Pure.Arg arg -> FilePath -> ForestI fs ((Forest_md fs,md),pads)
doZDefaultFile1' (Pure.Arg arg) path = do
	let rep = Pure.forestdefault
	let md = Pads.defaultMd1 arg rep
	fmd <- missingPathForestMD path
	fmd' <- updateForestMDErrorsInsideWithPadsMD fmd $ return md
	debug ("doZDefaultFile1 ") $ return $ ((fmd',md),rep)

doZDefaultArchive :: (IncK (IncForest fs) (Forest_md fs, rep),Typeable rep,ForestMD fs rep,ZippedICMemo fs) => FilePath -> (FilePath -> ForestI fs rep) -> ForestI fs (ForestFSThunkI fs (Forest_md fs,rep))
doZDefaultArchive path doContent = do
	let argsProxy = Proxy :: Proxy ()
	rep_thunk <- fsThunk $ doZDefaultArchiveInner path doContent
	addZippedMemo path argsProxy () rep_thunk Nothing
	return rep_thunk

doZDefaultArchiveInner :: (ForestMD fs rep,Input (FSThunk fs) Inside (IncForest fs) IORef IO) => FilePath -> (FilePath -> ForestI fs rep) -> ForestI fs (Forest_md fs,rep)
doZDefaultArchiveInner path doContent = do
	rep <- doContent (cardinalPath path)
	fmd <- missingPathForestMD path
	fmd' <- updateForestMDErrorsInsideWith fmd $ liftM (:[]) $ get_errors rep -- like a directory
	return (fmd',rep)

doZDefaultSymLink :: (IncK (IncForest fs) Forest_err,IncK (IncForest fs) ((Forest_md fs, Base_md), FilePath),ICRep fs,Input (FSThunk fs) Inside (IncForest fs) IORef IO) => FilePath -> ForestI fs (ForestFSThunkI fs ((Forest_md fs,Base_md),FilePath))
doZDefaultSymLink path = fsThunk $ doZDefaultSymLink' path

doZDefaultSymLinkInner :: (IncK (IncForest fs) Forest_err,IncK (IncForest fs) ((Forest_md fs, Base_md), FilePath),ICRep fs,Input (FSThunk fs) Inside (IncForest fs) IORef IO) => FilePath -> ForestI fs ((Forest_md fs,Base_md),FilePath)
doZDefaultSymLinkInner path = doZDefaultSymLink' path

doZDefaultSymLink' :: (IncK (IncForest fs) Forest_err,IncK (IncForest fs) ((Forest_md fs, Base_md), FilePath),ICRep fs,Input (FSThunk fs) Inside (IncForest fs) IORef IO) => FilePath -> ForestI fs ((Forest_md fs,Base_md),FilePath)
doZDefaultSymLink' path = debug ("defaultSymLink "++path) $ do
	fmd <- missingPathForestMD path
	return ((fmd,errorBasePD path),"")

doZDefaultDirectory :: (IncK (IncForest fs) Forest_err,IncK (IncForest fs) (Forest_md fs, rep),ICRep fs,ForestInput fs FSThunk Inside)
	=> FilePath -> (rep -> ForestI fs Forest_err) -> ForestI fs rep -> ForestI fs (ForestFSThunkI fs (Forest_md fs,rep))
doZDefaultDirectory path collectMDErrors doContent = fsThunk $ doZDefaultDirectory' path collectMDErrors doContent

doZDefaultDirectory' :: (IncK (IncForest fs) Forest_err,ICRep fs,ForestInput fs FSThunk Inside)
	=> FilePath -> (rep -> ForestI fs Forest_err) -> ForestI fs rep -> ForestI fs (Forest_md fs,rep)
doZDefaultDirectory' path collectMDErrors doContent = do
	rep <- doContent
	fmd <- missingDirForestMD path
	fmd' <- updateForestMDErrorsInsideWith fmd $ liftM (:[]) $ collectMDErrors rep
	return (fmd',rep)

doZDefaultMaybe :: (IncK (IncForest fs) Forest_err,IncK (IncForest fs) (Forest_md fs, Maybe rep),ICRep fs) => FilePath -> ForestI fs (ForestFSThunkI fs (Forest_md fs,Maybe rep))
doZDefaultMaybe path = fsThunk $ doZDefaultMaybe' path

doZDefaultMaybeInner :: (ICRep fs) => FilePath -> ForestI fs (Maybe rep)
doZDefaultMaybeInner path = return Nothing

doZDefaultMaybe' :: (IncK (IncForest fs) Forest_err,ICRep fs) => FilePath -> ForestI fs (Forest_md fs,Maybe rep)
doZDefaultMaybe' path = do
	fmd <- cleanForestMDwithFile path
	return (fmd,Nothing)

doZDefaultConstraint :: (ForestMD fs rep,ForestMD fs content,ForestContent rep content,IncK (IncForest fs) (ForestFSThunkI fs Forest_err, rep),ForestOutput fs ICThunk Inside,MData NoCtx (ForestI fs) rep) =>
	(content -> ForestI fs Bool) -> ForestI fs rep -> ForestI fs (ForestFSThunkI fs (ForestFSThunkI fs Forest_err,rep))
doZDefaultConstraint pred load = fsThunk $ doZDefaultConstraintInner pred load

doZDefaultConstraintInner :: (ForestMD fs rep,ForestMD fs content,ForestContent rep content,ForestOutput fs ICThunk Inside,MData NoCtx (ForestI fs) rep) =>
	(content -> ForestI fs Bool) -> ForestI fs rep -> ForestI fs (ForestFSThunkI fs Forest_err,rep)
doZDefaultConstraintInner pred load = do -- note that constraints do not consider the current path
	rep <- load
	err_t <- fsThunk $ do
		err_cond <- predForestErr $ pred $ BX.get lens_content rep
		err_inner <- get_errors rep
		return $ Pure.mergeForestErrs err_cond err_inner
	return (err_t,rep)

doZDefaultFocus :: (ICRep fs,Matching fs a,ForestMD fs rep) => FilePath -> a -> (FilePath -> ForestI fs rep) -> ForestI fs rep
doZDefaultFocus path matching load = do
	files <- forestM $ defaultMatch Proxy matching
	case files of
		[file] -> load (path </> file)
		files -> do
			let newpath = (path </> pickFile files)
			rep <- load newpath
			rep' <- if length files == 0
				then return rep -- if there is no match then an error will pop from the recursive load
				else addMultipleMatchesErrorMDInside newpath files rep
			return rep'

doZDefaultSimple :: (ForestMD fs rep,Matching fs a,MData NoCtx (ForestI fs) rep) =>
	FilePath -> ForestI fs a
	-> (FilePath -> ForestI fs rep)
	-> ForestI fs rep
doZDefaultSimple path matching load = matching >>= \m -> doZDefaultFocus path m load

doZDefaultSimpleWithConstraint :: (ForestMD fs rep,ForestMD fs content,ForestContent rep content,ForestOutput fs ICThunk Inside,Matching fs a,MData NoCtx (ForestI fs) rep) =>
	FilePath -> ForestI fs a -> (content -> ForestI fs Bool)
	-> (FilePath -> ForestI fs rep)
	-> ForestI fs (ForestFSThunkI fs Forest_err,rep)
doZDefaultSimpleWithConstraint path matching pred load = doZDefaultConstraintInner pred $ matching >>= \m -> doZDefaultFocus path m load

doZDefaultCompound :: (Pads1 key_arg key key_md,IncK (IncForest fs) Pure.FileInfo,Matching fs a,MData NoCtx (ForestI fs) rep',ForestMD fs rep') =>
	FilePath -> ForestI fs a
	-> ForestI fs key_arg -> ([(key,rep')] -> container_rep)
	-> (key -> ForestFSThunkI fs Pure.FileInfo -> FilePath -> ForestI fs rep')
	-> ForestI fs container_rep
doZDefaultCompound path matchingM mkeyarg buildContainerRep load = debug ("doLoadCompound: "++show path) $ do
	key_arg <- mkeyarg
	matching <- matchingM
	files <- forestM $ defaultMatch Proxy matching
	let loadEach n = do
		let key = fst $ Pads.parseString1 key_arg n
		liftM (key,) $ doZDefaultFocus path n $ \newpath -> do
			fileInfo_thunk <- ref $ Pure.mkErrorFileInfo (path </> n)
			load key fileInfo_thunk newpath
	loadlist <- mapM loadEach files
	return $ buildContainerRep loadlist

doZDefaultCompoundWithConstraint :: (Pads1 key_arg key key_md,IncK (IncForest fs) Pure.FileInfo,IncK (IncForest fs) Bool,ForestOutput fs ICThunk Inside,Matching fs a,MData NoCtx (ForestI fs) rep',ForestMD fs rep') =>
	FilePath -> ForestI fs a
	-> (key -> ForestFSThunkI fs Pure.FileInfo -> ForestI fs Bool)
	-> ForestI fs key_arg -> ([(key,rep')] -> container_rep)
	-> (key -> ForestFSThunkI fs Pure.FileInfo -> FilePath -> ForestI fs rep')
	-> ForestI fs container_rep
doZDefaultCompoundWithConstraint path matchingM pred mkeyarg buildContainerRep load = debug ("doLoadCompound: "++show path) $ do
	key_arg <- mkeyarg
	matching <- matchingM -- matching expressions are not saved for incremental reuse
	files <- forestM $ defaultMatch Proxy matching
	let makeInfo n = do
		let key = fst $ Pads.parseString1 key_arg n
		t <- ref $ Pure.mkErrorFileInfo (path </> n) -- we store the @FileInfo@ in a @FSThunk@ to allow incremental evaluation of the constraint expression
		u <- icThunk $ pred key t --the filename is a constant. during delta loading, whenever it changes we will load from scratch
		return ((n,key),(t,u))
	filesmetasInfo <- mapM makeInfo files
	filesmetasInfo' <- filterM (force . snd . snd) filesmetasInfo
	let loadEach ((n,key),(t,u)) = do
		rep <- doZDefaultFocus path n $ load key t
		return (key,rep)
	loadlist <- mapM loadEach filesmetasInfo'
	return (buildContainerRep loadlist)

