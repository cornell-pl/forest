{-# LANGUAGE TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}

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

doZDefaultFile1 :: (Pads1 arg pads md,Typeable arg,Eq pads,Eq md,Typeable pads,Typeable md,ZippedICMemo fs) => Pure.Arg arg -> FilePath -> ForestI fs (ForestFSThunkI fs ((Forest_md fs,md),pads))
doZDefaultFile1 (Pure.Arg arg :: Pure.Arg arg) path = do
	let argProxy = Proxy :: Proxy (Pure.Arg arg)
	rep_thunk <- fsThunk $ doZDefaultFile1' (Pure.Arg arg) path
	addZippedMemo path argProxy (return arg) rep_thunk Nothing
	return rep_thunk

doZDefaultFile1' :: (Pads1 arg pads md,Typeable arg,Eq pads,Eq md,Typeable pads,Typeable md,ZippedICMemo fs) => Pure.Arg arg -> FilePath -> ForestI fs ((Forest_md fs,md),pads)
doZDefaultFile1' (Pure.Arg arg) path = do
	let rep = Pure.forestdefault
	let md = Pads.defaultMd1 arg rep
	fmd <- missingPathForestMD path
	fmd' <- updateForestMDErrorsInsideWithPadsMD fmd (return md) -- adds the Pads errors
	return $ ((fmd',md),rep)

doZDefaultArchive :: (ForestMD fs rep,Eq rep,Typeable rep,ZippedICMemo fs) => FilePath -> (FilePath -> ForestI fs rep) -> ForestI fs (ForestFSThunkI fs (Forest_md fs,rep))
doZDefaultArchive path doContent = do
	let argsProxy = Proxy :: Proxy ()
	rep_thunk <- fsThunk $ doZDefaultArchive' path doContent
	addZippedMemo path argsProxy () rep_thunk Nothing
	return rep_thunk

doZDefaultArchiveInner :: (ForestMD fs rep,Eq rep,Typeable rep,	Input (FSThunk fs) Inside (IncForest fs) IORef IO) => FilePath -> (FilePath -> ForestI fs rep) -> ForestI fs rep
doZDefaultArchiveInner path doContent = doContent (cardinalPath path)

doZDefaultArchive' :: (ForestMD fs rep,Eq rep,Typeable rep,	Input (FSThunk fs) Inside (IncForest fs) IORef IO) => FilePath -> (FilePath -> ForestI fs rep) -> ForestI fs (Forest_md fs,rep)
doZDefaultArchive' path doContent = do
	rep <- doContent (cardinalPath path)
	fmd <- missingPathForestMD path
	fmd' <- updateForestMDErrorsInsideWith fmd $ liftM (:[]) $ get_errors rep -- like a directory
	return (fmd',rep)

doZDefaultSymLink :: (ICRep fs,Input (FSThunk fs) Inside (IncForest fs) IORef IO) => FilePath -> ForestI fs (SymLink fs)
doZDefaultSymLink path = liftM SymLink $ fsThunk $ doZDefaultSymLink' path

doZDefaultSymLink' :: Input (FSThunk fs) Inside (IncForest fs) IORef IO => FilePath -> ForestI fs ((Forest_md fs,Base_md),FilePath)
doZDefaultSymLink' path = do
	fmd <- missingPathForestMD path
	return ((fmd,errorBasePD path),"")

doZDefaultDirectory :: (Eq rep,Typeable rep,ICRep fs,ForestInput fs FSThunk Inside,Eq rep)
	=> FilePath -> (rep -> ForestI fs Forest_err) -> ForestI fs rep -> ForestI fs (ForestFSThunkI fs (Forest_md fs,rep))
doZDefaultDirectory path collectMDErrors doContent = fsThunk $ doZDefaultDirectory' path collectMDErrors doContent

doZDefaultDirectory' :: (ICRep fs,ForestInput fs FSThunk Inside,Eq rep)
	=> FilePath -> (rep -> ForestI fs Forest_err) -> ForestI fs rep -> ForestI fs (Forest_md fs,rep)
doZDefaultDirectory' path collectMDErrors doContent = do
	rep <- doContent
	fmd <- missingDirForestMD path
	fmd' <- updateForestMDErrorsInsideWith fmd $ liftM (:[]) $ collectMDErrors rep
	return (fmd',rep)

doZDefaultMaybe :: (Eq rep,Typeable rep,ICRep fs) => FilePath -> ForestI fs (ForestFSThunkI fs (Forest_md fs,Maybe rep))
doZDefaultMaybe path = fsThunk $ doZDefaultMaybe' path

doZDefaultMaybeInner :: (ICRep fs) => FilePath -> ForestI fs (Maybe rep)
doZDefaultMaybeInner path = return Nothing

doZDefaultMaybe' :: (ICRep fs) => FilePath -> ForestI fs (Forest_md fs,Maybe rep)
doZDefaultMaybe' path = do
	fmd <- cleanForestMDwithFile path
	return (fmd,Nothing)

doZDefaultConstraint :: (ForestOutput fs ICThunk Inside,ForestMD fs rep,MData NoCtx (ForestI fs) rep) =>
	(rep -> ForestI fs Bool) -> ForestI fs rep -> ForestI fs rep
doZDefaultConstraint pred load = do -- note that constraints do not consider the current path
	rep <- load
	rep' <- replace_errors rep $ \err -> do
		cond <- pred rep
		if cond
			then return err
			else return $ Pure.updateForestErr err [Pure.constraintViolationForestErr]
	return rep'

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

doZDefaultSimpleWithConstraint :: (ForestOutput fs ICThunk Inside,ForestMD fs rep,Matching fs a,MData NoCtx (ForestI fs) rep) =>
	FilePath -> ForestI fs a -> (rep -> ForestI fs Bool)
	-> (FilePath -> ForestI fs rep)
	-> ForestI fs rep
doZDefaultSimpleWithConstraint path matching pred load = doZDefaultConstraint pred $ matching >>= \m -> doZDefaultFocus path m load

doZDefaultCompound :: (Typeable container_rep,Eq container_rep,Matching fs a,MData NoCtx (ForestI fs) rep',ForestMD fs rep') =>
	FilePath -> ForestI fs a
	-> ([(FilePath,rep')] -> container_rep)
	-> (FileName -> ForestFSThunkI fs Pure.FileInfo -> FilePath -> ForestI fs rep')
	-> ForestI fs container_rep
doZDefaultCompound path matchingM buildContainerRep load = debug ("doLoadCompound: "++show path) $ do
	matching <- matchingM
	files <- forestM $ defaultMatch Proxy matching
	let loadEach n = liftM (n,) $ doZDefaultFocus path n $ \newpath -> do
		fileInfo_thunk <- ref $ Pure.mkErrorFileInfo (path </> n)
		rep' <- load n fileInfo_thunk newpath
		return rep'
	loadlist <- mapM loadEach files
	return $ buildContainerRep loadlist

doZDefaultCompoundWithConstraint :: (Typeable container_rep,Eq container_rep,ForestOutput fs ICThunk Inside,Matching fs a,MData NoCtx (ForestI fs) rep',ForestMD fs rep') =>
	FilePath -> ForestI fs a
	-> (FilePath -> ForestFSThunkI fs Pure.FileInfo -> ForestI fs Bool)
	-> ([(FilePath,rep')] -> container_rep)
	-> (FileName -> ForestFSThunkI fs Pure.FileInfo -> FilePath -> ForestI fs rep')
	-> ForestI fs container_rep
doZDefaultCompoundWithConstraint path matchingM pred buildContainerRep load = debug ("doLoadCompound: "++show path) $ do
	matching <- matchingM -- matching expressions are not saved for incremental reuse
	files <- forestM $ defaultMatch Proxy matching
	let makeInfo n = do
		t <- ref $ Pure.mkErrorFileInfo (path </> n) -- we store the @FileInfo@ in a @FSThunk@ to allow incremental evaluation of the constraint expression
		u <- icThunk $ pred n t --the filename is a constant. during delta loading, whenever it changes we will load from scratch
		return (n,(t,u))
	filesmetasInfo <- mapM makeInfo files
	filesmetasInfo' <- filterM (force . snd . snd) filesmetasInfo
	let loadEach (n,(t,u)) = do
		rep <- doZDefaultFocus path n $ load n t
		return (n,rep)
	loadlist <- mapM loadEach filesmetasInfo'
	return (buildContainerRep loadlist)

