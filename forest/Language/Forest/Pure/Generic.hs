{-# LANGUAGE GADTs, ConstraintKinds, TemplateHaskell, DeriveDataTypeable, UndecidableInstances, TypeOperators, TypeFamilies, DataKinds, KindSignatures, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NamedFieldPuns #-}
{-
** *********************************************************************
*                                                                      *
*              This software is part of the pads package               *
*           Copyright (c) 2005-2011 AT&T Knowledge Ventures            *
*                      and is licensed under the                       *
*                        Common Public License                         *
*                      by AT&T Knowledge Ventures                      *
*                                                                      *
*                A copy of the License is available at                 *
*                    www.padsproj.org/License.html                     *
*                                                                      *
*  This program contains certain software code or other information    *
*  ("AT&T Software") proprietary to AT&T Corp. ("AT&T").  The AT&T     *
*  Software is provided to you "AS IS". YOU ASSUME TOTAL RESPONSIBILITY*
*  AND RISK FOR USE OF THE AT&T SOFTWARE. AT&T DOES NOT MAKE, AND      *
*  EXPRESSLY DISCLAIMS, ANY EXPRESS OR IMPLIED WARRANTIES OF ANY KIND  *
*  WHATSOEVER, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF*
*  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, WARRANTIES OF  *
*  TITLE OR NON-INFRINGEMENT.  (c) AT&T Corp.  All rights              *
*  reserved.  AT&T is a registered trademark of AT&T Corp.             *
*                                                                      *
*                   Network Services Research Center                   *
*                          AT&T Labs Research                          *
*                           Florham Park NJ                            *
*                                                                      *
*              Kathleen Fisher <kfisher@research.att.com>              *
*                                                                      *
************************************************************************
-}


module Language.Forest.Pure.Generic   where

import Control.Exception
import Data.Monoid
import Language.Pads.Generic
import Language.Forest.Pure.MetaData
import Language.Forest.Manifest
import System.FilePath.Posix
import Data.Map hiding (map)
import Data.Set hiding (map)
import qualified Data.List as L
import Language.Forest.FS.FSRep
import Data.Proxy
import Control.Monad       
import Data.IORef
import Data.Data
import Data.Generics

-- * Transactional Forest interface

-- | The Forest transaction monad
type FTM fs = ForestM fs

class (FSRep fs) => TransactionalPureForest fs where
	
	atomically :: FTM fs a -> IO a
	
	retry :: FTM fs a
	
	orElse :: FTM fs a -> FTM fs a -> FTM fs a
	
	throw :: Exception e => e -> FTM fs a
	catch :: Exception e => FTM fs a -> (e -> FTM fs a) -> FTM fs a

-- * Automatically generated Forest code

-- A Class for the types of Forest specifications
class (FSRep fs,Data rep,ForestMD md) => PureForest fs args rep md | rep -> md, rep -> args  where
	
	load :: args -> FilePath -> ForestM fs (rep,md)
	load args path = latestTree >>= \tree -> loadWithTree args path tree getForestMDInTree
	
	-- automatically generated load function
	loadWithTree :: args -> FilePath -> FSTree fs -> GetForestMD fs -> ForestM fs (rep,md)
	
	manifest :: args -> (rep,md) -> ForestM fs (Manifest fs)
	manifest args dta = latestTree >>= \tree -> newManifestWith "/" tree >>= updateManifestWithTree args tree dta
	
	-- automatically generated manifest function
	updateManifestWithTree :: args -> FSTree fs -> (rep,md) -> Manifest fs -> ForestM fs (Manifest fs)
	
	-- | generates default metadata based on the specification
	-- note that @defaultMd@ tries to generate default metadata that is as consistent as possible with the representation, but cannot achieve that in general, e.g., if there is a user-specified constraint
	defaultMd :: args -> rep -> FilePath -> ForestM fs md


store :: FSRep fs => Manifest fs -> ForestM fs ()
store = storeManifest

listDirs :: Data  md => md -> [FilePath] 
listDirs = map fullpath . listify (\(r::FileInfo) -> (kind r) `elem` [DirectoryK])

listFiles :: Data md => md -> [FilePath] 
listFiles = map fullpath . listify (\(r::FileInfo) -> (kind r) `elem` [AsciiK, BinaryK])

findFiles :: Data md => md -> (FileInfo -> Bool) -> [FilePath]
findFiles md pred = map fullpath $ listify (pred) md

listNonEmptyFiles :: Data md => md -> [FilePath] 
listNonEmptyFiles = L.filter (\s->s/= "") . listFiles

listPaths :: Data md => md -> [FilePath] 
listPaths = map fullpath . listify (\(_::FileInfo) -> True)

listNonEmptyPaths :: Data md => md -> [FilePath] 
listNonEmptyPaths = map fullpath . listify (\(r::FileInfo) -> (fullpath r) /= "")

listInfoEmptyFiles :: Data a => a -> [FileInfo]
listInfoEmptyFiles = listify $ \(r::FileInfo) -> (fullpath r) == ""

listInfoNonEmptyFiles :: Data a => a -> [FileInfo]
listInfoNonEmptyFiles = listify $ \(r::FileInfo) -> (fullpath r) /= ""

listMDNonEmptyFiles :: (Data a) => a -> [Forest_md]
listMDNonEmptyFiles = listify aux where
	aux :: Forest_md -> Bool
	aux r = (fullpath $ fileInfo r) /= ""

mapInfoFiles :: Data a => a -> (Map FilePath FileInfo)
mapInfoFiles md =
	let fileInfos = listInfoNonEmptyFiles md
	    keyedInfos = map (\finfo -> (fullpath finfo, finfo)) fileInfos
	in Data.Map.fromList keyedInfos


