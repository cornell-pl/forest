{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NamedFieldPuns #-}
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


module Language.Forest.Generic   where

import Data.Data
import Data.Generics
import Language.Pads.Padsc
import Language.Forest.MetaData
import Language.Forest.Writing
import System.FilePath.Posix
import Data.Map hiding (map)
import Data.Set hiding (map)
import qualified Data.List as L

class (Data rep, ForestMD md) => Forest rep md | rep -> md  where
	load :: FilePath -> IO (rep, md)
	generateManifest :: (rep,md) -> IO Manifest
	updateManifest :: (rep,md) -> Manifest -> IO Manifest
	fdef :: rep
	fdef = myempty
	defaultMd :: rep -> FilePath -> md

 
class (Data rep, ForestMD md) => Forest1 arg rep md | rep -> md, rep -> arg  where
	load1 :: arg -> FilePath -> IO (rep, md)
	generateManifest1 :: arg -> (rep,md) -> IO Manifest
	fdef1 :: arg -> rep
	fdef1 = \s -> myempty
	updateManifest1 :: arg -> (rep,md) -> Manifest -> IO Manifest
	defaultMd1 :: arg -> rep -> FilePath -> md

class File rep md where
	fileLoad :: FilePath -> IO (rep, (Forest_md, md))

class File1 arg rep md where
	fileLoad1 :: arg -> FilePath -> IO (rep, (Forest_md, md))

store :: Forest rep md => (rep,md) -> IO ()
store repmd = generateManifest repmd >>= storeManifest

storeAt :: Forest rep md => (rep,md) -> FilePath -> IO ()
storeAt repmd path = generateManifest repmd >>= storeManifestAt path

store1 :: Forest1 arg rep md => arg -> (rep,md) -> IO ()
store1 arg repmd = generateManifest1 arg repmd >>= storeManifest

storeAt1 :: Forest1 arg rep md => arg -> (rep,md) -> FilePath -> IO ()
storeAt1 arg repmd path = generateManifest1 arg repmd >>= storeManifestAt path

validateLists
  :: (FilePath -> IO a)
     -> (a -> Manifest -> IO Manifest)
     -> Manifest
     -> IO Manifest
validateLists  load updateMan (m @ Manifest {tempDir, pathToRoot, entries, count}) = do
  { listValidDir <- getTempForestListDirectory
  ; storeManifestAt listValidDir m
  ; let fileName = case pathToRoot of { Nothing -> error "pathToRoot should not be null." ; Just p -> takeFileName p}
  ; let pathToDir = combine listValidDir fileName
  ; debugIO ("loading from: "++pathToDir)
  ; testResult <- load pathToDir
  ; emptyManifest <- newManifest
  ; testManifest <- updateMan testResult emptyManifest
  ; return (detectConflictsInListComps m testManifest)
  }

checkStore
  :: (FilePath -> IO a)
     -> (a -> Manifest -> IO Manifest)
     -> FilePath
     -> Manifest
     -> IO [(FilePath, [FilePath])]
checkStore load updateMan destDir writeManifest = do
  { res <- load destDir
  ; emptyManifest <- newManifest
  ; targetManifest <- updateMan res emptyManifest
  ; return (diffManifest targetManifest writeManifest)
  }




rawManifest1 :: Forest1 arg rep md => arg -> (rep, md) -> IO Manifest
rawManifest1 arg forest = do
  { origManifest <- newManifest
  ; updateManifest1 arg forest origManifest
  }




listDirs :: (ForestMD md) => md -> [FilePath] 
listDirs md = map fullpath (listify (\(r::FileInfo) -> (kind r) `elem` [DirectoryK]) md)

listFiles :: (ForestMD md) => md -> [FilePath] 
listFiles md = map fullpath (listify (\(r::FileInfo) -> (kind r) `elem` [AsciiK, BinaryK]) md)

findFiles :: (ForestMD md) => md -> (FileInfo -> Bool) -> [FilePath]
findFiles md pred = map fullpath (listify pred md)

listNonEmptyFiles :: (ForestMD md) => md -> [FilePath] 
listNonEmptyFiles md = L.filter (\s->s/= "") (listFiles md)

listPaths :: (ForestMD md) => md -> [FilePath] 
listPaths md = map fullpath (listify (\(_::FileInfo) -> True) md)

listNonEmptyPaths :: (ForestMD md) => md -> [FilePath] 
listNonEmptyPaths md = map fullpath (listify (\(r::FileInfo) -> (fullpath r) /= "") md)

listInfoEmptyFiles :: Data.Data.Data a => a -> [FileInfo]
listInfoEmptyFiles md = listify (\(r::FileInfo) -> (fullpath r) == "") md

listInfoNonEmptyFiles :: Data.Data.Data a => a -> [FileInfo]
listInfoNonEmptyFiles md = listify (\(r::FileInfo) -> (fullpath r) /= "") md

listMDNonEmptyFiles :: Data.Data.Data a => a -> [Forest_md]
listMDNonEmptyFiles md = listify (\(r::Forest_md) -> (fullpath (fileInfo r)) /= "") md


mapInfoFiles :: Data.Data.Data a => a -> Map FilePath FileInfo
mapInfoFiles md = 
  let fileInfos = listInfoNonEmptyFiles md
      keyedInfos = map (\finfo -> (fullpath finfo, finfo)) fileInfos
  in  Data.Map.fromList keyedInfos


