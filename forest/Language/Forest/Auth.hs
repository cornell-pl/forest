{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

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


module Language.Forest.Auth where

import Data.IORef
import Data.WithClass.MData
import Language.Forest.FS.FSRep
import Language.Forest.Class
import qualified Data.Map as Map
import Data.Map (Map(..))
import qualified Data.Set  as S
import qualified Data.List as L

import System.FilePath.Posix
import System.IO.Unsafe (unsafePerformIO)

restrictingPaths :: [(FilePath, [FilePath])] -> [FilePath]
restrictingPaths files = 
  let causes = foldr (\(f,files) accum -> (S.fromList files) `S.union` accum) S.empty files
  in S.toList causes



canRead :: (MData NoCtx (l (IncForest fs) IORef IO) md,ForestLayer fs l,ForestMD fs md) => md -> FilePath -> String -> ForestL fs l (Either String (Bool, [FilePath]))
canRead md fileName id = mapInfoFiles md >>= \mapInfo -> return $ canDoOpFromMap mapInfo Read id fileName 

canWrite :: (MData NoCtx (l (IncForest fs) IORef IO) md,ForestLayer fs l,ForestMD fs md) => md -> FilePath -> String -> ForestL fs l (Either String (Bool, [FilePath]))
canWrite md fileName id = mapInfoFiles md >>= \mapInfo -> return $ canDoOpFromMap mapInfo Write id fileName 

canExec :: (MData NoCtx (l (IncForest fs) IORef IO) md,ForestLayer fs l,ForestMD fs md) => md -> FilePath -> String -> ForestL fs l (Either String (Bool, [FilePath]))
canExec md fileName id = mapInfoFiles md >>= \mapInfo -> return $ canDoOpFromMap mapInfo Execute id fileName 

readProhibited :: (MData NoCtx (l (IncForest fs) IORef IO) md,ForestLayer fs l,ForestMD fs md) => md -> String -> ForestL fs l [(FilePath, [FilePath])]
readProhibited md id = opProhibited md Read id

writeProhibited :: (MData NoCtx (l (IncForest fs) IORef IO) md,ForestLayer fs l,ForestMD fs md) => md -> String -> ForestL fs l [(FilePath, [FilePath])]
writeProhibited md id = opProhibited md Write id

execProhibited :: (MData NoCtx (l (IncForest fs) IORef IO) md,ForestLayer fs l,ForestMD fs md) => md -> String -> ForestL fs l [(FilePath, [FilePath])]
execProhibited md id = opProhibited md Execute id

{- Collecting operations on files. -}
opProhibited :: (MData NoCtx (l (IncForest fs) IORef IO) md,ForestLayer fs l,ForestMD fs md) => md -> Permission -> String -> ForestL fs l [(FilePath, [FilePath])]
opProhibited md op id = do
	mapInfo <- mapInfoFiles md
	let files = Map.keys mapInfo
	groups <- forestIO $ getGroups id
	let allResults = L.map (\f -> (f,canDoOpFromMap' mapInfo op id groups ((Map.!) mapInfo f) f )) files
	let badFiles = L.filter (\(f,(b,files)) -> not b) allResults
	let formatedFiles = L.map (\(f,(b,files)) -> (f,files)) badFiles
	return formatedFiles


{- Supplied fileName should be relative to the same starting point given to the load function
   that produced the supplied meta data.
-}
canDoOp :: (MData NoCtx (l (IncForest fs) IORef IO) md,ForestLayer fs l,ForestMD fs md) => md -> Permission -> FilePath -> String -> ForestL fs l (Either String (Bool, [FilePath]))
canDoOp md op fileName id = mapInfoFiles md >>= \mapInfo -> return $ canDoOpFromMap mapInfo op id fileName 


canDoOpFromMap :: Map FilePath FileInfo -> Permission -> String -> FilePath -> Either String (Bool, [FilePath])
canDoOpFromMap infoMap op id filePath = 
  case Map.lookup filePath infoMap of
    Nothing -> Left ("File " ++ filePath ++ " does not exist in argument directory structure")
    Just fInfo -> let groups = unsafePerformIO $ getGroups id
                  in Right (canDoOpFromMap' infoMap op id groups fInfo filePath)

canDoOpFromMap' :: Map FilePath FileInfo -> Permission -> String -> [String] -> FileInfo -> FilePath -> (Bool, [FilePath])
canDoOpFromMap' infoMap op id groups fInfo filePath = 
  let canDo = op `elem` (getPermissions id groups fInfo)
      problemFiles = if canDo then [] else [filePath]
      parentPath = takeDirectory filePath
  in canDoOpOnPath infoMap op id groups parentPath canDo problemFiles

canDoOpOnPath :: Map FilePath FileInfo -> Permission -> String -> [String] -> FilePath -> Bool -> [FilePath] -> (Bool,[FilePath])
canDoOpOnPath infoMap op id groups filePath canDo problemFiles = 
  case Map.lookup filePath infoMap of
    Nothing -> (canDo, problemFiles)       -- Have reached the top of the directory tree
    Just fInfo -> let fpPermissions = getPermissions id groups fInfo
                      parentPath = takeDirectory filePath
                      newCanDo = op `elem` fpPermissions
                      newProblemFiles = if newCanDo then [] else [filePath]
                  in  canDoOpOnPath infoMap op id groups parentPath (canDo && newCanDo) (newProblemFiles ++ problemFiles)




{- Supplied fileName should be relative to the same starting point given to the load function
   that produced the supplied meta data.
-}
checkAuth :: (MData NoCtx (l (IncForest fs) IORef IO) md,ForestLayer fs l,ForestMD fs md) => md -> FilePath -> String -> ForestL fs l (Either String ([Permission], [Permission]))
checkAuth md fileName id = mapInfoFiles md >>= \mapInfo -> return $ checkAuthFromMap mapInfo fileName id


checkAuthFromMap :: Map FilePath FileInfo -> FilePath -> String -> Either String ([Permission],[Permission])
checkAuthFromMap infoMap filePath id = 
  case Map.lookup filePath infoMap of
    Nothing -> Left ("File " ++ filePath ++ " does not exist in argument directory structure")
    Just fInfo -> let groups = unsafePerformIO $ getGroups id
                      filePermissions = getPermissions id groups fInfo
                      parentPath = takeDirectory filePath
                  in Right (checkAuthOnPath infoMap parentPath id groups filePermissions [Read,Write,Execute])

checkAuthOnPath :: Map FilePath FileInfo -> FilePath -> String -> [String] -> [Permission] -> [Permission] -> ([Permission],[Permission])
checkAuthOnPath infoMap filePath id groups filePermissions dirPermissions = 
  case Map.lookup filePath infoMap of
    Nothing -> (dirPermissions, filePermissions)       -- Have reached the top of the directory tree
    Just fInfo -> let fpPermissions = getPermissions id groups fInfo
                      newDirPermissions = L.intersect fpPermissions dirPermissions
                      parentPath = takeDirectory filePath
                  in  checkAuthOnPath infoMap parentPath id groups filePermissions newDirPermissions

