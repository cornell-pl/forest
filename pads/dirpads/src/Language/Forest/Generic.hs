{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}

module Language.Forest.Generic   where

import Data.Data
import Data.Generics
import Language.Pads.Generic
import Language.Forest.MetaData
import Data.Map hiding (map)
import Data.Set hiding (map)
import qualified Data.List as L

class (Data rep, ForestMD md) => Forest rep md | rep -> md  where
 load :: FilePath -> IO(rep, md)
 fdef :: rep
 fdef = myempty

class (Data rep, ForestMD md) => Forest1 arg rep md | rep -> md, rep->arg  where
 load1 :: arg -> FilePath -> IO(rep, md)
 fdef1 :: arg -> rep
 fdef1 = \s->myempty

listDirs :: (ForestMD md) => md -> [FilePath] 
listDirs md = map fullpath (listify (\(r::FileInfo) -> (kind r) `elem` [DirectoryK]) md)

listFiles :: (ForestMD md) => md -> [FilePath] 
listFiles md = map fullpath (listify (\(r::FileInfo) -> (kind r) `elem` [AsciiK, BinaryK]) md)

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


