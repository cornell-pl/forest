{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}

module Language.Forest.Generic   where

import Data.Data
import Data.Generics
import Language.Pads.Generic
import Language.Forest.MetaData
import Data.Map hiding (map)
import Data.Set hiding (map)

class (Data rep, ForestMD md) => Forest rep md | rep -> md  where
 load :: FilePath -> IO(rep, md)
 fdef :: rep
 fdef = myempty

class (Data rep, ForestMD md) => Forest1 arg rep md | rep -> md, rep->arg  where
 load1 :: arg -> FilePath -> IO(rep, md)
 fdef1 :: arg -> rep
 fdef1 = \s->myempty

listFiles :: (ForestMD md) => md -> [FilePath] 
listFiles md = map fullpath (listify (\(r::FileInfo) -> (kind r) `elem` [AsciiK, BinaryK]) md)

listPaths :: (ForestMD md) => md -> [FilePath] 
listPaths md = map fullpath (listify (\(_::FileInfo) -> True) md)

listEmptyFiles md = listify (\(r::FileInfo) -> (fullpath r) == "") md

class BuildContainer2 c item where
  buildContainer2 :: [(FilePath,item)] -> c FilePath item

instance BuildContainer2 Map a  where
  buildContainer2 = Data.Map.fromList

class BuildContainer1 c item where
  buildContainer1 :: [(FilePath,item)] -> c (FilePath, item)

instance Ord a => BuildContainer1 Set a  where
  buildContainer1 = Data.Set.fromList


{-
instance BuildContainer Set item where
  buildContainer = Data.Set.fromList
-}

  
