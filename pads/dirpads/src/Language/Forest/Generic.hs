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



  
