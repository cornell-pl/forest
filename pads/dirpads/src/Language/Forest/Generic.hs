{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, FlexibleContexts #-}

module Language.Forest.Generic   where

import Data.Data
import Data.Generics
import Language.Forest.MetaData

class (Data rep, ForestMD md) => Forest rep md | rep -> md  where
 load :: FilePath -> IO(rep, md)
 fdef :: rep

class (Data rep, ForestMD md) => Forest1 arg rep md | rep -> md, rep->arg  where
 load1 :: arg -> FilePath -> IO(rep, md)
 fdef1 :: arg -> rep

listFiles :: (ForestMD md) => md -> [FilePath] 
listFiles md = map fullpath (listify (\(r::FileInfo) -> (kind r) == SimpleK) md)

listPaths :: (ForestMD md) => md -> [FilePath] 
listPaths md = map fullpath (listify (\(_::FileInfo) -> True) md)

listEmptyFiles md = listify (\(r::FileInfo) -> (fullpath r) == "") md
  
