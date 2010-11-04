{-# LANGUAGE FlexibleInstances #-}
module Language.Forest.Infer where
import Language.Forest.Forestc
import Language.Forest.Syntax

import Data.List
import Data.Set
import Data.Char
import System.FilePath.Posix
import Data.Data

buildDesc :: (ForestMD md) => md -> [ForestDecl]
buildDesc md = 
  let allpaths = Data.List.nub(listMDNonEmptyFiles md)
      (dirs,files) = Data.List.partition (\fmd -> (kind . fileInfo) fmd == DirectoryK) allpaths
      fileTyList = Prelude.map (\fmd -> ((fullpath . fileInfo) fmd,
                              case (kind . fileInfo) fmd of 
                                 AsciiK    -> File ("Ptext",  Nothing)
                                 otherwise -> File ("Pbinary", Nothing))) files
      forestTySet = Data.Set.fromList fileTyList                             
      sortedDirs = Data.List.sortBy (\d1 d2 -> compare (length (get_fullpath d2)) (length (get_fullpath d1)) ) dirs    -- Subdirectories appear before parent directories
  in
    []

collapseDirectory :: FilePath -> Data.Set.Set (FilePath, ForestTy) -> Data.Set.Set (FilePath, ForestDecl) -> 
                                 (Data.Set.Set (FilePath, ForestTy), Data.Set.Set (FilePath, ForestDecl))
collapseDirectory dir forestTys forestDecls = 
  let (nestedFiles, otherFiles)  = getNested dir forestTys
      (nestedDirs,  otherDirs )  = getNested dir forestDecls
  in undefined      

buildDirectory dir nestedFiles nestedDirs = 

buildSimpleField (fp, fTy) = 
  let externalName = takeFileName fp
      internalName = if isHaskellID externaName then externalName
                      

isHaskellIDchar c = isAlphaNum c ||  c == '_' || c == '\''
isHaskellID (f:str) = (isAsciiLower f) && (and (Prelude.map isHaskellIDchar str))

getHaskellId name n = 
 case filter isHaskellIDchar name of
     []      -> ("field_" ++ (show n))
     c:name' -> (toLower c) : name'


getNested dir set = Data.Set.partition (\(filepath,forestTy) -> dir == takeDirectory filepath) set

instance Ord ForestTy where
 compare f1 f2 = EQ

instance Ord ForestDecl where
 compare f1 f2 = EQ