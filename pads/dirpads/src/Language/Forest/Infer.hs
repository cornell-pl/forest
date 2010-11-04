{-# LANGUAGE FlexibleInstances #-}
module Language.Forest.Infer where
import Language.Forest.Forestc
import Language.Forest.Syntax
import Language.Haskell.TH

import Data.List
import Data.Char
import System.FilePath.Posix
import Data.Data

buildDesc :: (ForestMD md) => md -> [ForestDecl]
buildDesc md = 
  let allpaths = Data.List.nub(listMDNonEmptyFiles md)
      (dirs,files) = Data.List.partition (\fmd -> (kind . fileInfo) fmd == DirectoryK) allpaths
      forestTyList = Prelude.map getFileTy files
      dirNames = Data.List.map get_fullpath dirs
      sortedDirs = Data.List.sortBy (\d1 d2 -> compare (length d1) (length d2) ) dirNames    -- Subdirectories appear before parent directories
      (finalTys, finalDecls) = Prelude.foldr collapseDirectory (forestTyList, []) sortedDirs
  in
    Data.List.nub (Data.List.reverse finalDecls)

collapseDirectory :: FilePath -> ([(FilePath, ForestTy)], [ForestDecl]) -> ([(FilePath, ForestTy)], [ForestDecl])
collapseDirectory dir (forestTys, forestDecls) = 
  let (nestedFiles, otherFiles)  = getNested dir forestTys
      (dirTy, dirDecl) = buildDirectory dir nestedFiles
  in ( (dir,dirTy) : forestTys,  dirDecl : forestDecls)

buildDirectory dir nestedComponents  = 
  let compList = Prelude.map (buildSimpleField 1) nestedComponents   -- make monadic to pass this state around
      (dirName,n) = (getHaskellId 2 (takeFileName dir))
      forestTy = Directory (Record dirName compList)
      forestDecl = ForestDecl(dirName, Nothing, forestTy)
      refTy = Named dirName
  in (refTy, forestDecl)

buildSimpleField n (fp, fTy)  = 
  let externalName = takeFileName fp
      (internalName,n') = getHaskellId n externalName 
      externalE = mkStrLitM externalName
  in Simple (internalName, True, externalE, fTy, Nothing)
       
getNested dir set = Data.List.partition (\(filepath,forestTy) -> dir == takeDirectory filepath) set
                      

isHaskellIDchar c = isAlphaNum c ||  c == '_' || c == '\''
isHaskellID (f:str) = (isAsciiLower f) && (and (Prelude.map isHaskellIDchar str))

-- this needs to be fixed to always generate a unique Id
getHaskellId n name  = 
 case Data.List.filter isHaskellIDchar name of
     []      -> (("id_" ++ (show n)), n+1)
     c:name' -> ((toLower c) : name', n+1)



getFileTy fmd = ((fullpath . fileInfo) fmd,
                  case (kind . fileInfo) fmd of 
                       AsciiK    -> File ("Ptext",  Nothing)
                       otherwise -> File ("Pbinary", Nothing))


mkStrLitM s = LitE (StringL s)