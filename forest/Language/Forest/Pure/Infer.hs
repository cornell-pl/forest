{-# LANGUAGE FlexibleInstances #-}
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

module Language.Forest.Pure.Infer where

import Language.Forest.Syntax
import Language.Haskell.TH

import Data.List
import Data.Char
import System.FilePath.Posix
import Data.Data
import Language.Forest.IO.Utils
import Language.Forest.Pure.MetaData
import Language.Forest.Pure.Generic
import Language.Forest.Pure.CodeGen.Utils



buildDesc :: (Data md,ForestMD md) => md -> [ForestDecl]
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
      forestDecl = ForestDecl(dirName, [], forestTy)
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
                       AsciiK    -> File ("Text",  Nothing)
                       otherwise -> File ("Binary", Nothing))

