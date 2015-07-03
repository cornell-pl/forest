{-# LANGUAGE ScopedTypeVariables #-}
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

module Language.Forest.Shell where

import Language.Forest.Forestc
import System.Directory
import System.IO
import System.IO.Unsafe
import System.Process
import System.FilePath.Posix
import Data.List

execShellCmdWithManifest md cmdf = do 
 { let files = listNonEmptyFiles md
 ; td <- getTemporaryDirectory
 ; (fp, handle) <- openTempFile td "ForestTempFile"
 ; mapM_ (hPutStrLn handle) files
 ; hClose handle
 ; let cmd = cmdf fp
 ; system cmd
 ; removeFile fp
 }

execShellCmdWithXArgs' :: [FilePath] -> String -> IO String
execShellCmdWithXArgs' files cmd = do
 { let delimFiles = intersperse ['\0'] files
 ; td <- getTemporaryDirectory
 ; (fp, handle) <- openTempFile td "ForestTempFile"
 ; mapM_ (hPutStr handle) delimFiles
 ; hClose handle
 ; (result_fp, result_handle) <- openTempFile td "ForestResultFile"
 ; let x_cmd =  "cat " ++ fp ++ " | xargs -0 " ++ cmd ++ "> "++ result_fp
 ; system x_cmd
 ; let result = unsafePerformIO (hGetContents result_handle)
-- ; hClose result_handle
 ; removeFile fp
 ; removeFile result_fp
 ; return result
 } 

execShellCmdWithXArgs :: (ForestMD md) => md -> String -> IO String
execShellCmdWithXArgs     md cmd = execShellCmdWithXArgs' (listNonEmptyFiles md) cmd

execShellCmdWithXArgsDirs :: (ForestMD md) => md -> String -> IO String
execShellCmdWithXArgsDirs md cmd = execShellCmdWithXArgs' (listDirs md) cmd

ls :: (ForestMD md) => md -> String -> IO String
ls md options = execShellCmdWithXArgs md ("ls " ++ options)

rm :: (ForestMD md) => md -> String -> IO String
rm md options = execShellCmdWithXArgs md ("rm " ++ options)

rmdir :: (ForestMD md) => md -> String -> IO String
rmdir md options = execShellCmdWithXArgs' (reverse (sort(listDirs md))) ("rmdir " ++ options)

grep :: (ForestMD md) => md -> String -> IO String
grep md options = execShellCmdWithXArgs md ("grep " ++ options)
  
tar :: (ForestMD md) => md -> FilePath -> IO ()
tar md name = execShellCmdWithManifest md (\fp -> "tar -T " ++ fp ++ " -cf " ++ name)

cp :: (ForestMD md) => md -> FilePath ->  IO ()
cp md target = do
 { td <- getTemporaryDirectory
 ; (fp, handle) <- openTempFile td "ForestCp.tar"
 ; let baseName = takeFileName fp
 ; let targetPath = target ++ "/" ++ baseName
 ; tar md fp
 ; system ("mv " ++ fp ++ " " ++ target)
 ; system ("cd " ++ target ++ "; tar -xvf " ++ targetPath)
 ; hClose handle
 ; removeFile targetPath
 }

getLoadArgs s = 
  let (ty_name, arg) = Data.List.break (\c->c=='(') s
  in case arg of 
       [] -> (ty_name, Nothing)
       '(':str -> if Data.List.last str == ')' then (ty_name, Just (init str)) 
                  else error ("Argument to shell tool should be enclosed in parens.  Instead found:"++arg)
