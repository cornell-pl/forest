{-# LANGUAGE ScopedTypeVariables #-}
module Language.Forest.Shell where

import Language.Forest.Forestc
import System.Directory
import System.IO
import System.IO.Unsafe
import System.Cmd
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

echo :: (ForestMD md) => md -> String -> IO String
echo md options = execShellCmdWithXArgs md ("ls " ++ options)

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

{-
tar :: (ForestMD md) => md -> String -> IO ()
tar md name = do 
 { let files = filter (\s->s /= "") (listFiles md)
 ; td <- getTemporaryDirectory
 ; (fp, handle) <- openTempFile td "ForestTarSpec"
 ; mapM_ (hPutStrLn handle) files
 ; hClose handle
 ; let cmd = "tar -T " ++ fp ++ " -cf " ++ name
 ; system cmd
 ; removeFile fp
 }
-}

-- >ftar Universal.universal_d universe.tar
-- >Universal-tar universal_d universe.tar

getLoadArgs s = 
  let (ty_name, arg) = Data.List.break (\c->c=='(') s
  in case arg of 
       [] -> (ty_name, Nothing)
       '(':str -> if Data.List.last str == ')' then (ty_name, Just (init str)) 
                  else error ("Argument to shell tool should be enclosed in parens.  Instead found:"++arg)

{-
StudentShell tar PrincetonCS_d CS.tar 
-}
