module Language.Forest.ForestIO where

import Language.Pads.Padsc
import Language.Forest.MetaData
import Language.Forest.Generic
import Language.Forest.Errors
import System.FilePath.Posix
import System.Posix.Files
import System.Cmd
import System.Exit
import System.Directory
import System.IO
import Text.Regex

import qualified Control.Exception as CE

import Data.Data
import Data.Maybe


fileload :: Pads pads md => FilePath -> IO (pads, (Forest_md, md))
fileload path = do
   fmd       <- getForestMD path
   (rep, md) <- parseFile path
   return (rep, (fmd, md))

fileload1 :: Pads1 arg pads md => arg -> FilePath -> IO (pads, (Forest_md, md))
fileload1 arg path = do
   fmd       <- getForestMD path
   (rep, md) <- parseFile1 arg path
   return (rep, (fmd, md))



getTempFile :: IO FilePath
getTempFile = do 
 { fp <- getTemporaryDirectory
 ; (fp, handle) <- openTempFile fp "Forest"
 ; hClose handle
 ; system ("rm -f " ++ fp)
 ; return fp
 }

untar :: FilePath -> IO(ExitCode, FilePath, FilePath)  
untar path = do 
 { tempDir <- getTempFile
 ; createDirectory tempDir
 ; oldCurDir <-  getCurrentDirectory
 ; setCurrentDirectory tempDir
 ; let cmd = "tar -xf " ++ path
-- ; putStrLn cmd
 ; exitCode <- system cmd
 ; return (exitCode, tempDir, oldCurDir)
 }


gunzip :: FilePath -> IO(ExitCode, FilePath)  
gunzip path = do
  { tempFile <- getTempFile
  ; let cmd = "gunzip " ++ path ++ " -c > " ++ tempFile
  ; exitCode <- system cmd
  ; return (exitCode,tempFile)
  }

remove :: FilePath -> IO()
remove path = do
 { let cmd = "rm -rf " ++ path   
 ; exitCode <- system cmd
 ; return ()
 }

tarload   :: (ForestMD md, Data rep) =>  (FilePath -> IO (rep, md)) -> FilePath -> IO (rep, md)
tarload load path = checkPath path (do
  { md <- getForestMD path
  ; (exitCode, tempDir, savedCurDir) <- untar path
  ; (rep,md) <- case exitCode of
                 ExitSuccess -> do
                  { (rep, md_zip) <- load tempDir
                  ; let md' = replace_fmd_header md_zip md
                  ; return (rep, md')
                  }
                 ExitFailure errCode -> do
                  { let newMD = updateForestMDwith md [systemErrorForestMD errCode]
                  ; let md' = replace_fmd_header myempty newMD      -- This won't work until we get generic default working on maps, etc.
                  ; return (myempty, md')
                  }
  ; setCurrentDirectory savedCurDir
  ; remove tempDir
  ; return (rep, md)
  })



gzipload'  :: (ForestMD md, Data rep) =>  (FilePath -> IO (rep, md)) -> FilePath -> IO (rep, md)
gzipload' load path = checkPath path (do 
  { md <- getForestMD path
  ; (exitCode, newpath) <- gunzip path
  ; case exitCode of
      ExitSuccess -> do
        { (rep, md_zip) <- load newpath
        ; let md' = replace_fmd_header md_zip md
        ; remove newpath
        ; return (rep, md')
        }
      ExitFailure errCode -> do
        { let newMD = updateForestMDwith md [systemErrorForestMD errCode]
        ; let md' = replace_fmd_header gdef newMD                      -- This won't work until we get generic default working on maps, etc.
        ; remove newpath
        ; return (gdef, md')
        }
  })

doLoadSymLink :: FilePath -> IO(FilePath, (Forest_md, Base_md))
doLoadSymLink path = checkPath path (do
    md <- getForestMD path
    fpEither <- CE.try (readSymbolicLink path)
    case fpEither of
        Left (e::CE.SomeException) -> return
                ("", 
                 (updateForestMDwith md 
                   [Forest_md { Language.Forest.MetaData.numErrors = 1
                              , errorMsg = Just (ForestIOException (show e))
                              , fileInfo = errorFileInfo
                              }],
                  cleanBasePD))
        Right fp -> return
                (fp, (md,cleanBasePD))
  )



doLoadMaybe :: ForestMD md =>  IO (rep,md) -> IO (Maybe rep, (Forest_md, Maybe md))
doLoadMaybe f = do
   (r,fmd) <- f 
   let bfmd = get_fmd_header fmd
   if Language.Forest.MetaData.numErrors bfmd == 0 
       then return (Just r, (bfmd, Just fmd))
       else return (Nothing, (cleanForestMD, Nothing))

checkPath :: (Data rep, ForestMD md) => FilePath -> IO(rep,md) -> IO(rep,md)
checkPath path ifExists = do 
   { exists <-  fileExist path
   ; if not exists then 
       do { def_md <- myempty
          ; let new_md = replace_fmd_header def_md (missingPathForestMD path)
          ; return (myempty, new_md)
          }
     else ifExists
   }

getMatchingFiles :: FilePath -> RE -> IO [FilePath]
getMatchingFiles path re = do 
  { files <- getDirectoryContents path
  ; return (filterByRegex re files)
  }

filterByRegex (RE regStr) candidates = 
  let re = mkRegexWithOpts ('^':regStr++"$") True True
      matchOne str = isJust (matchRegex re str)
  in Prelude.filter matchOne candidates

