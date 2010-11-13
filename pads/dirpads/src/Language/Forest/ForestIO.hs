{- XXX  add error checking to all calls to system; wrap with failure -}

{-# LANGUAGE ScopedTypeVariables, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances #-}
module Language.Forest.ForestIO where

import Language.Pads.Padsc
import Language.Forest.MetaData
import Language.Forest.Generic
import Language.Forest.Errors

import System.FilePath.Posix
import System.FilePath.Glob
import System.Posix.Files
import System.Cmd
import System.Exit
import System.Directory
import System.IO
import System.IO.Unsafe
import Text.Regex

-- import Data.Time.Clock (getCurrentTime :: IO UTCTime)
-- import Data.Time.Clock.Posix (getPOSIXTime:: IO POSIXTime)

-- EpochTime = CTime
-- Defined in Data.Time.Clock.POSIX
-- posixSecondsToUTCTime (realToFrac :: POSIXTime)



import qualified Control.Exception as CE

import Data.Data
import Data.Maybe



fileload :: Pads pads md => FilePath -> IO (pads, (Forest_md, md))
fileload path = checkPath path (do
   fmd        <- unsafeInterleaveIO (getForestMD path)
   ~(rep, md) <- unsafeInterleaveIO (parseFile path)
   return (rep, (fmd, md)))

fileload1 :: Pads1 arg pads md => arg -> FilePath -> IO (pads, (Forest_md, md))
fileload1 arg path = checkPath path (do
   fmd       <- unsafeInterleaveIO (getForestMD path)
   ~(rep, md) <- unsafeInterleaveIO (parseFile1 arg path)
   return (rep, (fmd, md)))

instance (Pads rep md) => File rep md where
  fileLoad = fileload

instance (Pads1 arg rep md) => File1 arg rep md where
  fileLoad1 = fileload1


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



gzipload  :: (ForestMD md, Data rep) =>  (FilePath -> IO (rep, md)) -> FilePath -> IO (rep, md)
gzipload load path = checkPath path (do 
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
        ; let md' = replace_fmd_header myempty newMD                      
        ; remove newpath
        ; return (myempty, md')
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


doLoadConstraint :: (Data rep, ForestMD md) => IO(rep,md) -> FilePath -> ((rep,md) -> Bool) -> IO(rep,md)
doLoadConstraint load path pred = checkPath path (do
 { result @ ~(r,fmd) <- load
 ; let isGood = pred result
 ; if isGood 
      then return result
      else let bfmd = get_fmd_header fmd
               newbfmd = updateForestMDwith bfmd [constraintViolation]
           in return (r, replace_fmd_header fmd newbfmd)
 })

doLoadMaybe :: ForestMD md =>  FilePath -> IO (rep,md) -> IO (Maybe rep, (Forest_md, Maybe md))
doLoadMaybe path f = do
   { exists <- fileExist path
   ; if not exists then
       return (Nothing, (cleanForestMD, Nothing))
     else do
      { (r,fmd) <- f 
      ; let bfmd = get_fmd_header fmd
      ; if Language.Forest.MetaData.numErrors bfmd == 0 
         then return (Just r, (bfmd, Just fmd))
         else return (Nothing, (cleanForestMD, Nothing))
      }
   }


pickFile :: [FilePath] -> FilePath
pickFile files = case files of
  [] -> "-This is an illegal file path according to POSIX Standard."  -- an illegal file path
  f:fs -> f

checkNonEmpty :: (Data rep, ForestMD md) => FilePath -> FilePath -> IO(rep,md) -> IO(rep,md)
checkNonEmpty path file ifExists = 
  if file == "" then 
       do { let def_md = myempty
          ; let new_md = replace_fmd_header def_md (fileMatchFailureForestMD path)
          ; return (myempty, new_md)
          }
  else ifExists

checkPath :: (Data rep, ForestMD md) => FilePath -> IO(rep,md) -> IO(rep,md)
checkPath path ifExists = do 
   { exists <-  fileExist path
   ; if not exists then 
       do { let def_md = myempty
          ; let new_md = replace_fmd_header def_md (missingPathForestMD path)
          ; return (myempty, new_md)
          }
     else ifExists
   }

checkPathIsDir path ifGood = do
  { isGood <- doesDirectoryExist path
  ; if isGood then ifGood
    else do 
      { let def_md = myempty
      ; let new_md = replace_fmd_header def_md (missingPathForestMD path)
      ; return (myempty, new_md)
      }
  }

checkIsDir :: (Data rep, ForestMD md) => Forest_md -> IO(rep,md) -> IO(rep,md)
checkIsDir fmd ifDir = 
  if kind (fileInfo fmd) /= DirectoryK then (nonDir fmd) else ifDir

nonDir :: (Data rep, ForestMD md) => Forest_md -> IO(rep,md) 
nonDir fmd = do 
  { let def_md = myempty
  ; let new_md = replace_fmd_header def_md (notDirectoryForestMD (fullpath(fileInfo fmd)) )
  ; return (myempty, new_md)
  }



data GL = GL String

class Matching a where
 getMatchingFiles           :: FilePath -> a -> IO [FilePath]
 getMatchingFilesWithFilter :: FilePath -> (FilePath -> Bool) -> a -> IO [FilePath]

instance Matching RE where
 getMatchingFiles           = getMatchingFilesRE
 getMatchingFilesWithFilter = getMatchingFilesWithFilterRE

instance Matching GL where
 getMatchingFiles = getMatchingFilesGlob
 getMatchingFilesWithFilter = getMatchingFilesWithFilterGlob

getMatchingFilesRE :: FilePath -> RE -> IO [FilePath]
getMatchingFilesRE path re = do 
  { files <- getDirectoryContents path
  ; let matches = (filterByRegex re files)
  ; return matches
  }

getMatchingFilesWithFilterRE :: FilePath -> (FilePath -> Bool) -> RE -> IO [FilePath]
getMatchingFilesWithFilterRE path afilter re = do 
  { files <- getDirectoryContents path
  ; let matches = (filterByRegex re files)
  ; return (Prelude.filter afilter matches)
  }

filterByRegex (RE regStr) candidates = 
  let re = mkRegexWithOpts ('^':regStr++"$") True True
      matchOne str = isJust (matchRegex re str)
  in Prelude.filter matchOne candidates

getMatchingFilesGlob :: FilePath -> GL -> IO [FilePath]
getMatchingFilesGlob path (GL glob) = do 
  { let gl = compile glob
  ; files <- getDirectoryContents path
  ; let matches = (Prelude.filter (match gl) files)
  ; return matches
  }

getMatchingFilesWithFilterGlob :: FilePath -> (FilePath -> Bool) -> GL -> IO [FilePath]
getMatchingFilesWithFilterGlob path afilter (GL glob) = do 
  { let gl = compile glob
  ; files <- getDirectoryContents path
  ; let matches = Prelude.filter (compFilter (match gl) afilter) files
  ; return matches
  }

compFilter f1 f2 item =  f1 item && f2 item 


getMatchingFilesGlob' :: FilePath -> GL -> IO [FilePath]
getMatchingFilesGlob' path (GL glob) = do 
  { let gl = compile glob
  ; ([matches], unmatches) <- globDir [gl] path
  ; return matches
  }


concatPath stem new = 
  case new of
   [] -> stem
   ('/':rest) -> new
   otherwise -> stem ++ "/" ++ new
