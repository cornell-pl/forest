{- XXX  add error checking to all calls to system; wrap with failure -}

{-# LANGUAGE ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}

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

module Language.Forest.ForestIO where

import Language.Pads.Padsc
import Language.Forest.MetaData
import Language.Forest.Generic
import Language.Forest.Errors
import Language.Forest.Delta
import Language.Forest.MetaData
import Language.Forest.ListDiff

import qualified System.FilePath.Posix
import System.FilePath.Glob
import System.Posix.Env
import System.Posix.Files
import System.Cmd
import System.Exit
import System.Directory
import System.IO
import System.IO.Unsafe
import Text.Regex
import System.FilePath.Posix
import Control.Monad
import Data.List

import Debug.Trace

-- import Data.Time.Clock (getCurrentTime :: IO UTCTime)
-- import Data.Time.Clock.Posix (getPOSIXTime:: IO POSIXTime)

-- EpochTime = CTime
-- Defined in Data.Time.Clock.POSIX
-- posixSecondsToUTCTime (realToFrac :: POSIXTime)



import qualified Control.Exception as CE

import Data.Data
import Data.Maybe
import System.Random

-- lazy file loading
fileload :: Pads pads md => FilePath -> IO (pads, (Forest_md, md))
fileload path = checkPath path (do
   fmd        <- unsafeInterleaveIO (getForestMD path)
   ~(rep, md) <- unsafeInterleaveIO (parseFile path)
   return (rep, (fmd, md)))
-- Isn't lazy I/O being used by default? Is unsafeInterleaveIO really necessary here?

-- | strict file loading
fileload' :: Pads pads md => FilePath -> IO (pads, (Forest_md, md))
fileload' path = checkPath path (do
   fmd        <- (getForestMD path)
   ~(rep, md) <- (parseFile path)
   return (rep, (fmd, md)))


fileload1 :: Pads1 arg pads md => arg -> FilePath -> IO (pads, (Forest_md, md))
fileload1 arg path = checkPath path (do
   fmd       <- unsafeInterleaveIO (getForestMD path)
   ~(rep, md) <- unsafeInterleaveIO (parseFile1 arg path)
   return (rep, (fmd, md)))


fileload1' :: Pads1 arg pads md => arg -> FilePath -> IO (pads, (Forest_md, md))
fileload1' arg path = checkPath path (do
   fmd       <-  (getForestMD path)
   ~(rep, md) <-  (parseFile1 arg path)
   return (rep, (fmd, md)))

instance (Pads rep md) => File rep md where
  fileLoad = fileload

instance (Pads1 arg rep md) => File1 arg rep md where
  fileLoad1 = fileload1



getTempForestScratchDirectory :: IO FilePath
getTempForestScratchDirectory = do 
  { tempDir <- getTempForestDirectory 
  ; let forestScratchDir = combine tempDir "Scratch"
  ; createDirectoryIfMissing False forestScratchDir
  ; return forestScratchDir
  }

getTempFile :: IO FilePath
getTempFile = do 
 { fp <- getTempForestScratchDirectory
 ; (fp', handle) <- openTempFile fp "Forest"
 ; hClose handle
 ; system ("rm -f " ++ fp')
 ; return fp'
 }

getTempDir :: IO FilePath
getTempDir = do 
 { fp <- getTempFile;
 ; createDirectoryIfMissing False fp
 ; return fp
 }

untar :: FilePath -> IO(ExitCode, FilePath, FilePath)  
untar path = do 
 { tempDir <- getTempDir
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
--  ; remove tempDir
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
--        ; remove newpath
        ; return (rep, md')
        }
      ExitFailure errCode -> do
        { let newMD = updateForestMDwith md [systemErrorForestMD errCode]
        ; let md' = replace_fmd_header myempty newMD                      
        ; remove newpath
        ; return (myempty, md')
        }
  })

doLoadSymLink :: FilePath -> IO (FilePath, (Forest_md, Base_md))
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

doLoadDeltaConstraint :: (Data rep, ForestMD md) => rep -> IO (ValueDeltas rep,ValueDeltas md) -> FilePath -> ((rep,md) -> Bool) -> IO (ValueDeltas rep,ValueDeltas md)
doLoadDeltaConstraint rep load path pred = do
	(drep,dmd) <- load
	rep' <- applyValueDeltas drep rep
	return (drep,dmd ++ [Apply $ \md' -> if pred (rep',md') then md' else replace_fmd_header md' $ updateForestMDwith (get_fmd_header md') [constraintViolation] ])

doLoadMaybe :: ForestMD md =>  FilePath -> IO (rep,md) -> IO (Maybe rep, (Forest_md, Maybe md))
doLoadMaybe path f = do
   { exists <- fileExist path
   ; if not exists then
       return (Nothing, (cleanForestMDwithFile path, Nothing))
     else do
      { (r,fmd) <- f 
      ; let bfmd = get_fmd_header fmd
      ; if Language.Forest.MetaData.numErrors bfmd == 0 
         then return (Just r, (bfmd, Just fmd))
         else return (Nothing, (cleanForestMDwithFile path, Nothing))
      }
   }

doLoadDeltaMaybe :: ForestMD md => FilePath -> ((rep,md) -> IO (ValueDeltas rep,ValueDeltas md)) -> IO (rep,md) -> (Maybe rep,(Forest_md,Maybe md)) -> IO (ValueDeltas (Maybe rep), ValueDeltas (Forest_md, Maybe md))
doLoadDeltaMaybe path fDelta f (mrep,(fmd,mmd)) = do
	exists <- fileExist path
	case (exists,mrep,mmd) of
		(False,_,_) -> return $ ([Replace Nothing],[Replace (cleanForestMDwithFile path,Nothing)])
		(True,Just rep,Just md) -> do
			(drep,dmd) <- fDelta (rep,md)
			return $ ( [ModMb drep], [ModSnd [ModMb dmd],Apply $ \(fmd,imd) -> (revalidateForestMDwith fmd [get_fmd_header $ fromJust imd],imd)] )
		otherwise -> liftM replaceBoth (doLoadMaybe path f)

doLoadDeltaSimpleIs :: (Data rep,ForestMD md) => FilePath -> FilePath -> FilePath -> FileSystemDeltas -> (FilePath -> FileSystemDeltas -> IO (ValueDeltas rep,ValueDeltas md)) -> IO (ValueDeltas rep,ValueDeltas md) -> IO (ValueDeltas rep,ValueDeltas md)
doLoadDeltaSimpleIs path newName fullpath_old df loadD loadS = trace ("reloading simple is "++path++":"++newName ++ ":"++fullpath_old++ ":"++show df) $ case isSubPathOf fullpath_old path of
	Just oldName -> if movePathToPathInFSDelta path oldName newName df
		then skipValidationIf (oldName == newName) $ do
				(drep,dmd) <- loadD newName (intersectFSDeltasWithPathsUnder df $ concatPath path newName)
				return (drep,Apply (replace_fullpath $ concatPath path newName) : dmd) -- we need to update the fullpath in the metadata
		else trace "loadS1" $ loadS
	Nothing -> trace "loadS2" $ loadS

doLoadDeltaSimpleMatches :: (Data rep,ForestMD md,Matching a) => FilePath -> a -> FilePath -> FileSystemDeltas -> (FilePath -> FileSystemDeltas -> IO (ValueDeltas rep,ValueDeltas md)) -> IO (ValueDeltas rep,ValueDeltas md) -> IO (ValueDeltas rep,ValueDeltas md)
doLoadDeltaSimpleMatches path expr fullpath_old df loadD loadS = trace ("reloading simple match "++path ++ show df) $case isSubPathOf fullpath_old path of
	Just oldName -> do
		case movePathToInFSDelta path oldName df of
			Just newName -> do
				newNames <- getMatchingFiles path expr
				if newName `elem` newNames
					then skipValidationIf (oldName == newName) $ do
						(drep,dmd) <- loadD newName (intersectFSDeltasWithPathsUnder df $ concatPath path newName)
						let multiple md = if length newNames > 1 then replace_fmd_header md (addMultipleMatchesErrorMD newNames $ get_fmd_header md) else md
						return (drep,Apply (multiple . replace_fullpath (concatPath path newName)) : dmd)
					else trace "loadS3" $ loadS
			otherwise -> trace "loadS4" $ loadS
	Nothing -> trace "loadS5" $ loadS

type CompDeltaList rep md = [(String , (FilePath -> (rep,md) -> FileSystemDeltas -> IO (ValueDeltas rep,ValueDeltas md) , FilePath -> IO (rep,md)))]

lookupLoadD j db = fst $ snd $ db!!j
lookupLoadS name = snd . fromJust . lookup name

doLoadCompoundDelta :: (Data rep,ForestMD md) => FilePath -> ([(String,rep)],[(String,md)]) -> FileSystemDeltas -> CompDeltaList rep md -> IO (ListDeltas (String,rep),ListDeltas (String,md))
doLoadCompoundDelta path (rep,md) df newNamesLoad = trace ("reloading compound inside"++show path) $ do
	let oldNames = map fst rep
	let newNames = map fst newNamesLoad
	(dmods,mapdw,dw) <- trace (show oldNames ++"----\n"++ show newNames) $ alignFileLists path df oldNames newNames
	let loadiD (i,j) = doLoadIDelta i path (oldNames!!i) (dmods!!i) (snd $ rep!!i,snd $ md!!i) df (lookupLoadD j newNamesLoad)
	(dRepMods,dMdMods) <- liftM ((concat >< concat) . unzip) $ mapM loadiD mapdw
	(dRepLst,dMdLst) <- doLoadPathDeltas path newNamesLoad dw
	return (dRepMods ++ dRepLst , dMdMods ++ dMdLst)

doLoadIDelta :: (Data rep,ForestMD md) => Int -> FilePath -> FilePath -> ValueDeltas FilePath -> (rep,md) -> FileSystemDeltas -> (FilePath -> (rep,md) -> FileSystemDeltas -> IO (ValueDeltas rep,ValueDeltas md)) -> IO (ListDeltas (String,rep),ListDeltas (String,md))
doLoadIDelta i path newfile [] (vi,di) df loadiD = skipValidation $ do -- filename did not change
	let newPath = concatPath path newfile
	(drepi,dmdi) <- loadiD newPath (vi,di) (intersectFSDeltasWithPathsUnder df newPath)
	return $ ([ModPos i [ModSnd drepi]],[ModPos i [ModSnd dmdi]])
doLoadIDelta i path oldfile dfile (vi,di) df loadiD = do -- since this represents a FS move, we need to update the fullpath in the metadata
	newfile <- applyValueDeltas dfile oldfile
	let newPath = concatPath path newfile
	(drepi,dmdi) <- loadiD newPath (vi,di) (intersectFSDeltasWithPathsUnder df newPath)
	return $ ([ModPos i [ModFst [Replace newfile],ModSnd drepi]],[ModPos i [ModFst [Replace newfile],ModSnd $ Apply (replace_fullpath newPath) : dmdi]])

doLoadPathDeltas :: FilePath -> CompDeltaList rep md -> ListDeltas String -> IO (ListDeltas (String,rep),ListDeltas (String,md))
doLoadPathDeltas path newNamesLoad [] = return ([],[])
doLoadPathDeltas path newNamesLoad (DelPos p : ds) = doLoadPathDeltas path newNamesLoad ds >>= \(dv,dd) -> return (DelPos p:dv,DelPos p:dd)
doLoadPathDeltas path newNamesLoad (ReorderPos f : ds) = doLoadPathDeltas path newNamesLoad ds >>= \(dv,dd) -> return (ReorderPos f:dv,ReorderPos f:dd)
doLoadPathDeltas path newNamesLoad (InsPos p name : ds) = do -- static load for the new file
	(dv,dd) <- doLoadPathDeltas path newNamesLoad ds
	(rep,md) <- (lookupLoadS name newNamesLoad) (concatPath path name)
	return (InsPos p (name,rep) : dv , InsPos p (name,md) : dd)

-- we return a list of deltas on each position and a list of deltas represented both as a mapping from source positions to target positions and a sequence of DelPos,InsPos,ReorderPos operations
-- the first list reflects move operations in the filesystem
alignFileLists :: FilePath -> FileSystemDeltas -> [FilePath] -> [FilePath] -> IO ([ValueDeltas FilePath],[(Int,Int)],ListDeltas FilePath)
alignFileLists path df sfiles tfiles = do
	let dsfiles = alignMoves path df sfiles
	sfiles' <- mapM (uncurry applyValueDeltas) (zip dsfiles sfiles)
	(mapdw,dw) <- align sfiles' tfiles
	return (dsfiles,mapdw,dw)

alignMoves :: FilePath -> FileSystemDeltas -> [FilePath] -> [ValueDeltas FilePath]
alignMoves path df (sfile:sfiles) = case movePathToInFSDelta path sfile df of
		Just tfile@((==sfile) -> False) -> replaceDelta tfile : alignMoves path df sfiles
		otherwise -> [] : alignMoves path df sfiles
alignMoves path df [] = []

doLoadDirectoryDelta :: Data rep => FilePath -> FileSystemDeltas -> (Maybe FileInfo -> IO (ValueDeltas rep,ValueDeltas (Forest_md,md))) -> IO (rep,(Forest_md,md)) -> IO (ValueDeltas rep,ValueDeltas (Forest_md,md))
doLoadDirectoryDelta path df loadD loadS = trace ("reloading directory " ++ path) $ do
	case intersectFSDeltasWithPath True df path of
		[] -> skipValidation $ loadD Nothing -- when the directory is not updated
		[ChangeAttrs ((==path) -> True) a'] -> loadD (Just a') -- when only the directory attributes are updated
		[RemDir ((==path) -> True)] -> return ([],[ModFst [Apply (invalidateForestMD (MissingFile path))]]) -- when the directory is removed
		otherwise -> trace ("doLoadDirectoryDelta: " ++ show df) $ liftM replaceBoth loadS -- for any other case, default non-incremental load

doLoadConstantDelta :: Data rep => FilePath -> FileSystemDeltas -> IO (rep,(Forest_md,md)) -> IO (ValueDeltas rep,ValueDeltas (Forest_md,md))
doLoadConstantDelta path df loadS = trace ("reloading file " ++ path) $ do
	case intersectFSDeltasWithPath False df path of
		[] -> return ([],[])
		[ChangeAttrs ((==path) -> True) a'] -> return ([],[ModFst [Apply $ \fmd -> fmd { fileInfo = a' } ]])
		otherwise -> trace ("doLoadConstant: " ++ show df) $ liftM replaceBoth loadS

skipValidation = skipValidationIf True

skipValidationIf :: Bool -> IO (ValueDeltas rep,ValueDeltas md) -> IO (ValueDeltas rep,ValueDeltas md)
skipValidationIf b io = do
	(drep,dmd) <- io
	if b && isEmptyVD drep && isEmptyVD dmd then return ([],[]) else return (drep,dmd)

skipUpdate :: [Bool] -> IO (ValueDeltas rep,ValueDeltas md) -> IO (ValueDeltas rep,ValueDeltas md)
skipUpdate l io = if and l then return ([],[]) else io

emptyDelta :: [Bool] -> a -> ValueDeltas a
emptyDelta l v = if (and l) then [] else replaceDelta v

pEmptyDelta :: ValueDeltas v -> v1 -> ValueDeltas v1
pEmptyDelta d v = emptyDelta [isEmptyVD d] v

checkPredMD :: (Data rep,ForestMD md) => Bool -> IO (ValueDeltas rep,ValueDeltas md) -> IO (ValueDeltas rep,ValueDeltas md)
checkPredMD cond io = do
	(drep,dmd) <- io
	return (drep,if cond then dmd else [Apply $ \md -> replace_fmd_header md (addPredFailureMD $ get_fmd_header md)])

pickFile :: [FilePath] -> FilePath
pickFile files = case files of
  [] -> "-This is an illegal file path according to POSIX Standard."  -- an illegal file path
--  [] -> ""  -- an illegal file path
  f:fs -> f

checkMultiple :: (Data rep, ForestMD md) => [String] -> IO (rep,md) -> IO (rep,md)
checkMultiple files io = if length files > 1
	then do
		(rep,md) <- io
		return (rep,replace_fmd_header md (addMultipleMatchesErrorMD files $ get_fmd_header md))
	else io

checkNonEmpty :: (Data rep, ForestMD md) => FilePath -> FilePath -> IO(rep,md) -> IO(rep,md)
checkNonEmpty path file ifExists = 
  if file == "" then 
       do { let def_md = myempty
          ; let new_md = replace_fmd_header def_md (fileMatchFailureForestMD path)
          ; return (myempty, new_md)
          }
  else if file == "-This is an illegal file path according to POSIX Standard." then 
          return (myempty, myempty)
  else ifExists

checkPath :: (Data rep, ForestMD md) => FilePath -> IO (rep,md) -> IO (rep,md)
checkPath path ifExists = do 
   { exists <-  fileExist path
   ; if not exists then 
       do { let def_md = myempty
          ; let new_md = replace_fmd_header def_md (missingPathForestMD path)
          ; return (myempty, new_md)
          }
     else ifExists
   }

checkPathIsDir :: (Data rep, ForestMD md) => FilePath -> IO (rep,md) -> IO (rep,md)
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

invalidateForestMD :: Language.Forest.Errors.ErrMsg -> Forest_md -> Forest_md
invalidateForestMD errMsg f_md = f_md { Language.Forest.MetaData.numErrors = Language.Forest.MetaData.numErrors f_md + 1, errorMsg = Just errMsg }

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

envVar str = fromJust (unsafePerformIO $ (System.Posix.Env.getEnv str))

infix 7  ><
-- The infix product combinator.
(><) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
(f >< g) (x,y) = (f x,g y)
