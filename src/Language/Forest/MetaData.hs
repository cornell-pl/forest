{-# LANGUAGE CPP, NamedFieldPuns, RecordWildCards, FlexibleInstances, DeriveDataTypeable, TemplateHaskell,ScopedTypeVariables, DoAndIfThenElse,
    TypeSynonymInstances #-}
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

module Language.Forest.MetaData where

import Control.Monad.IO.Class
import System.Posix.Files
import System.Posix.User
import System.Posix.Types
import System.FilePath.Posix
import System.Process
import GHC.IO.Handle
import Foreign.C.Types
import System.Posix.Directory
import System.Posix.Time
import System.Directory
import Text.Regex
import System.Time.Utils
import Data.Time.Clock.POSIX
import Data.Time.Clock

import qualified Control.Exception as CE

import Data.Data
import Data.Generics.Builders
import Data.DeriveTH                 -- Library for deriving instances for existing types

import Data.List hiding (group, unzip, zip)

import Text.PrettyPrint.Mainland as PP hiding (group, empty)

import Language.Pads.Source
import Language.Pads.RegExp 
import Language.Pads.Generic
import Language.Pads.GenPretty
import Language.Forest.Errors


{- Base type library support -}



data FileType = UnknownK | AsciiK | BinaryK | DirectoryK 
 deriving (Eq, Data, Typeable, Ord)

instance Show FileType where
 show UnknownK = "Unknown"
 show AsciiK  = "ASCII File"
 show BinaryK  = "Binary File"
 show DirectoryK = "Directory"

(derive makeDataAbstract ''COff)
(derive makeDataAbstract ''CMode)
(derive makeDataAbstract ''CTime)

data FileInfo = FileInfo { fullpath :: FilePath
                         , owner :: String
                         , group :: String
                         , size  :: COff
                         , access_time :: EpochTime
                         , mod_time :: EpochTime
                         , read_time :: EpochTime
                         , mode :: FileMode
                         , symLink :: Maybe FilePath
                         , kind :: FileType
                         }
       deriving (Eq, Data, Typeable, Ord)

{- Function System.Time.Utils.epochToClockTime converts EpochTime to CalendarTime -}

instance Show FileInfo where
  show f = "FileInfo {" 
                    ++  "fullpath = " ++ (fullpath f)     ++ ", "
                    ++  "owner = "    ++ (owner f)        ++ ", "
                    ++  "group = "    ++ (group f)        ++ ", "
                    ++  "size = "     ++ (show $ size f)  ++ ", "
                    ++  "access_time = "     ++ (show $ epochToClockTime $ access_time f)  ++ ", "
                    ++  "mod_time = "        ++ (show $ epochToClockTime $ mod_time f)     ++ ", "
                    ++  "read_time = "       ++ (show $ epochToClockTime $ read_time f)    ++ ", "
                    ++  "mode = "            ++ (modeToModeString $ mode f)                ++ ", "
                    ++  "symLink = "         ++ (show $ symLink f)                       ++ ", "
                    ++  "kind = "            ++ (show $ kind f)                            ++ "}"



instance Pretty COff where
 ppr = text . show 

instance Pretty EpochTime where
 ppr = text . show . epochToClockTime 

instance Pretty FileMode where
 ppr = text . modeToModeString


fileInfo_def = FileInfo { fullpath = ""
                        , owner = ""
                        , group = ""
                        , size = 0
                        , access_time = 0
                        , read_time = 0
                        , mod_time = 0
                        , mode = 0
                        , symLink = Nothing
                        , kind = UnknownK       
                        }

errorFileInfo :: FileInfo
errorFileInfo  = FileInfo 
     { fullpath = ""
     , owner = ""
     , group = ""
     , size  = -1
     , access_time = -1
     , read_time = -1
     , mod_time = -1
     , mode     = -1
     , symLink = Nothing
     , kind = UnknownK
     }

mkErrorFileInfo :: FilePath -> FileInfo
mkErrorFileInfo  path = FileInfo 
     { fullpath = path
     , owner = ""
     , group = ""
     , size  = -1
     , access_time = -1
     , read_time = -1
     , mod_time = -1
     , mode     = -1
     , symLink = Nothing
     , kind = UnknownK
     }


data Forest_md = Forest_md { numErrors :: Int
                           , errorMsg  :: Maybe ErrMsg
                           , fileInfo  :: FileInfo 
                           }
   deriving (Typeable, Data, Eq, Show, Ord)

forest_md_def = Forest_md{ numErrors = 1
                         , errorMsg = Nothing
                         , fileInfo = fileInfo_def
                         }


{- Meta data type class -}
class Data md => ForestMD md where
  get_fmd_header :: md -> Forest_md
  replace_fmd_header :: md -> Forest_md -> md
  get_fileInfo :: md -> FileInfo
  get_fileInfo md = fileInfo (get_fmd_header md)
  get_fullpath :: md -> String
  get_fullpath md = fullpath $ fileInfo (get_fmd_header md)
  get_owner :: md -> String
  get_owner md = owner $ fileInfo (get_fmd_header md)
  get_group :: md -> String
  get_group md = group $ fileInfo (get_fmd_header md)
  get_size :: md -> COff
  get_size md = size $ fileInfo (get_fmd_header md)
  get_access_time :: md -> EpochTime
  get_access_time md = access_time $ fileInfo (get_fmd_header md)
  get_mod_time :: md -> EpochTime
  get_mod_time md = mod_time $ fileInfo (get_fmd_header md)
  get_read_time :: md -> EpochTime
  get_read_time md = read_time $ fileInfo (get_fmd_header md)
  get_mode :: md -> FileMode
  get_mode md = mode $ fileInfo (get_fmd_header md)
  get_modes :: md -> String
  get_modes md = modeToModeString $ mode $ fileInfo (get_fmd_header md)
  get_symLink :: md -> Maybe FilePath
  get_symLink md = symLink $ fileInfo (get_fmd_header md)
  get_kind :: md -> FileType
  get_kind md = kind $ fileInfo (get_fmd_header md)

instance ForestMD Forest_md where
  get_fmd_header b = b
  replace_fmd_header old new = new

instance Data b => ForestMD (Forest_md,b) where
  get_fmd_header (h,b) = h
  replace_fmd_header (h1,b) h2 = (h2,b)



cleanForestMD = Forest_md {numErrors = 0, errorMsg = Nothing, fileInfo = fileInfo_def}
errorForestMD = Forest_md {numErrors = 1, errorMsg = Nothing, fileInfo = errorFileInfo}
missingPathForestMD path = Forest_md {numErrors = 1, errorMsg = Just (MissingFile path), fileInfo = mkErrorFileInfo path}
fileMatchFailureForestMD path = Forest_md {numErrors = 1, errorMsg = Just (MatchFailure path), fileInfo = mkErrorFileInfo path}
systemErrorForestMD i = Forest_md {numErrors = 1, errorMsg = Just (SystemError i), fileInfo = errorFileInfo}
notDirectoryForestMD path = Forest_md {numErrors = 1, errorMsg = Just (NotADirectory path), fileInfo = mkErrorFileInfo path}
constraintViolation       = Forest_md {numErrors = 1, errorMsg = Just ConstraintViolation, fileInfo = errorFileInfo}

mergeErrors m1 m2 = case (m1,m2) of
            (Nothing,Nothing) -> Nothing
            (Just a, _) -> Just a
            (_, Just b) -> Just b

cleanForestMDwithFile :: FilePath -> Forest_md
cleanForestMDwithFile path = setFilePathinMD path cleanForestMD

removeGzipSuffix :: Forest_md -> Forest_md
removeGzipSuffix fmd = 
  let fp = get_fullpath fmd
      fp' = System.FilePath.Posix.dropExtension fp
  in setFilePathinMD fp' fmd

getTempForestDirectory :: IO FilePath
getTempForestDirectory = do 
  { tempDir <- getTemporaryDirectory 
  ; let forestDir = combine tempDir "Forest"
  ; createDirectoryIfMissing False forestDir
  ; return forestDir
  }


setFilePathinMD :: FilePath -> Forest_md -> Forest_md
setFilePathinMD path (Forest_md {numErrors, errorMsg, fileInfo}) = 
                Forest_md {numErrors, errorMsg, fileInfo = setFilePathinFileInfo path fileInfo}

setFilePathinFileInfo :: FilePath -> FileInfo -> FileInfo
setFilePathinFileInfo path 
               (FileInfo { fullpath 
                         , owner
                         , group
                         , size
                         , access_time
                         , mod_time
                         , read_time
                         , mode 
                         , symLink
                         , kind
                         }) = 
                FileInfo { fullpath = path 
                         , owner
                         , group
                         , size
                         , access_time
                         , mod_time
                         , read_time
                         , mode 
                         , symLink
                         , kind
                         }  


mergeForestMDs mds = 
     Data.List.foldl (\(Forest_md {numErrors=num1, errorMsg = errorMsg1, fileInfo=info1}) 
                       (Forest_md {numErrors=num2, errorMsg = errorMsg2, fileInfo=info2}) ->
                       (Forest_md {numErrors=num1 + num2 
                                  ,errorMsg = mergeErrors errorMsg1 errorMsg2
                                  ,fileInfo = errorFileInfo })) 
                     cleanForestMD                          
                     mds

updateForestMDwith base updates =  let
       (Forest_md {numErrors=numd, errorMsg = errorMsgd, fileInfo=infod}) = base
       (Forest_md {numErrors=numc, errorMsg = errorMsgc, fileInfo=infoc}) = mergeForestMDs updates
   in  (Forest_md {numErrors=numd + numc 
                  , errorMsg = mergeErrors errorMsgd errorMsgc
                  , fileInfo=infod})


isAscii_old :: FilePath -> IO Bool
isAscii_old fp = do 
  { let cmd = "file -i -L " ++ fp
  ; (_, Just hout, _, ph) <-
       createProcess (shell cmd){ std_out = CreatePipe }

  ; result <- hGetLine hout
  ; hClose hout
  ; waitForProcess ph
  ; return ("ascii" `Data.List.isSuffixOf` result)
  }

isAscii :: FilePath -> IO Bool
isAscii fp =  do
  { let cmd = "file -i -L " ++ fp
  ; result <- doShellCmd cmd
  ; return ("ascii" `Data.List.isSuffixOf` result)
  }

fileStatusToKind :: FilePath -> FileStatus -> IO FileType
fileStatusToKind path fs = do
    if isRegularFile fs then 
      do { ascii <- isAscii path
         ; if ascii then return AsciiK else return BinaryK
         }
    else if isDirectory fs then return DirectoryK
    else return UnknownK
   

specialStringToMode special = 
    case special of
      '-' -> [regularFileMode]
      'd' -> [directoryMode]
      'b' -> [blockSpecialMode]
      'c' -> [characterSpecialMode]
      'l' -> [symbolicLinkMode]
      'p' -> [namedPipeMode]
      's' -> [socketMode]
      '?' -> []

modeStringToMode' :: String -> [FileMode]
modeStringToMode' [special,or,ow,ox,gr,gw,gx,otr,otw,otx] = 
                  specialM ++ ownerrM ++ ownerwM ++ ownerxM ++ grouprM ++ groupwM ++ groupxM  ++ otherrM ++ otherwM ++ otherxM
  where  specialM = specialStringToMode special
         ownerrM  = if or == 'r' then [ownerReadMode] else []
         ownerwM  = if ow == 'w' then [ownerWriteMode] else []
         ownerxM  = if ox == 'x' then [ownerExecuteMode] else []
         grouprM  = if gr == 'r' then [groupReadMode] else []
         groupwM  = if gw == 'w' then [groupWriteMode] else []
         groupxM  = if gx == 'x' then [groupExecuteMode] else []
         otherrM  = if otr == 'r' then [otherReadMode] else []
         otherwM  = if otw == 'w' then [otherWriteMode] else []
         otherxM  = if otx == 'x' then [otherExecuteMode] else []

unionFileModeL modes = foldl1 unionFileModes modes


modeStringToMode :: String -> FileMode
modeStringToMode s = unionFileModeL $ modeStringToMode' s

modeToModeString :: FileMode -> String
modeToModeString mode = special ++ ownerr ++ ownerw ++ ownerx ++ groupr ++ groupw ++ groupx ++ otherr ++ otherw ++ otherx
   where special = if intersectFileModes mode regularFileMode == regularFileMode then "-"
                   else if intersectFileModes mode directoryMode == directoryMode then "d"
                   else if intersectFileModes mode blockSpecialMode == blockSpecialMode then "b"
                   else if intersectFileModes mode characterSpecialMode == characterSpecialMode then "c"
                   else if intersectFileModes mode symbolicLinkMode == symbolicLinkMode then "l"
                   else if intersectFileModes mode namedPipeMode == namedPipeMode then "p"
                   else if intersectFileModes mode socketMode == socketMode then "s"
                   else "?"
         ownerr = if intersectFileModes mode ownerReadMode == ownerReadMode then "r" else "-"
         ownerw = if intersectFileModes mode ownerWriteMode == ownerWriteMode then "w" else "-"
         ownerx = if intersectFileModes mode ownerExecuteMode == ownerExecuteMode then "x" else "-"
         groupr = if intersectFileModes mode groupReadMode == groupReadMode then "r" else "-"
         groupw = if intersectFileModes mode groupWriteMode == groupWriteMode then "w" else "-"
         groupx = if intersectFileModes mode groupExecuteMode == groupExecuteMode then "x" else "-"
         otherr = if intersectFileModes mode otherReadMode == otherReadMode then "r" else "-"
         otherw = if intersectFileModes mode otherWriteMode == otherWriteMode then "w" else "-"
         otherx = if intersectFileModes mode otherExecuteMode == otherExecuteMode then "x" else "-"

pl (c1, c2) = case (c1,c2) of
  (c1, '-') -> True
  (c1,c2) | c1 == c2 -> True
  otherwise -> False

(<==) s1 s2 = and (map pl (zip s1 s2))

data Permission = Read | Write | Execute
  deriving (Eq, Ord, Show, Typeable, Data)

ownModeToPermission :: FileMode -> [Permission]
ownModeToPermission mode = ownerr ++ ownerw ++ ownerx
   where ownerr = if intersectFileModes mode ownerReadMode == ownerReadMode then [Read] else []
         ownerw = if intersectFileModes mode ownerWriteMode == ownerWriteMode then [Write] else []
         ownerx = if intersectFileModes mode ownerExecuteMode == ownerExecuteMode then [Execute] else []

grpModeToPermission :: FileMode -> [Permission]
grpModeToPermission mode = groupr ++ groupw ++ groupx
   where
     groupr = if intersectFileModes mode groupReadMode == groupReadMode then [Read] else []
     groupw = if intersectFileModes mode groupWriteMode == groupWriteMode then [Write] else []
     groupx = if intersectFileModes mode groupExecuteMode == groupExecuteMode then [Execute] else []

othModeToPermission :: FileMode -> [Permission]
othModeToPermission mode = otherr ++ otherw ++ otherx
   where
     otherr = if intersectFileModes mode otherReadMode == otherReadMode then [Read] else []
     otherw = if intersectFileModes mode otherWriteMode == otherWriteMode then [Write] else []
     otherx = if intersectFileModes mode otherExecuteMode == otherExecuteMode then [Execute] else []

getPermissionsForGroups :: [String] -> FileInfo -> [Permission]
getPermissionsForGroups groups fInfo = 
  if (group fInfo) `elem` groups 
    then grpModeToPermission (mode fInfo)
    else []

getPermissionsForOwner :: String -> FileInfo -> [Permission]
getPermissionsForOwner id fInfo = 
  if (owner fInfo) == id
    then ownModeToPermission (mode fInfo)
    else []

getPermissionsForOther :: FileInfo -> [Permission]
getPermissionsForOther  fInfo =  othModeToPermission (mode fInfo)

getPermissions :: String -> [String] -> FileInfo -> [Permission] 
getPermissions id groups fInfo = nub (owner++group++other)
  where owner = getPermissionsForOwner id fInfo
        group = getPermissionsForGroups groups fInfo
        other = getPermissionsForOther fInfo

getGroups :: String -> IO [String]
getGroups id = do
  { groupEntries <- getAllGroupEntries 
  ; let id_groups = filter (\g -> id `elem` (groupMembers g)) groupEntries
  ; return (Data.List.map groupName id_groups)
  }

{- Should raise no exceptions -}
getForestMD :: FilePath -> IO Forest_md
getForestMD path = do
    fdEither <- CE.try (getFileStatus path)
    case fdEither of
        Left (e::CE.SomeException) -> return
                (Forest_md { numErrors = 1
                           , errorMsg = Just (ForestIOException (show e))
                           , fileInfo = mkErrorFileInfo path
                           })
        Right fd -> do 
         { let file_ownerID = fileOwner fd
         ; ownerEntry    <- getUserEntryForID file_ownerID
         ; let file_groupID = fileGroup fd
         ; groupEntry <- getGroupEntryForID file_groupID
         ; fdsym <- getSymbolicLinkStatus path
         ; symLinkOpt <- if isSymbolicLink fdsym then readOptSymLink path else return Nothing
         ; readTime <- epochTime
         ; knd <- fileStatusToKind path fd
         ; return (Forest_md{ numErrors = 0
                            , errorMsg = Nothing
                            , fileInfo = FileInfo 
                                          { fullpath = path
                                          , owner = userName ownerEntry
                                          , group = groupName groupEntry
                                          , size  = fileSize fd
                                          , access_time = accessTime fd
                                          , mod_time = modificationTime fd
                                          , read_time = readTime
                                          , mode     =  fileMode fd
                                          , symLink =symLinkOpt
                                          , kind  = knd
                                          }
                            })
         }

getRelForestMD :: FilePath -> FilePath -> IO Forest_md
getRelForestMD root local = getForestMD (root++"/"++local)

readOptSymLink :: FilePath -> IO (Maybe FilePath)
readOptSymLink fp = do 
  { link <- readSymbolicLink fp
  ; return (Just link)
  }

{- Get a list of the files in the argument directory. 
   Replaced by getDirectoryContents -}
getFiles' :: FilePath -> IO [FilePath]
getFiles' path = do 
  { dirStream <- openDirStream path
  ; answer <- get_files dirStream
  ; closeDirStream dirStream
  ; return answer
  } where get_files dirStream = do
                     { file <- readDirStream dirStream
                     ; if file == [] then return []
                                     else do { files <- get_files dirStream
                                             ; return (file : files)
                                             } 
                     }

doShellCmd :: String -> IO String
doShellCmd cmd = do 
	debugIO $ "Executing cmd: " ++ cmd
	(_, Just hout, _, ph) <- createProcess (shell cmd){ std_out = CreatePipe }
	isEof <- hIsEOF hout
	if isEof 
		then do
			waitForProcess ph
			return ""
	else do
		result <- hGetLine hout
		hClose hout
		waitForProcess ph
		return result

debugIO :: MonadIO m => String -> m ()
#ifdef DEBUG
debugIO str = putStrLn str
#endif
#ifndef DEBUG
debugIO str = return ()
#endif


