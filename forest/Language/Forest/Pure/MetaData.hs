{-# LANGUAGE StandaloneDeriving, DataKinds, TypeFamilies, TypeOperators, UndecidableInstances, ConstraintKinds, FlexibleContexts, MultiParamTypeClasses, RankNTypes, NamedFieldPuns, RecordWildCards, FlexibleInstances, DeriveDataTypeable, TemplateHaskell,ScopedTypeVariables, DoAndIfThenElse,
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

***********************************************************************i
-}

module Language.Forest.Pure.MetaData where

import Control.Exception
import Language.Forest.IO.Utils
import Data.Word
import Data.Int
import Language.Pads.CoreBaseTypes
import Language.Pads.MetaData hiding (numErrors)
import Data.Maybe
import Control.Monad
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
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Control.Exception as CE

import Data.Data
import Data.Generics.Aliases
import Language.Haskell.TH.Syntax

import Data.List hiding (group, unzip, zip)

import Text.PrettyPrint.Mainland as PP hiding (group, empty,(</>))

import Language.Pads.Source
import Language.Pads.RegExp 
import Language.Pads.Generic hiding (ext1B,ext2B)
import Language.Pads.GenPretty
import Language.Forest.Errors
import Language.Forest.FS.FSRep
import System.IO.Unsafe
import Data.DeriveTH

$(derive makeDataAbstract ''COff)
$(derive makeDataAbstract ''CMode)
$(derive makeDataAbstract ''CTime)
deriving instance Data FileType
deriving instance Data FileInfo

data Forest_md = Forest_md { errors :: Forest_err, fileInfo :: FileInfo } deriving (Typeable,Data,Eq)

type GetForestMD fs = (FilePath -> FSTree fs -> ForestM fs Forest_md)

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
       deriving (Eq, Typeable, Ord)
data FileType = UnknownK | AsciiK | BinaryK | DirectoryK 
 deriving (Eq, Typeable, Ord)

{- Should raise no exceptions -}
-- it loads the metadata from a physical path on disk, but returns the metadata as if it belonged to an original path alias
getForestMD :: (FSRep fs) => FilePath -> FilePath -> ForestM fs Forest_md
getForestMD oripath diskpath = do
	fdEither <- forestIO $ try $ getFileStatus diskpath
	case fdEither of
		Left (e::SomeException) -> do
			let err = Forest_err { numErrors = 1, errorMsg = Just (ForestIOException (show e)) }
			return $ Forest_md { errors = err, fileInfo = mkErrorFileInfo oripath }
		Right fd -> do 
			let file_ownerID = fileOwner fd
			ownerEntry    <- forestIO $ getUserEntryForID file_ownerID
			let file_groupID = fileGroup fd
			groupEntry <- forestIO $ getGroupEntryForID file_groupID
			fdsym <- forestIO $ getSymbolicLinkStatus diskpath
			symLinkOpt <- forestIO $ if isSymbolicLink fdsym then readOptSymLink diskpath else return Nothing
			readTime <- forestIO epochTime
			knd <- forestIO $ fileStatusToKind diskpath fd
			let err = Forest_err { numErrors = 0, errorMsg = Nothing }
			let info = FileInfo { fullpath = oripath, owner = userName ownerEntry, group = groupName groupEntry, size  = fileSize fd, access_time = accessTime fd, mod_time = modificationTime fd, read_time = readTime, mode     =  fileMode fd, symLink =symLinkOpt, kind  = knd }
			return $ Forest_md { errors = err, fileInfo = info } 

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

fileStatusToKind :: FilePath -> FileStatus -> IO FileType
fileStatusToKind path fs = do
    if isRegularFile fs then 
      do { ascii <- isAscii path
         ; if ascii then return AsciiK else return BinaryK
         }
    else if isDirectory fs then return DirectoryK
    else return UnknownK


{- Base type library support -}

instance Show FileType where
 show UnknownK = "Unknown"
 show AsciiK  = "ASCII File"
 show BinaryK  = "Binary File"
 show DirectoryK = "Directory"

{- Function System.Time.Utils.epochToClockTime converts EpochTime to CalendarTime -}

instance Show FileInfo where
  show f = "FileInfo {" 
                    ++  "fullpath = " ++ (show $ fullpath f)     ++ ", "
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

-- for cases where we want to avoid reading from the filesystme
doesExistInMD :: FilePath -> Forest_md -> Bool
doesExistInMD path fmd = (fullpath (fileInfo fmd) == path) && (access_time (fileInfo fmd) > 0 || read_time (fileInfo fmd) > 0)

doesFileExistInMD :: FilePath -> Forest_md -> Bool
doesFileExistInMD path fmd = doesExistInMD path fmd && kind (fileInfo fmd) /= DirectoryK

doesDirectoryExistInMD :: FilePath -> Forest_md -> Bool
doesDirectoryExistInMD path fmd = doesExistInMD path fmd && kind (fileInfo fmd) == DirectoryK

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
errorFileInfo  = mkErrorFileInfo ""

forest_md_def :: Forest_md
forest_md_def = Forest_md (forest_err_def) fileInfo_def
forest_err_def :: Forest_err
forest_err_def = Forest_err 1 Nothing

fmd_fullpath :: Forest_md -> FilePath
fmd_fullpath = fullpath . fileInfo

fmd_kind :: Forest_md -> FileType
fmd_kind = kind . fileInfo

fmd_symLink :: Forest_md -> Maybe FilePath
fmd_symLink = symLink . fileInfo

{- Meta data type class -}
class ForestMD md where
	get_fmd_header :: md -> Forest_md
	replace_fmd_header :: md -> (Forest_md -> Forest_md) -> md
	
	get_errors :: md -> Forest_err
	get_errors md = errors (get_fmd_header md)
	isValidMD :: md -> Bool
	isValidMD md = numErrors (get_errors md) == 0
	replace_errors :: md -> (Forest_err -> Forest_err) -> md
	replace_errors md f = replace_fmd_header md $ \fmd -> fmd { errors = f (get_errors fmd) }
		
	get_fileInfo :: md -> FileInfo
	get_fileInfo md = fileInfo $ get_fmd_header md
	get_fullpath :: md -> String
	get_fullpath md = fullpath $ fileInfo $ get_fmd_header md
	get_owner :: md -> String
	get_owner md = owner $ fileInfo $ get_fmd_header md
	get_group :: md -> String
	get_group md = group $ fileInfo $ get_fmd_header md
	get_size :: md -> COff
	get_size md = size $ fileInfo $ get_fmd_header md
	get_access_time :: md -> EpochTime
	get_access_time md = access_time $ fileInfo $ get_fmd_header md
	get_mod_time :: md -> EpochTime
	get_mod_time md = mod_time $ fileInfo $ get_fmd_header md
	get_read_time :: md -> EpochTime
	get_read_time md = read_time $ fileInfo $ get_fmd_header md
	get_mode :: md -> FileMode
	get_mode md = mode $ fileInfo $ get_fmd_header md
	get_modes :: md -> String
	get_modes md = modeToModeString $ mode $ fileInfo $ get_fmd_header md
	get_symLink :: md -> (Maybe FilePath)
	get_symLink md = symLink $ fileInfo $ get_fmd_header md
	get_kind :: md -> FileType
	get_kind md = kind $ fileInfo $ get_fmd_header md


instance ForestMD Forest_md where
	get_fmd_header b = b
	replace_fmd_header fmd f = f fmd

instance ForestMD (Forest_md,b) where
	get_fmd_header (a,b) = get_fmd_header a
	replace_fmd_header (fmd,b) f = let fmd' = replace_fmd_header fmd f in (fmd',b)
instance (ForestMD a) => ForestMD ((a,b),c) where
	get_fmd_header ((a,b),c) = get_fmd_header a
	replace_fmd_header ((a,b),c) f = let a' = replace_fmd_header a f in ((a',b),c)

cleanForestMD :: Forest_md
cleanForestMD = Forest_md { errors = cleanForestMDErr, fileInfo = fileInfo_def}
cleanForestMDErr = Forest_err {numErrors = 0, errorMsg = Nothing }

errorForestMD :: Forest_md
errorForestMD = Forest_md { errors = errorForestMDErr, fileInfo = errorFileInfo}
errorForestMDErr = Forest_err {numErrors = 1, errorMsg = Nothing }
missingPathForestMD path = Forest_md { errors = missingPathForestErr path, fileInfo = mkErrorFileInfo path}
missingPathForestErr path = Forest_err { numErrors = 1, errorMsg = Just (MissingFile path) }
wrongFileExtensionForestErr ext path = Forest_err { numErrors = 1, errorMsg = Just (WrongFileExtension ext path) }
missingDirForestMD path = Forest_md {errors = missingDirForestErr path, fileInfo = mkErrorFileInfo path}
missingDirForestErr path = Forest_err { numErrors = 1, errorMsg = Just (MissingDirectory path) }
fileMatchFailureForestMD path = Forest_md {errors = fileMatchFailureForestErr path, fileInfo = mkErrorFileInfo path}
fileMatchFailureForestErr path = Forest_err { numErrors = 1, errorMsg = Just (MatchFailure path) }
systemErrorForestMD i = Forest_md {errors = systemErrorForestErr i, fileInfo = errorFileInfo}
systemErrorForestErr i = Forest_err { numErrors = 1, errorMsg = Just (SystemError i) }
notDirectoryForestMD path = Forest_md {errors = notDirectoryForestErr path, fileInfo = mkErrorFileInfo path}
notDirectoryForestErr path = Forest_err { numErrors = 1, errorMsg = Just (NotADirectory path) }
constraintViolationForestMD :: Forest_md
constraintViolationForestMD = Forest_md {errors = constraintViolationForestErr, fileInfo = errorFileInfo}
constraintViolationForestErr = Forest_err { numErrors = 1, errorMsg = Just ConstraintViolation }
ioExceptionForestMD :: Forest_md
ioExceptionForestMD = Forest_md { errors = ioExceptionForestErr, fileInfo = errorFileInfo }
ioExceptionForestErr = Forest_err { numErrors = 1, errorMsg = Just (ForestIOException $ "not a symbolic link") }

mergeErrors m1 m2 = case (m1,m2) of
            (Nothing,Nothing) -> Nothing
            (Just a, _) -> Just a
            (_, Just b) -> Just b

cleanForestMDwithFile :: FilePath -> Forest_md
cleanForestMDwithFile path =
	let fmd = cleanForestMD
	in fmd { fileInfo = (fileInfo fmd) { fullpath = path } }

-- | Tests if two metadata values are both valid or invalid
sameValidity :: (ForestMD md1,ForestMD md2) => md1 -> md2 -> Bool
sameValidity md1 md2 =
	let err1 = get_errors md1
	    err2 = get_errors md2
	in (numErrors err1 == 0) == (numErrors err2 == 0)

sameValidity' :: (ForestMD md1) => md1 -> Forest_err -> Bool
sameValidity' md1 err2 = (numErrors (get_errors md1) == 0) == (numErrors err2 == 0)

-- | Tests if two metadata values point to the same filepath
sameFullPath :: (ForestMD md1,ForestMD md2) => md1 -> md2 -> Bool
sameFullPath md1 md2 =
	let fmd1 = get_fmd_header md1
	    fmd2 = get_fmd_header md2
	in fullpath (fileInfo fmd1) == fullpath (fileInfo fmd2)

-- | Tests if two metadata values point to the same canonical filepath, in respect to a given tree
sameCanonicalFullPathInTree :: (FSRep fs,ForestMD md1,ForestMD md2) => md1 -> md2 -> FSTree fs -> ForestM fs Bool
sameCanonicalFullPathInTree md1 md2 tree = do
	let fmd1 = get_fmd_header md1
	let fmd2 = get_fmd_header md2
	let path1 = fullpath (fileInfo fmd1)
	let path2 = fullpath (fileInfo fmd2)
	canpath1 <- canonalizePathWithTree path1 tree
	canpath2 <- canonalizePathWithTree path2 tree
	return $ canpath1 == canpath2

mergeForestErrs :: Forest_err -> Forest_err -> Forest_err
mergeForestErrs err1 err2 = Forest_err { numErrors = numErrors err1 + numErrors err2, errorMsg = mergeErrors (errorMsg err1) (errorMsg err2) }

mergeMDErrors :: [Forest_err] -> Forest_err
mergeMDErrors = foldl' mergeForestErrs cleanForestMDErr

--mergeForestMDErrors :: (FSRep fs) => [Forest_md] -> ForestO fs Forest_err
--mergeForestMDErrors = foldM (\err fmd -> get_errors fmd >>= \err1 -> return $ mergeForestErrs err1 err) cleanForestMDErr

updateForestErr :: Forest_err -> [Forest_err] -> Forest_err
updateForestErr err errs = foldl' mergeForestErrs err errs

updateForestMDAttributes f info = f { fileInfo = info }

addMultipleMatchesErrorMD :: (ForestMD md) => FilePath -> [String] -> md -> md
addMultipleMatchesErrorMD path names md = replace_errors md $ \olderrors ->
	let errMsg' = case errorMsg olderrors of
		Nothing -> MultipleMatches path names
		Just e ->  e
	in Forest_err { numErrors = numErrors olderrors + 1, errorMsg = Just errMsg' }

-- adds new errors to a @Forest_md@
updateForestMDErrorsWith :: ForestMD md => md -> [Forest_err] -> md
updateForestMDErrorsWith md errs = replace_errors md $ \err0 -> updateForestErr err0 errs

updateForestMDErrorsWithPadsMD :: (PadsMD pads_md,ForestMD md) => md -> pads_md -> md
updateForestMDErrorsWithPadsMD md pmd = replace_errors md $ \err0 -> updateForestErr err0 [padsError $ get_md_header pmd]

-- replaces the original errors of a @Forest_md@
replaceForestMDErrorsInsideWith :: ForestMD md => md -> [Forest_err] -> md
replaceForestMDErrorsInsideWith md errs = replace_errors md $ \err0 -> mergeMDErrors errs

specialStringToMode special = case special of
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
	groupEntries <- getAllGroupEntries 
	let id_groups = filter (\g -> id `elem` (groupMembers g)) groupEntries
	return (Data.List.map groupName id_groups)

mergeJustDefault :: (Data a,Data b) => (Maybe a,Maybe b) -> (a,b)
mergeJustDefault (Just x,Just y) = (x,y)
mergeJustDefault (Just x,Nothing) = (x,forestdefault)
mergeJustDefault (Nothing,Just y) = (forestdefault,y)
mergeJustDefault (Nothing,Nothing) = (forestdefault,forestdefault)

forestdefault ::  GenericB
forestdefault = genericB forestdefault'

genericB :: (forall a. Data a => a -> a) -> GenericB
genericB gen = gen (error "genericB")

forestdefault' :: (forall a. (Data a) => a -> a)
forestdefault' a = ext1B (ext2B (general a
		`extB` char 
		`extB` int
		`extB` integer
		`extB` float 
		`extB` double 
		`extB` coff
		`extB` epochTime
		`extB` fileMode
		`extB` byteString)
		map)
		(list) where
	-- Generic case (does not guarantee termination for recursive types)
	general :: (Data a) => a -> a
	general proxy = let d = dataTypeOf proxy in fromConstrB (forestdefault' (error "general")) (indexConstr d 1)
	
	-- Base cases
	char    = '\NUL'
	int     = 0      :: Int
	integer = 0      :: Integer
	float   = 0.0    :: Float
	double  = 0.0    :: Double
	coff    = 0      :: COff
	epochTime = 0    :: EpochTime
	fileMode = 0     :: FileMode
	byteString = B.empty     :: ByteString
	list :: Data b => [b]
	list   = []
	map :: (Map.Map k v)
	map = Map.empty
	set :: (Set.Set v)
	set = Set.empty

noPath = ""

-- forest argument pairs
fstStar (a :*: b) = a
sndStar (a :*: b) = b
infixr 5 :*:
data a :*: b = a :*: b deriving (Typeable,Eq,Show,Ord) -- the forest datatype for pairs of arguments
newtype Arg a = Arg a deriving Typeable

defaultForest_mdWithErrors :: (FSRep fs) => Forest_err -> ForestM fs Forest_md
defaultForest_mdWithErrors err = return $ Forest_md err forestdefault

getForestMDInTree :: (FSRep fs) => FilePath -> FSTree fs -> ForestM fs (Forest_md)
getForestMDInTree path tree = pathInTree path tree >>= getForestMD path

getRelForestMDInTree :: (FSRep fs) => FilePath -> FSTree fs -> FilePath -> ForestM fs (Forest_md)
getRelForestMDInTree path tree file = getForestMDInTree (path </> file) tree

