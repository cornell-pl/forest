{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, UndecidableInstances, ConstraintKinds, FlexibleContexts, MultiParamTypeClasses, RankNTypes, NamedFieldPuns, RecordWildCards, FlexibleInstances, DeriveDataTypeable, TemplateHaskell,ScopedTypeVariables, DoAndIfThenElse,
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

module Language.Forest.MetaData where

import System.Mem.StableName
import System.Mem.Weak as Weak
import System.Mem.WeakTable as WeakTable
import System.Mem.MemoTable
import Data.Word
import Data.Int
import Control.Monad.Incremental
import Language.Pads.Instances
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

import System.Mem.WeakRef

import qualified Control.Exception as CE

import Data.Data
import Data.WithClass.MData
import Data.WithClass.MGenerics.Aliases
import Data.DeriveTH                 -- Library for deriving instances for existing types
import Data.WithClass.Derive.MData
import Data.WithClass.Derive.DeepTypeable
import Data.DeepTypeable
import Language.Haskell.TH.Syntax
--import Data.Derive.MDataForest

import Data.List hiding (group, unzip, zip)

import Text.PrettyPrint.Mainland as PP hiding (group, empty)

import Language.Pads.Source
import Language.Pads.RegExp 
import Language.Pads.Generic hiding (ext1B,ext2B)
import Language.Pads.GenPretty
import Language.Forest.Errors
import Language.Forest.FS.FSRep
import System.IO.Unsafe



-- arguments of Forest specifications
data Forest_args args = Forest_args { arguments :: args }
	deriving (Eq,Typeable,Ord)

$( derive makeMData ''Forest_args )
$( derive makeDeepTypeable ''Forest_args )

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
doesExistInMD :: FilePath -> Forest_md fs -> Bool
doesExistInMD path fmd = (fullpath (fileInfo fmd) == path) && (access_time (fileInfo fmd) > 0 || read_time (fileInfo fmd) > 0)

doesFileExistInMD :: FilePath -> Forest_md fs -> Bool
doesFileExistInMD path fmd = doesExistInMD path fmd && kind (fileInfo fmd) /= DirectoryK

doesDirectoryExistInMD :: FilePath -> Forest_md fs -> Bool
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

forest_md_def :: (ForestInput fs FSThunk Inside,ForestLayer fs l) => ForestL fs l (Forest_md fs)
forest_md_def = do
	err <- inside $ fsthunk [] $ return forest_err_def
	return $ Forest_md err fileInfo_def
forest_err_def :: Forest_err
forest_err_def = Forest_err 1 Nothing

fmd_fullpath :: Forest_md fs -> FilePath
fmd_fullpath = fullpath . fileInfo

fmd_kind :: Forest_md fs -> FileType
fmd_kind = kind . fileInfo

fmd_symLink :: Forest_md fs -> Maybe FilePath
fmd_symLink = symLink . fileInfo

-- we need to actually create new thunks to allow the modification to occur at the inner layer
modify_errors_under :: (Eq md,ForestMD fs md) => ForestFSThunk fs Inside md -> (Forest_err -> ForestI fs Forest_err) -> ForestO fs ()
modify_errors_under t newerrs = fsmodify [] t $ \md -> replace_errors md newerrs

{- Meta data type class -}
class (FSRep fs,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => ForestMD fs md where
	isUnevaluatedMDThunk :: ForestLayer fs l => md -> ForestL fs l Bool
	isUnforcedMDThunk :: ForestLayer fs l => md -> ForestL fs l Bool
	get_fmd_header :: ForestLayer fs l => md -> ForestL fs l (Forest_md fs)
	replace_fmd_header :: md -> (Forest_md fs -> ForestI fs (Forest_md fs)) -> ForestI fs md
	
	get_errors_thunk :: ForestLayer fs l => md -> ForestL fs l (ForestFSThunk fs Inside Forest_err)
	get_errors_thunk md = do
		fmd <- get_fmd_header md
		return $ errors fmd
	get_errors :: ForestLayer fs l => md -> ForestL fs l Forest_err
	get_errors md = do
		fmd <- get_fmd_header md
		inside $ fsforce $ errors fmd
	isValidMD :: ForestLayer fs l => md -> ForestL fs l Bool
	isValidMD md = do
		err <- get_errors md
		return $ numErrors err == 0
	replace_errors :: md -> (Forest_err -> ForestI fs Forest_err) -> ForestI fs md
	replace_errors md f = do
		replace_fmd_header md $ \fmd -> do
			t <- fsThunk [] $ f =<< get_errors fmd
			return $ fmd { errors = t }
	modify_errors :: md -> (Forest_err -> ForestI fs Forest_err) -> ForestO fs ()
	modify_errors md newerrs = do
		fmd <- get_fmd_header md
		let err_thunk = errors fmd
		fsmodify [] err_thunk newerrs
	overwrite_errors :: md -> ForestI fs Forest_err -> ForestO fs ()
	overwrite_errors md newerrs = do
		fmd <- get_fmd_header md
		let err_thunk = errors fmd
		fsoverwrite [] err_thunk newerrs
		
	get_fileInfo :: ForestLayer fs l => md -> ForestL fs l FileInfo
	get_fileInfo md = liftM fileInfo (get_fmd_header md)
	get_fullpath :: ForestLayer fs l => md -> ForestL fs l String
	get_fullpath md = liftM (fullpath . fileInfo) (get_fmd_header md)
	get_owner :: ForestLayer fs l => md -> ForestL fs l String
	get_owner md = liftM (owner . fileInfo) (get_fmd_header md)
	get_group :: ForestLayer fs l => md -> ForestL fs l String
	get_group md = liftM (group . fileInfo) (get_fmd_header md)
	get_size :: ForestLayer fs l => md -> ForestL fs l COff
	get_size md = liftM (size . fileInfo) (get_fmd_header md)
	get_access_time :: ForestLayer fs l => md -> ForestL fs l EpochTime
	get_access_time md = liftM (access_time . fileInfo) (get_fmd_header md)
	get_mod_time :: ForestLayer fs l => md -> ForestL fs l EpochTime
	get_mod_time md = liftM (mod_time . fileInfo) (get_fmd_header md)
	get_read_time :: ForestLayer fs l => md -> ForestL fs l EpochTime
	get_read_time md = liftM (read_time . fileInfo) (get_fmd_header md)
	get_mode :: ForestLayer fs l => md -> ForestL fs l FileMode
	get_mode md = liftM (mode . fileInfo) (get_fmd_header md)
	get_modes :: ForestLayer fs l => md -> ForestL fs l String
	get_modes md = liftM (modeToModeString . mode . fileInfo) (get_fmd_header md)
	get_symLink :: ForestLayer fs l => md -> ForestL fs l (Maybe FilePath)
	get_symLink md = liftM (symLink . fileInfo) (get_fmd_header md)
	get_kind :: ForestLayer fs l => md -> ForestL fs l FileType
	get_kind md = liftM (kind . fileInfo) (get_fmd_header md)

change_fmd_header :: (Eq md,FSRep fs,ForestInput fs FSThunk Inside,ForestInput fs FSThunk l) => ForestFSThunk fs l (Forest_md fs,md) -> Forest_md fs -> ForestO fs ()
change_fmd_header t fmd' = fsmodify [] t $ \(fmd,md) -> return (fmd',md)

instance (FSRep fs,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => ForestMD fs (Forest_md fs) where
	isUnevaluatedMDThunk fmd = return False
	isUnforcedMDThunk fmd = return False
	get_fmd_header b = return b
	replace_fmd_header fmd f = f fmd

-- the left side may be a @Forest_md@ and the right side a metadata value
instance (FSRep fs,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => ForestMD fs (Forest_md fs,b) where
	isUnevaluatedMDThunk (fmd,md) = error "isUnevaluatedMDThunk should never be called for a Forest_md"
	isUnforcedMDThunk (fmd,md) = error "isUnforcedMDThunk should never be called for a Forest_md"
	get_fmd_header (a,b) = get_fmd_header a
	replace_fmd_header (fmd,b) f = replace_fmd_header fmd f >>= \fmd' -> return (fmd',b)
-- or the left side may be a metadata thunk and the right side a sequence of argument thunks
instance (ForestMD fs (ForestFSThunk fs l a),Eq a,MData NoCtx (ForestL fs l) b,ForestMD fs a) => ForestMD fs (ForestFSThunk fs l a,b) where
	isUnevaluatedMDThunk (t,args) = isUnevaluatedMDThunk t
	isUnforcedMDThunk (t,args) = isUnforcedMDThunk t
	get_fmd_header (a,b) = get_fmd_header a
	replace_fmd_header (t,b) f = replace_fmd_header t f >>= \t' -> return (t',b)
-- when load arguments and Adapton thunks are all mixed together
instance (ForestMD fs a) => ForestMD fs ((a,b),c) where
	isUnevaluatedMDThunk ((a,b),c) = isUnevaluatedMDThunk a
	isUnforcedMDThunk ((a,b),c) = isUnforcedMDThunk a
	get_fmd_header ((a,b),c) = get_fmd_header a
	replace_fmd_header ((a,b),c) f = replace_fmd_header a f >>= \a' -> return ((a',b),c)

instance (Eq a,ForestMD fs a,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => ForestMD fs (ForestFSThunk fs Inside a) where
	isUnevaluatedMDThunk t = inside $ isUnevaluatedFSThunk t
	isUnforcedMDThunk t = inside $ isUnforcedFSThunk t
	get_fmd_header t = do
		md <- inside (fsforce t)
		get_fmd_header md
	replace_fmd_header t f = fsthunk [] $ fsforce t >>= \v -> replace_fmd_header v f

-- replaces the content of a stable metadata value with the content of another one
class FSRep fs => StableMD fs md where
	overwriteMD :: [FSTree fs] -> md -> ForestI fs md -> ForestO fs ()

instance (FSRep fs,Eq a,ForestInput fs FSThunk Inside) => StableMD fs (ForestFSThunk fs Inside a) where
	overwriteMD ts t m = fsoverwrite ts t $ fsforce =<< m
instance (Eq a,Eq b,StableMD fs a,StableMD fs b,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => StableMD fs (a,b) where
	overwriteMD ts (t1,t2) m = do
		load <- inside $ fsThunk ts m 
		overwriteMD ts t1 $ liftM fst $ fsforce load
		overwriteMD ts t2 $ liftM snd $ fsforce load
instance (Eq a,Eq b,StableMD fs a,StableMD fs b,ForestInput fs FSThunk Inside,ForestLayer fs Outside) => StableMD fs (a :*: b) where
	overwriteMD ts (t1 :*: t2) m = do
		load <- inside $ fsThunk ts m
		overwriteMD ts t1 $ liftM fstStar $ fsforce load
		overwriteMD ts t2 $ liftM sndStar $ fsforce load

cleanForestMD :: (ForestInput fs FSThunk Inside) => ForestI fs (Forest_md fs)
cleanForestMD = do
	err <- fsthunk [] $ return cleanForestMDErr
	return $ Forest_md { errors = err, fileInfo = fileInfo_def}
cleanForestMDErr = Forest_err {numErrors = 0, errorMsg = Nothing }

errorForestMD :: (ForestInput fs FSThunk Inside) => ForestI fs (Forest_md fs)
errorForestMD = do
	err <- fsthunk [] $ return errorForestMDErr
	return $ Forest_md { errors = err, fileInfo = errorFileInfo}
errorForestMDErr = Forest_err {numErrors = 1, errorMsg = Nothing }
missingPathForestMD path = do
	err <- fsthunk [] $ return $ missingPathForestErr path
	return $ Forest_md { errors = err, fileInfo = mkErrorFileInfo path}
missingPathForestErr path = Forest_err { numErrors = 1, errorMsg = Just (MissingFile path) }
wrongFileExtensionForestErr ext path = Forest_err { numErrors = 1, errorMsg = Just (WrongFileExtension ext path) }
missingDirForestMD path = do
	err <- fsthunk [] $ return $ missingDirForestErr path
	return $ Forest_md {errors = err, fileInfo = mkErrorFileInfo path}
missingDirForestErr path = Forest_err { numErrors = 1, errorMsg = Just (MissingDirectory path) }
fileMatchFailureForestMD path = do
	err <- fsthunk [] $ return $ fileMatchFailureForestErr path
	return $ Forest_md {errors = err, fileInfo = mkErrorFileInfo path}
fileMatchFailureForestErr path = Forest_err { numErrors = 1, errorMsg = Just (MatchFailure path) }
systemErrorForestMD i = do
	err <- fsthunk [] $ return $ systemErrorForestErr i
	return $ Forest_md {errors = err, fileInfo = errorFileInfo}
systemErrorForestErr i = Forest_err { numErrors = 1, errorMsg = Just (SystemError i) }
notDirectoryForestMD path = do
	err <- fsthunk [] $ return $ notDirectoryForestErr path
	return $ Forest_md {errors = err, fileInfo = mkErrorFileInfo path}
notDirectoryForestErr path = Forest_err { numErrors = 1, errorMsg = Just (NotADirectory path) }
constraintViolationForestMD :: (ForestInput fs FSThunk Inside) => ForestI fs (Forest_md fs)
constraintViolationForestMD = do
	err <- fsthunk [] $ return constraintViolationForestErr
	return $ Forest_md {errors = err, fileInfo = errorFileInfo}
constraintViolationForestErr = Forest_err { numErrors = 1, errorMsg = Just ConstraintViolation }
ioExceptionForestMD :: (ForestInput fs FSThunk Inside) => ForestI fs (Forest_md fs)
ioExceptionForestMD = do
	err <- fsthunk [] $ return ioExceptionForestErr
	return $ Forest_md { errors = err, fileInfo = errorFileInfo }
ioExceptionForestErr = Forest_err { numErrors = 1, errorMsg = Just (ForestIOException $ "not a symbolic link") }

mergeErrors m1 m2 = case (m1,m2) of
            (Nothing,Nothing) -> Nothing
            (Just a, _) -> Just a
            (_, Just b) -> Just b

cleanForestMDwithFile :: (ForestInput fs FSThunk Inside) => FilePath -> ForestI fs (Forest_md fs)
cleanForestMDwithFile path = do
	fmd <- cleanForestMD
	return $ fmd { fileInfo = (fileInfo fmd) { fullpath = path } }

--removeGzipSuffix :: FSRep fs => Forest_md -> ForestO fs Forest_md
--removeGzipSuffix fmd = do
--	fp <- get_fullpath fmd
--	let fp' = System.FilePath.Posix.dropExtension fp
--	return $ setFilePathinMD fp' fmd
--
--setFilePathinMD :: FilePath -> Forest_md -> Forest_md
--setFilePathinMD path (Forest_md {numErrors, errorMsg, fileInfo}) = 
--                Forest_md {numErrors, errorMsg, fileInfo = setFilePathinFileInfo path fileInfo}
--
--setFilePathinFileInfo :: FilePath -> FileInfo -> FileInfo
--setFilePathinFileInfo path 
--               (FileInfo { fullpath 
--                         , owner
--                         , group
--                         , size
--                         , access_time
--                         , mod_time
--                         , read_time
--                         , mode 
--                         , symLink
--                         , kind
--                         }) = 
--                FileInfo { fullpath = path 
--                         , owner
--                         , group
--                         , size
--                         , access_time
--                         , mod_time
--                         , read_time
--                         , mode 
--                         , symLink
--                         , kind
--                         }  

-- | Tests if two metadata values are both valid or invalid
sameValidity :: (ForestLayer fs l,ForestMD fs md1,ForestMD fs md2) => md1 -> md2 -> ForestL fs l Bool
sameValidity md1 md2 = do
	err1 <- get_errors md1
	err2 <- get_errors md2
	return $ (numErrors err1 == 0) == (numErrors err2 == 0)

sameValidity' :: (ForestLayer fs l,ForestMD fs md1) => md1 -> Forest_err -> ForestL fs l Bool
sameValidity' md1 err2 = do
	err1 <- get_errors md1
	return $ (numErrors err1 == 0) == (numErrors err2 == 0)

-- | Tests if two metadata values point to the same filepath
sameFullPath :: (ForestLayer fs l,ForestMD fs md1,ForestMD fs md2) => md1 -> md2 -> ForestL fs l Bool
sameFullPath md1 md2 = do
	fmd1 <- get_fmd_header md1
	fmd2 <- get_fmd_header md2
	return $ fullpath (fileInfo fmd1) == fullpath (fileInfo fmd2)

-- | Tests if two metadata values point to the same canonical filepath, in respect to a given tree
sameCanonicalFullPathInTree :: (ForestLayer fs l,ForestMD fs md1,ForestMD fs md2) => md1 -> md2 -> FSTree fs -> ForestL fs l Bool
sameCanonicalFullPathInTree md1 md2 tree = do
	fmd1 <- get_fmd_header md1
	fmd2 <- get_fmd_header md2
	let path1 = fullpath (fileInfo fmd1)
	let path2 = fullpath (fileInfo fmd2)
	canpath1 <- canonalizePathWithTree path1 tree
	canpath2 <- canonalizePathWithTree path2 tree
	return $ canpath1 == canpath2

mergeForestErrs :: Forest_err -> Forest_err -> Forest_err
mergeForestErrs err1 err2 = Forest_err { numErrors = numErrors err1 + numErrors err2, errorMsg = mergeErrors (errorMsg err1) (errorMsg err2) }

mergeMDErrors :: [Forest_err] -> Forest_err
mergeMDErrors = foldl' mergeForestErrs cleanForestMDErr

--mergeForestMDErrors :: (FSRep fs) => [Forest_md fs] -> ForestO fs Forest_err
--mergeForestMDErrors = foldM (\err fmd -> get_errors fmd >>= \err1 -> return $ mergeForestErrs err1 err) cleanForestMDErr

updateForestErr :: Forest_err -> [Forest_err] -> Forest_err
updateForestErr err errs = foldl' mergeForestErrs err errs

updateForestMDAttributes f info = f { fileInfo = info }

--addPredFailureMD :: Forest_md -> Forest_md
--addPredFailureMD (Forest_md{numErrors, errorMsg, fileInfo}) = 
--  let errMsg' = case errorMsg of
--                  Nothing -> ForestPredicateFailure
--                  Just e ->  e
--  in Forest_md{numErrors = numErrors + 1, errorMsg = Just errMsg', fileInfo = fileInfo}
--
addMultipleMatchesErrorMD :: (ForestMD fs md) => FilePath -> [String] -> md -> ForestO fs ()
addMultipleMatchesErrorMD path names md = modify_errors md $ \olderrors -> do
	let errMsg' = case errorMsg olderrors of
		Nothing -> MultipleMatches path names
		Just e ->  e
	return $ Forest_err { numErrors = numErrors olderrors + 1, errorMsg = Just errMsg' }

addMultipleMatchesErrorMDInside :: (ForestMD fs md) => FilePath -> [String] -> md -> ForestI fs md
addMultipleMatchesErrorMDInside path names md = replace_errors md $ \olderrors -> do
	let errMsg' = case errorMsg olderrors of
		Nothing -> MultipleMatches path names
		Just e ->  e
	return $ Forest_err { numErrors = numErrors olderrors + 1, errorMsg = Just errMsg' }

--
--revalidateForestMDwith :: Forest_md -> [Forest_md] -> Forest_md
--revalidateForestMDwith base updates =  let
--       (Forest_md {numErrors=numd, errorMsg = errorMsgd, fileInfo=infod}) = base
--       (Forest_md {numErrors=numc, errorMsg = errorMsgc, fileInfo=infoc}) = mergeForestMDs updates
--   in  (Forest_md {numErrors=numc 
--                  , errorMsg = errorMsgc
--                  , fileInfo=infod})
--
--updateForestMDwith :: Forest_md fs -> [Forest_md fs] -> Forest_md fs
--updateForestMDwith base updates =  let
--       (Forest_md {numErrors=numd, errorMsg = errorMsgd, fileInfo=infod}) = base
--       (Forest_md {numErrors=numc, errorMsg = errorMsgc, fileInfo=infoc}) = mergeForestMDs updates
--   in  (Forest_md {numErrors=numd + numc 
--                  , errorMsg = mergeErrors errorMsgd errorMsgc
--                  , fileInfo=infod})

--updateForestMDWith :: FSRep fs => Forest_md fs -> ForestO fs [Forest_md fs] -> ForestO fs ()
--updateForestMDWith fmd get_fmds = updateForestMDErrorsWith fmd $ mapM get_errors =<< get_fmds

updateForestMDErrorsWith :: ForestMD fs md => md -> ForestI fs [Forest_err] -> ForestO fs ()
updateForestMDErrorsWith md get_errs = modify_errors md $ \err0 -> get_errs >>= \errs -> return $ updateForestErr err0 errs

updateForestMDErrorsInsideWith :: ForestMD fs md => md -> ForestI fs [Forest_err] -> ForestI fs md
updateForestMDErrorsInsideWith md get_errs = replace_errors md $ \err0 -> get_errs >>= \errs -> return $ updateForestErr err0 errs

replaceForestMDErrorsWith :: ForestMD fs md => md -> ForestI fs [Forest_err] -> ForestO fs ()
replaceForestMDErrorsWith md get_errs = overwrite_errors md $ do
	errs <- get_errs
	return $ mergeMDErrors errs

replaceForestMDErrorsInsideWith :: ForestMD fs md => md -> ForestI fs [Forest_err] -> ForestI fs md
replaceForestMDErrorsInsideWith md get_errs = replace_errors md $ \err0 -> get_errs >>= return . mergeMDErrors

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

mdefault :: (FSRep fs,ForestLayer fs l) => GenericB NoCtx (ForestL fs l)
mdefault = genericB mdefault'

genericB :: (forall a. MData NoCtx m a => Proxy NoCtx -> a -> m a) -> GenericB NoCtx m
genericB gen = gen (Proxy::Proxy NoCtx) (error "genericB")

mdefault' :: FSRep fs => (forall a. (FSRep fs,MData ctx (ForestL fs l) a) => Proxy ctx -> a -> (ForestL fs l) a)
mdefault' ctx a = ext1B ctx (ext2B ctx (general ctx a
		`extB` char 
		`extB` int
		`extB` integer
		`extB` float 
		`extB` double 
		`extB` coff
		`extB` epochTime
		`extB` fileMode
		`extB` byteString)
--		`extB` (thunkFS a)
		map)
		(list ctx) where
	-- Generic case (does not guarantee termination for recursive types)
	general :: (FSRep fs,MData ctx (ForestL fs l) a) => Proxy ctx -> a -> ForestL fs l a
	general ctx proxy = Data.WithClass.MData.dataTypeOf ctx proxy >>= \d -> Data.WithClass.MData.fromConstrB ctx (mdefault' ctx (error "general")) (Data.WithClass.MData.indexConstr d 1)
	
	-- Base cases
	char    = return '\NUL'
	int     = return 0      :: Monad m => m Int
	integer = return 0      :: Monad m => m Integer
	float   = return 0.0    :: Monad m => m Float
	double  = return 0.0    :: Monad m => m Double
	coff    = return 0      :: Monad m => m COff
	epochTime = return 0    :: Monad m => m EpochTime
	fileMode = return 0     :: Monad m => m FileMode
	byteString = return B.empty     :: Monad m => m ByteString
	list :: MData ctx m b => Proxy ctx -> m [b]
	list ctx   = return []
	map :: Monad m => m (Map.Map k v)
	map = return Map.empty
--	thunkFS :: (FSRep fs,MData (ForestO fs) a) => a -> ForestO fs (FSThunk fs a)
--	thunkFS proxy = do
--		a <- mdefault' proxy
--		fsthunk [] $ return a
		
instance (FSRep fs,MData ctx m FileInfo,MData ctx m (ForestFSThunkI fs Forest_err),Sat (ctx (Forest_md fs)))
	=> MData ctx m (Forest_md fs) where
	gfoldl ctx k z (Forest_md x1 x2) = z (\mx1 -> return $ \mx2 -> mx1 >>= \x1 -> mx2 >>= \x2 -> return $ Forest_md x1 x2) >>= flip k (return x1) >>= flip k (return x2)
	gunfold ctx k z c = z (\mx1 -> return $ \mx2 -> mx1 >>= \x1 -> mx2 >>= \x2 -> return $ Forest_md x1 x2) >>= k >>= k
	toConstr ctx x@(Forest_md x1 x2) = Data.WithClass.MData.dataTypeOf ctx x >>= return . flip indexConstr 1
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Language.Forest.MetaData.Forest_md" [mkConstr ty "Forest_md" [] Prefix]

noPath = ""

fstStar (a :*: b) = a
sndStar (a :*: b) = b
infixr 5 :*:
data a :*: b = a :*: b deriving (Typeable,Eq,Show,Ord) -- the forest datatype for pairs of arguments
$( derive makeMData ''(:*:) )
$( derive makeDeepTypeable ''(:*:) )
newtype Arg a = Arg a

defaultForest_mdWithErrors :: (FSRep fs,ForestLayer fs l) => Forest_err -> ForestL fs l (Forest_md fs)
defaultForest_mdWithErrors err = do
	errors <- inside $ fsref err
	info <- mdefault
	return $ Forest_md errors info

instance Memo (Forest_md fs) where
	type Key (Forest_md fs) = StableName (Forest_md fs)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo FileInfo where
	type Key FileInfo = StableName FileInfo
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)
	
instance Memo FileType where
	type Key FileType = StableName FileType
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo ErrMsg where
	type Key ErrMsg = StableName ErrMsg
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo CMode where
	type Key CMode = StableName CMode
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo CTime where
	type Key CTime = StableName CTime
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo COff where
	type Key COff = StableName COff
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo Forest_err where
	type Key Forest_err = StableName Forest_err
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance (Memo a,Memo b) => Memo (a :*: b) where
	type Key (a :*: b) = (Key a,Key b)
	{-# INLINE memoKey #-}
	memoKey (x :*: y) = (andMkWeak w1 w2,(k1,k2))
		where (w1,k1) = memoKey x
		      (w2,k2) = memoKey y
