{-# LANGUAGE NamedFieldPuns, RecordWildCards, FlexibleInstances, DeriveDataTypeable, TemplateHaskell,ScopedTypeVariables, 
    TypeSynonymInstances #-}
module Language.Forest.MetaData where

import System.Posix.Files
import System.Posix.User
import System.Posix.Types
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



data FileType = UnknownK | SimpleK | DirectoryK 
 deriving (Eq, Show, Data, Typeable)

(derive makeDataAbstract ''COff)
(derive makeDataAbstract ''CMode)
(derive makeDataAbstract ''CTime)

data FileInfo = FileInfo { owner :: String
                         , group :: String
                         , size  :: COff
                         , access_time :: EpochTime
                         , mod_time :: EpochTime
                         , read_time :: EpochTime
                         , mode :: FileMode
                         , isSymLink :: Bool
                         , kind :: FileType
                         }
       deriving (Eq, Show, Data, Typeable)

{- Function System.Time.Utils.epochToClockTime converts EpochTime to CalendarTime -}


-- instance Pretty UTCTime where
-- ppr = text . show

instance Pretty COff where
 ppr = text . show 

instance Pretty EpochTime where
 ppr = text . show . epochToClockTime 

instance Pretty FileMode where
 ppr = text . show

--instance Pretty FileType where
-- ppr = text . show

fileInfo_def = FileInfo { owner = ""
                        , group = ""
                        , size = 0
                        , access_time = 0
                        , read_time = 0
                        , mod_time = 0
                        , mode = 0
                        , isSymLink = False
                        , kind = UnknownK       
                        }

errorFileInfo :: FileInfo
errorFileInfo  = FileInfo 
     { owner = ""
     , group = ""
     , size  = -1
     , access_time = -1
     , read_time = -1
     , mod_time = -1
     , mode     = -1
     , isSymLink = False
     , kind = UnknownK
     }


data Forest_md = Forest_md { numErrors :: Int
                           , errorMsg  :: Maybe ErrMsg
                           , fileInfo  :: FileInfo 
                           }
   deriving (Typeable, Data, Eq, Show)

forest_md_def = Forest_md{ numErrors = 1
                         , errorMsg = Nothing
                         , fileInfo = fileInfo_def
                         }


get_owner x = owner $ fileInfo $ fst x
get_group x = group $ fileInfo $ fst x
get_size  x = size $ fileInfo $ fst x
get_access_time  x = access_time $ fileInfo $ fst x
get_read_time  x = read_time $ fileInfo $ fst x
get_mod_time  x = mod_time $ fileInfo $ fst x
get_mode  x = mode $ fileInfo $ fst x
get_sym  x = isSymLink $ fileInfo $ fst x
get_kind  x = kind $ fileInfo $ fst x


{- Meta data type class -}
class Data md => ForestMD md where
  get_fmd_header :: md -> Forest_md
  replace_fmd_header :: md -> Forest_md -> md

instance ForestMD Forest_md where
  get_fmd_header b = b
  replace_fmd_header old new = new

instance Data b => ForestMD (Forest_md,b) where
  get_fmd_header (h,b) = h
  replace_fmd_header (h1,b) h2 = (h2,b)



cleanForestMD = Forest_md {numErrors = 0, errorMsg = Nothing, fileInfo = errorFileInfo}
errorForestMD = Forest_md {numErrors = 1, errorMsg = Nothing, fileInfo = errorFileInfo}
missingPathForestMD path = Forest_md {numErrors = 1, errorMsg = Just (MissingFile path), fileInfo = errorFileInfo}
systemErrorForestMD i = Forest_md {numErrors = 1, errorMsg = Just (SystemError i), fileInfo = errorFileInfo}

mergeErrors m1 m2 = case (m1,m2) of
            (Nothing,Nothing) -> Nothing
            (Just a, _) -> Just a
            (_, Just b) -> Just b


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

fileStatusToKind fs = if isRegularFile fs then SimpleK 
                      else if isDirectory fs then DirectoryK
                      else UnknownK

{- Should raise no exceptions -}
getForestMD :: FilePath -> IO Forest_md
getForestMD path = do
    fdEither <- CE.try (getFileStatus path)
    case fdEither of
        Left (e::CE.SomeException) -> return
                (Forest_md { numErrors = 1
                           , errorMsg = Just (ForestIOException (show e))
                           , fileInfo = errorFileInfo
                           })
        Right fd -> do 
         { let file_ownerID = fileOwner fd
         ; ownerEntry    <- getUserEntryForID file_ownerID
         ; let file_groupID = fileGroup fd
         ; groupEntry <- getGroupEntryForID file_groupID
         ; fdsym <- getSymbolicLinkStatus path
         ; readTime <- epochTime
         ; return (Forest_md{ numErrors = 0
                            , errorMsg = Nothing
                            , fileInfo = FileInfo 
                                          { owner = userName ownerEntry
                                          , group = groupName groupEntry
                                          , size  = fileSize fd
                                          , access_time = accessTime fd
                                          , mod_time = modificationTime fd
                                          , read_time = readTime
                                          , mode     =  fileMode fd
                                          , isSymLink = isSymbolicLink fdsym
                                          , kind  = fileStatusToKind fd
                                          }
                            })
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

