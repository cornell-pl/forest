{-# LANGUAGE NamedFieldPuns, RecordWildCards, FlexibleInstances, DeriveDataTypeable, TemplateHaskell,ScopedTypeVariables #-}
module Language.Forest.MetaData where

import System.Posix.Files
import System.Posix.User
import System.Posix.Types
import qualified Control.Exception as CE

import Data.Data
import Data.List hiding (group)

import Data.DeriveTH                 -- Library for deriving instances for existing types

import Text.PrettyPrint.Mainland as PP hiding (group)

import Language.Forest.Errors

{- Base type library support -}

-- (derive makeDataAbstract ''COff)
-- (derive makeDataAbstract ''EpochTime)
-- (derive makeDataAbstract ''FileMode)

data FileType = SimpleK | DirectoryK | SymLinkK | UnknownK
 deriving (Eq, Show, Data, Typeable)

data FileInfo = FileInfo { owner :: String
                         , group :: String
                         , size  :: COff
                         , access_time :: EpochTime
                         , mod_time :: EpochTime
                         , mode :: FileMode
                         , kind :: FileType
                         }
       deriving (Eq, Show)

{- Function System.Time.Utils.epochToClockTime converts EpochTime to CalendarTime -}

(derive makeTypeable     ''FileInfo)
(derive makeDataAbstract ''FileInfo)

data Forest_md = Forest_md { numErrors :: Int
                           , errorMsg  :: Maybe ErrMsg
                           , fileInfo  :: FileInfo 
                           }
   deriving (Typeable, Data, Eq, Show)

get_owner x = owner $ fileInfo $ fst x
get_group x = group $ fileInfo $ fst x
get_size  x = size $ fileInfo $ fst x
get_access_time  x = access_time $ fileInfo $ fst x
get_mod_time  x = mod_time $ fileInfo $ fst x
get_mode  x = mode $ fileInfo $ fst x
get_kind  x = kind $ fileInfo $ fst x


{- Meta data type class -}
class ForestMD md where
  get_fmd_header :: md -> Forest_md
  replace_fmd_header :: md -> Forest_md -> md

instance ForestMD Forest_md where
  get_fmd_header b = b
  replace_fmd_header old new = new

instance ForestMD (Forest_md,b) where
  get_fmd_header (h,b) = h
  replace_fmd_header (h1,b) h2 = (h2,b)

{-
pprForestMD Forest_md {numErrors=num} = text "Errors:" <+> PP.ppr num 

instance Pretty Forest_md where
  ppr = pprForestMD 
-}

cleanForestMD = Forest_md {numErrors = 0, errorMsg = Nothing, fileInfo = errorFileInfo}
errorForestMD = Forest_md {numErrors = 1, errorMsg = Nothing, fileInfo = errorFileInfo}
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
                      else if isSymbolicLink fs then SymLinkK
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
         ; return (Forest_md{ numErrors = 0
                            , errorMsg = Nothing
                            , fileInfo = FileInfo 
                                          { owner = userName ownerEntry
                                          , group = groupName groupEntry
                                          , size  = fileSize fd
                                          , access_time = accessTime fd
                                          , mod_time = modificationTime fd
                                          , mode     =  fileMode fd
                                          , kind  = fileStatusToKind fd
                                          }
                            })
         }


errorFileInfo :: FileInfo
errorFileInfo  = FileInfo 
     { owner = ""
     , group = ""
     , size  = -1
     , access_time = -1
     , mod_time = -1
     , mode     = -1
     , kind = UnknownK
     }