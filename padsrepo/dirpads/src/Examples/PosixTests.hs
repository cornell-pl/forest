{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}

module Examples.PosixTests where
import System.Posix.Files
import System.Posix.User
import System.Posix.Types
import System.IO.Unsafe (unsafePerformIO)

file_path = "/Users/kfisher/pads/dirpads/src/Examples/PosixTests.hs"

test :: IO (String)
test = do b <- fileExist file_path
          if b then return "File exists" else return "No file"

data FileInfo = FileInfo { owner :: String
                         , group :: String
                         , size  :: COff
                         , access_time :: EpochTime
                         , mod_time :: EpochTime
                         , mode :: FileMode
                         }
       deriving (Eq, Show)

getMetaData :: FilePath -> IO FileInfo
getMetaData path = do 
  { fd <- getFileStatus file_path
  ; let file_size = fileSize fd
  ; let file_ownerID = fileOwner fd
  ; ownerEntry    <- getUserEntryForID file_ownerID
  ; let file_owner_name = userName ownerEntry
  ; let file_groupID = fileGroup fd
  ; groupEntry <- getGroupEntryForID file_groupID
  ; let file_group_name = groupName groupEntry
  ; let touchTime = accessTime fd
  ; let modTime = modificationTime fd
  ; let fMode = fileMode fd
  ; return (FileInfo { owner = file_owner_name
                     , group = file_group_name
                     , size  = file_size
                     , access_time = touchTime
                     , mod_time = modTime
                     , mode     = fMode
                     })
  }