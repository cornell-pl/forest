{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.CVS where

import Language.Pads.Padsc
import Language.Forest.Forestc hiding (Local, Dir, entries)
import Language.Pads.GenPretty

import System.IO.Unsafe (unsafePerformIO)

[pads| data Repository_f = Repository_f (Line StringLn)

       data Mode_t = Ext ":ext:" | Local ":local:" | Server ":server:" 

       data Root_t = Root_t
             { cvs_mode :: Maybe Mode_t
             , machine  :: StringC ':', ':'
             , path     :: StringLn            
             }                                  
       data Root_f = Root_f (Line Root_t)

       data Dentry_t = Dentry_t
             { "D/"
             , dirname :: StringC '/'
             , "////"
             }

       data Revision_t  = Version (Int, '.', Int) | Added '0' | Removed '-'
       data TimeStamp_t = TimeStamp_t
             { ts       :: StringSE '[/+]'
             , conflict :: Maybe ('+', StringC '/') }

       data Fentry_t = Fentry_t
             {                                  "/"  
             , filename   :: StringC '/',       "/"
             , revision   :: Revision_t,        "/"
             , timestamp  :: TimeStamp_t,       "/"   
             , options    :: StringC '/',       "/"  
             , tagdate    :: StringLn
             }

       data Entry_t   = Dir Dentry_t | File Fentry_t | NoDir 'D'
     
       data Entries_f = Entries_f ([Line Entry_t] terminator EOF)
|]

getEntries cvs =  let (Entries_f l) = entries cvs  in l

isDir entry  = case entry of {Dir _  -> True; otherwise -> False}
isFile entry = case entry of {File _ -> True; otherwise -> False}

getDirs  cvs = map (\(Dir d)  -> dirname  d) (filter isDir  (getEntries cvs))
getFiles cvs = map (\(File f) -> filename f) (filter isFile (getEntries cvs))


[forest| type CVS_d = Directory 
              { repository is "Repository" :: File Repository_f
              , root       is "Root"       :: File Root_f
              , entries    is "Entries"    :: File Entries_f
              }
             
         type CVS_Repository_d = Directory
             { cvs         is "CVS"                 :: CVS_d
             , dirs        is [ d :: CVS_Repository_d | d <- <| getDirs  cvs |> ]
             , files       is [ f :: TextFile         | f <- <| getFiles cvs |> ]
             } |]



entries_file = meta_dir ++ "/Entries"
(entries_rep, entries_pd) = let (Entries_f rep, md) = unsafePerformIO $ parseFile entries_file in (rep,md)

meta_dir = "/Users/kfisher/pads/dirpads/src/Examples/CVS"
(meta_rep, meta_md) = unsafePerformIO $ cVS_d_load meta_dir

examples_dir = "/Users/kfisher/pads/dirpads/src/Examples"
(examples_rep, examples_md) = unsafePerformIO $ cVS_Repository_d_load examples_dir

babylon_dir =  "/Users/kfisher/pads/dirpads/src/Examples/data/Simple/babylon"
(babylon_rep, babylon_md) = unsafePerformIO $ cVS_Repository_d_load babylon_dir

simple_dir =  "/Users/kfisher/pads/dirpads/src/Examples/data/Simple"
(simple_rep, simple_md) = unsafePerformIO $ cVS_Repository_d_load simple_dir

classof10_dir =  "/Users/kfisher/pads/dirpads/src/Examples/data/facadm/classof10"
(classof10_rep, classof10_md) = unsafePerformIO $ cVS_Repository_d_load classof10_dir

classof11_dir =  "/Users/kfisher/pads/dirpads/src/Examples/data/facadm/classof11"
(classof11_rep, classof11_md) = unsafePerformIO $ cVS_Repository_d_load classof11_dir

facadm_dir =  "/Users/kfisher/pads/dirpads/src/Examples/data/facadm"
(facadm_rep, facadm_md) = unsafePerformIO $ cVS_Repository_d_load facadm_dir


mkPrettyInstance ''CVS_Repository_d
mkPrettyInstance ''CVS_Repository_d_md
