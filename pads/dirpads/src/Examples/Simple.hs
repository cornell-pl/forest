{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

{- To do:
   implement pre-processor type constructor
   implement tar, gzip, etc. type constructors
   implement symbolic links
   debug CVS example
   build pretty printer for directory values and meta-data
   library for manipulating times and permissions
   explore laziness in loading directory files
   implement glob patterns in addition to regular expressions?
   implement patterns in physical names for simple records?
   make relative paths work as arguments to ty_load
   incorporate pads meta-data error counts into forest error counts.
   get gdef to work for representations involving a map (right now raises exception if directory doesn't exist)
   implement regular expression primitives
   implement ls tool

   DONE implement Map/List constructor form for comprehension
   DONE make escape syntax consistent
   DONE fix meta-data error propagation to match pads
   DONE implement nested directories
   DONE implement making second name optional if first name is a legal haskell record field label
   DONE implement parameterized directories
   DONE implement constraints
   DONE implement dependencies
   DONE implement set comprehension directories
   DONE implement matching comprehensions
-}

module Examples.Simple where

import Language.Pads.Padsc
import Language.Forest.Forestc
import Language.Haskell.TH
import Language.Forest.Syntax
import Language.Forest.CodeGen
import System.Time.Utils


import System.IO.Unsafe (unsafePerformIO)

import Language.Haskell.Meta as LHM
import Text.Regex
import Data.Maybe

[pads| type Entry_t = (Pstring ',', ',', Pint)
       type Hosts_t = [Line Entry_t]                  |]

getName (Entry_t (Pstring s, _)) = s
getNames (Hosts_t hs) = map getName hs
getHost (Hosts_t hs) = case hs of 
                       [] -> "Nested"
                       (h:_) -> getName h    -- quantum
                         

[forest| type Hosts_f  = File Hosts_t        -- Hosts_t is expected to be a PADS type 

         type Scores_d = Directory { scores :: File Ptext }

         type Simple_d (file_name :: String ) = Directory 
                         { local  is "local.txt"              :: File Hosts_t where <| (get_owner local_md) == "kfisher" |> 
                         , remote is <|file_name ++ ".txt"|>  :: Hosts_f 
                         , nested is <|getHost local|>        :: Scores_d     where <| (get_group nested_md) == (get_owner local_md) |> 
                         }    |] 


host_file = "/Users/kfisher/pads/dirpads/src/Examples/data/Simple/local.txt"
(host_rep, host_md) = let (Hosts_t rep, md) = unsafePerformIO $ parseFile host_file in (rep,md)
host_file_length = Prelude.length host_rep
host_file_take n  = Prelude.take n host_rep

host_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/Simple"
(simple_rep, simple_md) = unsafePerformIO $ simple_d_load "remote" host_dir


[forest| type Nested_d (file_name :: String) = Directory 
               { hostIndex is <|file_name++".txt"|>  :: File Hosts_t 
               , hosts is Map [ h :: Scores_d | h <- <| getNames hostIndex |>  where <| h /= "china"|> ]
               }  |]

(nested_remote_rep, nested_remote_md) = unsafePerformIO $ nested_d_load "remote" host_dir
(nested_local_rep, nested_local_md) = unsafePerformIO $ nested_d_load "local" host_dir

re = RE ".*[.]txt"


[forest| type Match_d = Directory
             { files is Map [ h :: File Ptext | h <- matches (RE ".*[.]txt") where <| h /= "local.txt"|> ] }
       |]

(match_rep, match_md) = unsafePerformIO $ match_d_load  host_dir

{-
Given a regular expression and a list of strings, return list of strings that match entire regular expression
-}

sample_list = ["remote.txt", "local.txt", "quantum"]
{-
Nested_d {hostIndex = Hosts_t [Entry_t (Pstring "quantum",Pint 2),
                               Entry_t (Pstring "babylon",Pint 3),
                               Entry_t (Pstring "travelbook",Pint 4)], 
          hosts = fromList [("babylon",Scores_d {scores = Ptext "5,Bella6,Edward\n4,Jake\n"}),
                            ("quantum",Scores_d {scores = Ptext "7,Dorothy6,Glinda\n9,Scarecrow\n"}),
                            ("travelbook",Scores_d {scores = Ptext "8,Sunshine3,Mel\n3,Con\n"})]}
-}
{- Type declarations to generate 
ghc  :info ty_name
newtype Hosts_f = Hosts_f Hosts_t
type Hosts_f_md = (Forest_md, Hosts_t_md) 

data Simple_d    = Simple_d {local :: Hosts_t, remote :: Hosts_f}
     deriving (Eq, Show)
type Simple_d_md =  (Forest_md, Simple_d_inner_md)
data Simple_d_inner_md
  = Simple_d_inner_md {local_md :: (Forest_md, Hosts_t_md),
                       remote_md :: Hosts_f_md}
     deriving (Eq, Show)

data Nested_d = Nested_d { hostIndex :: Hosts_t
                         , hosts :: Data.Map String Ptext }
type Nested_d_md =  (Forest_md, Nested_d_inner_md)
data Nested_d_inner_md
  = Nested_d_inner_md { hostIndex_md :: (Forest_md, Hosts_t_md),
                        hosts_md :: Data.Map String (Forest_md, Ptext_md)}

AppT (AppT (ConT Data.Map.Map) (ConT GHC.Base.String)) (ConT GHC.Types.Int)
-}

{-
pathE = (VarE (mkName "path"))
field1 = ("local", "local.txt", File "Hosts_t")
field2 = ("remote", "remote.txt", Named "Hosts_f")
fields = [field1,field2]
dirId = "Simple_d"

forestTy = Directory (Record dirId fields)
tyName = mkName dirId
mdName = mkName (dirId ++ "_md")
loadName = mkName (dirId ++ "_load")
-}
-- genLoadM loadName tyName mdName forestTy Nothing



hosts_f_loadH :: FilePath -> IO (Hosts_f, Hosts_f_md)
hosts_f_loadH path = do
   fmd       <- getForestMD path
   (rep, md) <- parseFile   path
   return (Hosts_f rep, (fmd, md))


{-
*Examples.Simple Language.Haskell.TH Control.Monad> liftM Language.Haskell.TH.ppr  (runQ (loadRecord dirName fields pathE))
do top_md <- Language.Forest.MetaData.getForestMD path
   (local_0,    local_md_1) <- Language.Forest.CodeGen.fileload ((GHC.Base.++) path "/local.txt")
   (remote_2,  remote_md_3) <- hosts_f_load ((GHC.Base.++) path "/remote.txt")
   GHC.Base.return (Simple_d{local = local_0, remote = remote_2},
                    (top_md,
                     Simple_d_inner_md{local_md = local_md_1, remote_md = remote_md_3}))
-}
{-
simple_d_loadH :: FilePath -> IO(Simple_d, Simple_d_md)
simple_d_loadH path = do
     simple_md <- getForestMD path
     (tlocal, tlocal_md)   <- fileload (path++"/local.txt")
     (tremote, tremote_md) <- hosts_f_loadH (path ++ "/remote.txt")
     return (Simple_d {local = tlocal, remote = tremote},
             (simple_md, Simple_d_inner_md { local_md = tlocal_md, remote_md = tremote_md}))
-}


{-
(Forest_md {numErrors = 1, 
            errorMsg = Just (ForestIOException 
                "/Users/kfisher/pads/dirpads/src/Examples/data/Simple/remote.txt1: getFileStatus: does not exist (No such file or directory)"), 
            fileInfo = FileInfo { owner = "kfisher"
                                , group = "kfisher"
                                , size = 204
                                , access_time = 1284004790
                                , mod_time = 1282845009
                                , mode = 16877}},
  Simple_d_inner_md {local_md = (Forest_md 
                                  {numErrors = 0
                                  , errorMsg = Nothing
                                  , fileInfo = FileInfo {owner = "kfisher"
                                                        , group = "kfisher"
                                                        , size = 33
                                                        , access_time = 1284418649
                                                        , mod_time = 1282843992
                                                        , mode = 33188}},
                                 (Errors: 0,
                                   [(Errors: 0,(Errors: 0,Errors: 0,Errors: 0)),
                                    (Errors: 0,(Errors: 0,Errors: 0,Errors: 0)),
                                    (Errors: 0,(Errors: 0,Errors: 0,Errors: 0))])), 
                    remote_md = (Forest_md {numErrors = 1
                                           , errorMsg = Just (ForestIOException 
                                 "/Users/kfisher/pads/dirpads/src/Examples/data/Simple/remote.txt1: getFileStatus: does not exist (No such file or directory)") 
                                           , fileInfo = FileInfo {owner = ""
                                                                 , group = ""
                                                                 , size = -1
                                                                 , access_time = -1
                                                                 , mod_time = -1
                                                                 , mode = 65535}},
                                 (Errors: 1 Problem with file: /Users/kfisher/pads/dirpads/src/Examples/data/Simple/remote.txt1(/Users/kfisher/pads/dirpads/src/Examples/data/Simple/remote.txt1: openFile: does not exist (No such file or directory)).,[]))})
-}