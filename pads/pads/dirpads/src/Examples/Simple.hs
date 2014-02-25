{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables, NamedFieldPuns #-}

{- To do:
   TOOL: lookup :: (Data a) => String -> Maybe a  (where a = String_t)
   computed fields: can I put a list of fileNames in the rep of a Directory or not?
        type AnyDir = Directory
          { names = [ n :: String | n <- matches (GL "*"),
          ... }
   performance tuning
   library for manipulating times and permissions
      add `isCompatabile` comparator for FileModes
   incorporate pads meta-data error counts into forest error counts.
   implement regular expression primitives
   implement pre-processor type constructor
   debug CVS example: too many file handles open
   change "type" to "newtype"?  (ask david and nate)  explore whether types work
   extend syntax of Haskell identifiers to include qualified names (e.g., AI.AI.t)
   clean up namespace
   Add padstypes to matches generator:
      { students is [ filename :: File (Student <| name student |>) | 
                    (filename, student) <= matches Student_filename where not (template filename) ] }
   How to specify fields that match a regular expression but have a single representation.
   BUG: Maybe followed by a regular expression: see Students4.hs Grades
   BUG: not an error to read a directory as a text file :-(
   BUG: Maybe as other than last type constructor in record (see PWS.hs)
   BUG: pretty printer for Maybes
   BUG: Maybe with a underlying type with a constraint violation fails; I guess this is the right behavior...
   literate haskell

   DONE TOOL: check permissions
   DONE TOOL: shell tools
   DONE TOOL: given file path, predicate on FMDs, depth limit, produce forest description
   DONE TOOL: given a (rep,md), produce a dot graph (colored according to metadata)
   DONE add [] form for directory;    remove Directory keyword
   FIXED BUG: error squasing in maybes doesn't work; move path check to inside loadTy function to fix.
   DONE implement this form for typedefs in forest
   DONE add other type constructors besides map
   DONE implement simple matches 
   DONE redo comprenensions as discussed (f_att)  (where -> ',')
   DONE explore laziness in loading directory files
   DONE write a "unverisal description" w/binry and ascii
   DONE implement glob patterns in addition to regular expressions
   DONE make relative paths work as arguments to ty_load
   DONE get gdef to work for representations involving a map (right now raises exception if directory doesn't exist)
   DONE implement symbolic links
   DONE build pretty printer for directory values and meta-data
   DONE implement tar, gzip, etc. type constructors
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

import Language.Haskell.TH hiding (ppr)
import System.Time.Utils
import System.FilePath.Posix
import Language.Pads.GenPretty
import Language.Forest.Graph

import System.IO.Unsafe (unsafePerformIO)

import Language.Haskell.Meta as LHM
import Text.Regex
import Data.Maybe
import Data.Set as S hiding (map)


-- import Examples.AI 

[pads| type SEntry_t = (Pstring ',', ',', Pint)
       type Hosts_t = [Line SEntry_t]                  |]

getName (SEntry_t (Pstring s, _)) = s
getNames (Hosts_t hs) = map getName hs
getHost (Hosts_t hs) = case hs of 
                       [] -> "Nested"
                       (h:_) -> getName h    -- quantum
                         

[forest| type Hosts_f  = File Hosts_t        -- Hosts_t is expected to be a PADS type 
         type ReadOnly    = Text where <| get_modes this_att == "-rw-r--r--" |>
         type CheckPerm (perm :: String)    = Text where <| get_modes this_att == perm |>

         type Scores_d = Directory { scores :: CheckPerm "-rw-r--r--" }

         type Simple_d (file_name :: String ) = Directory 
                         { local  is "local.txt"              :: File Hosts_t where <| (get_owner local_md) == "kfisher" |> 
                         , mylink                             :: Scores_d
                         , fileLink                           :: Hosts_f
                         , remote is <|file_name ++ ".txt"|>  :: Hosts_f      where <| (get_modes remote_md) == "-rw-rw-r--" |>
                         , nested is <|getHost local|>        :: Scores_d     where <| (get_group nested_md) == (get_owner local_md) |> 
                         , mylink_sym is "mylink"             :: SymLink      where <| mylink_sym == "quantum" |>
                         , optFile                            :: Maybe ReadOnly
                         }    

         type PrivateFile = constrain this :: File Ptext where <| get_modes this_md == "-rw-rw-r--" |>

         type TextFiles = Set [ f :: File Ptext | f <- matches (GL "*.txt") ]

         type NoBin =
             [ b :: Binary
             | b <- matches (GL "*"), 
              <|get_kind b_att == BinaryK|> ]
             where <|length this == 0|>
|] 

mkPrettyInstance ''TextFiles

mkPrettyInstance ''Simple_d
mkPrettyInstance ''Simple_d_md

doSimpleTest :: IO Manifest
doSimpleTest = do 
 { pResult <- simple_d_load "remote" host_dir
 ; emptyManifest <- newManifest
 ; simple_d_updateManifest "remote" pResult emptyManifest
 }


local_file = "/Users/kfisher/pads/dirpads/src/Examples/data/Simple/local.txt"
remote_file = "/Users/kfisher/pads/dirpads/src/Examples/data/Simple/remote.txt"
(lpvt_rep, lpvt_md) =  unsafePerformIO $ privateFile_load local_file
(rpvt_rep, rpvt_md) =  unsafePerformIO $ privateFile_load remote_file
(lread_rep, lread_md) =  unsafePerformIO $ readOnly_load local_file
(rread_rep, rread_md) =  unsafePerformIO $ readOnly_load remote_file

host_file = "/Users/kfisher/pads/dirpads/src/Examples/data/Simple/local.txt"
(Hosts_f host_result, hostMD)  = unsafePerformIO $ hosts_f_load host_file
(host_rep, host_md) = let (Hosts_t rep, md) = unsafePerformIO $ parseFile host_file in (rep,md)
host_file_length = Prelude.length host_rep
host_file_take n  = Prelude.take n host_rep

doHostTest :: IO Manifest
doHostTest = do 
 { pResult <- hosts_f_load host_file
 ; emptyManifest <- newManifest
 ; hosts_f_updateManifest pResult emptyManifest
 }

host_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/Simple"
(simple_rep, simple_md) = unsafePerformIO $ simple_d_load "remote" host_dir


resultSimpleIO =  mdToPDF simple_md "/Users/kfisher/pads/dirpads/src/Examples/Simple.pdf"

(text_rep, text_md) = unsafePerformIO $ textFiles_load host_dir

doListTest :: IO Manifest
doListTest = do 
 { pResult <- textFiles_load host_dir
 ; emptyManifest <- newManifest
 ; textFiles_updateManifest pResult emptyManifest
 }


notChina h = h /= "china"


[forest| type Nested_d (file_name :: String) = Directory 
               { hostIndex is <|file_name++".txt"|>  :: File Hosts_t 
               , hostsM is Map  [ h :: Scores_d | h <- <| getNames hostIndex |>, <| h /= "china"|> ]
               , hostsS is Set  [ h :: Scores_d | h <- <| getNames hostIndex |>, <| h /= "china"|> ]
               }  |]


doNestedTest = do 
 { pResult <- nested_d_load "remote" host_dir
 ; generateManifest1 "remote" pResult
 }



doManifest :: Manifest -> IO ()
doManifest manifest = 
   storeManifestAt "/Users/kfisher/temp"  manifest

(nested_remote_rep, nested_remote_md) = unsafePerformIO $ nested_d_load "remote" host_dir
(nested_local_rep, nested_local_md) = unsafePerformIO $ nested_d_load "local" host_dir

resultNestedIO =  mdToPDF nested_remote_md "/Users/kfisher/pads/dirpads/src/Examples/Nested_Remote.pdf"

re = RE ".*[.]txt"


[forest| type Match_d = Directory
             { files is Map [ h :: File Ptext | h <- matches (RE ".*[.]txt"),  <| h  /= "local.txt"|> ] }
       |]

(match_rep, match_md) = unsafePerformIO $ match_d_load  host_dir
resultMatchIO =  mdToPDF match_md "/Users/kfisher/pads/dirpads/src/Examples/Match.pdf"



[forest| type Global = Directory 
               { globalNotes is "/Users/kfisher/pads/dirpads/src/Notes/ghc-tricks"  :: Text }
  |]

(global_rep, global_md) = unsafePerformIO $ global_load host_dir