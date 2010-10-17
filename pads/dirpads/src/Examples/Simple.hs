{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

{- To do:
   explore laziness in loading directory files
   literate haskell
   library for manipulating times and permissions
      add `isCompatabile` comparator for FileModes
   write a "unverisal description" w/binry and ascii
   TOOL: given file path, predicate on FMDs, depth limit, produce forest description
   TOOL: given a (rep,md), produce a dot graph (colored according to metadata)
   TOOL: lookup :: (Data a) => String -> Maybe a  (where a = String_t)
   TOOL: check permissions
   implement patterns in physical names for simple records?
   incorporate pads meta-data error counts into forest error counts.
   implement regular expression primitives
   implement ls tool
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
import Language.Forest.Syntax
import Language.Forest.CodeGen
import System.Time.Utils
import Language.Pads.GenPretty

import System.IO.Unsafe (unsafePerformIO)

import Language.Haskell.Meta as LHM
import Text.Regex
import Data.Maybe
-- import Examples.AI 

[pads| type SEntry_t = (Pstring ',', ',', Pint)
       type Hosts_t = [Line SEntry_t]                  |]

getName (SEntry_t (Pstring s, _)) = s
getNames (Hosts_t hs) = map getName hs
getHost (Hosts_t hs) = case hs of 
                       [] -> "Nested"
                       (h:_) -> getName h    -- quantum
                         

[forest| type Hosts_f  = File Hosts_t        -- Hosts_t is expected to be a PADS type 

         type Scores_d = Directory { scores :: File Ptext }

         type Simple_d (file_name :: String ) = Directory 
                         { local  is "local.txt"              :: File Hosts_t where <| (get_owner local_md) == "kfisher" |> 
                         , remote is <|file_name ++ ".txt"|>  :: Hosts_f      where <| (get_modes remote_md) == "-rw-r--r--" |>
                         , nested is <|getHost local|>        :: Scores_d     where <| (get_group nested_md) == (get_owner local_md) |> 
                         , mylink_sym is "mylink"             :: SymLink      where <| mylink_sym == "quantum" |>
                         , mylink                             :: Scores_d
                         , generic is "Generic.o"             :: File Pbinary
--                         , mylink                             :: SymLink Scores_d   where <| sym_link mylink == "quantum" |>
--                         , airef  is ai_file                  :: File AI_t
                         }    |] 

mkPrettyInstance ''Simple_d
mkPrettyInstance ''Simple_d_md



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


