{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.LazyTest where

import Language.Pads.Padsc
import Language.Forest.Forestc

import System.IO.Unsafe (unsafePerformIO)

import Examples.AI 

[pads| type SEntry_t = (Pstring ',', ',', Pint)
       type Hosts_t = [Line SEntry_t]                  |]


[forest| type Scores_d = Directory 
            { scores :: File Ptext
--            , airef  is ai_file :: File AI_t       
} |] 

path = "/Users/kfisher/pads/dirpads/src/Examples/data/Simple/babylon"
(simple_rep, simple_md) = unsafePerformIO $ scores_d_load path

{-
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


-}