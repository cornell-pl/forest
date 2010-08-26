{-# LANGUAGE TypeSynonymInstances #-}

module Examples.SimpleForest where

import Language.Pads.Padsc
import Language.Forest.Forestc

import System.IO.Unsafe (unsafePerformIO)


[pads| type Entry_t = (Pstring(:',':), ',', Pint)
       type EntryL_t = Line Entry_t
       type Hosts_f = [EntryL_t]                                        |]

[forest| type Simple = Directory 
                         { local  is "local.txt"  :: Hosts_f
                         , remote is "remote.txt" :: Hosts_f } |]

{- Type declarations to generate -}
data Simple = Simple { local  :: Host_f
                     , remote :: Host_f }

data Simple_md_inner = Simple_md_inner 
                         { local_md  :: Host_f_md
                         , remote_md :: Host_f_md
                         }

type Simple_md = (Forest_Base_md, Simple_md_inner)



host_file = "/Users/kfisher/pads/dirpads/src/Examples/data/Simple/local.txt"
(host_rep, host_md) = let (Hosts_t rep, md) = unsafePerformIO $ parseFile host_file in (rep,md)
host_file_length = Prelude.length host_rep
host_file_take n  = Prelude.take n host_rep