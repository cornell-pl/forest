{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}
module Examples.Universal where
import Language.Pads.Padsc
import Language.Forest.Forestc
import Language.Pads.GenPretty
import System.IO.Unsafe (unsafePerformIO)

[forest| type Universal_d = Directory 
             { ascii_files  is [ f :: File Ptext   | f <- matches (GL "*"), <| get_kind  f_att == AsciiK      |> ]
             , binary_files is [ b :: File Pbinary | b <- matches (GL "*"), <| get_kind  b_att == BinaryK     |> ]
             , directories  is [ d :: Universal_d  | d <- matches (GL "*"), <| get_kind  d_att == DirectoryK  |> ]
             , symLinks     is [ s :: SymLink      | s <- matches (GL "*"), <| get_isSym s_att == True        |> ]
             } |]


mkPrettyInstance ''Universal_d
mkPrettyInstance ''Universal_d_md

universal_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/universal"
(universe_rep, universe_md) = unsafePerformIO $ universal_d_load  universal_dir

universal_pretty = putStrLn (pretty 120 (universal_d_ppr universe_rep))
