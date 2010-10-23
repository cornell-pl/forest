{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}
module Examples.Universal where
import Language.Pads.Padsc
import Language.Forest.Forestc
import Language.Pads.GenPretty
import System.IO.Unsafe (unsafePerformIO)

[forest| type Universal_d = Directory 
             { ascii_files  is [ f :: File Ptext   | f <- matches (GL "*") where <| get_kind f_md == AsciiK      |> ]
             , binary_files is [ b :: File Pbinary | b <- matches (GL "*") where <| get_kind b_md == BinaryK     |> ]
             , directories  is [ d :: Universal_d  | d <- matches (GL "*") where <| get_kind d_md == DirectoryK  |> ]
             , symLinks     is [ s :: SymLink      | s <- matches (GL "*") where <| get_sym  s_md == True        |> ]
             } |]

mkPrettyInstance ''Universal_d
mkPrettyInstance ''Universal_d_md

universal_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/universal"
(universe_rep, universe_md) = unsafePerformIO $ universal_d_load  universal_dir

universal_pretty = putStrLn (pretty 120 (universal_d_ppr universe_rep))
