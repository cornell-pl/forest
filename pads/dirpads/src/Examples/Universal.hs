{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}
module Examples.Universal where
import Language.Pads.Padsc
import Language.Forest.Forestc
import Language.Pads.GenPretty
import Language.Forest.Graph
import Language.Forest.Infer
import Language.Forest.Pretty

import System.IO.Unsafe (unsafePerformIO)

[forest| type Universal_d = Directory 
             { ascii_files  is [ f :: Text         | f <- matches (GL "*"), <| get_kind  f_att == AsciiK      |> ]
             , binary_files is [ b :: File Pbinary | b <- matches (GL "*"), <| get_kind  b_att == BinaryK     |> ]
             , directories  is [ d :: Binary       | d <- matches (GL "*"), <| get_kind  d_att == DirectoryK  |> ]
             , symLinks     is [ s :: SymLink      | s <- matches (GL "*"), <| get_isSym s_att == True        |> ]
             } |]


mkPrettyInstance ''Universal_d
mkPrettyInstance ''Universal_d_md

universal_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/universal"
(universe_rep, universe_md) = unsafePerformIO $ universal_d_load  universal_dir

universal_pretty = putStrLn (pretty 120 (universal_d_ppr universe_rep))
universalIO =  mdToPDF universe_md "/Users/kfisher/pads/dirpads/src/Examples/Universal.pdf"

decls = buildDesc universe_md
decls_pretty = putStrLn(pretty 80 ( ppr_decls decls))

simple_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/Simple"
(simple_rep, simple_md) = unsafePerformIO $ universal_d_load  simple_dir
simple_decls = buildDesc simple_md
simple_pretty = putStrLn(pretty 80 ( ppr_decls simple_decls))

classof11_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/facadm/classof11"
(classof11_rep, classof11_md) = unsafePerformIO $ universal_d_load  classof11_dir
classof11_decls = buildDesc classof11_md
classof11_pretty = putStrLn(pretty 80 ( ppr_decls classof11_decls))

getDesc :: FilePath -> IO String
getDesc path = do
 { (rep,md) <- universal_d_load path
 ; let decls = buildDesc md
 ; return (pretty 80 (ppr_decls decls))
 }
