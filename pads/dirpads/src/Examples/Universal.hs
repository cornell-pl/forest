{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.Coral where

import Language.Pads.Padsc
import Language.Forest.Forestc
import Language.Pads.GenPretty
import Language.Forest.CodeGen
import Language.Forest.MetaData

import Data.Map (fromListWith)

import System.IO.Unsafe (unsafePerformIO)

[forest| type Universal_d = Directory {
--              ascii_files  is [ f :: File Ptext   | f <- matches (GL "*") where <| get_kind f_md == AsciiK      |> ]
--             , binary_files is [ b :: File Pbinary | b <- matches (GL "*") where <| get_kind b_md == BinaryK     |> ]
              directories  is [ d :: Universal_d  | d <- matches (GL "*") where <| get_kind d_md == DirectoryK  |> ]
--             , symLinks     is [ s :: SymLink      | s <- matches (GL "*") where <| get_sym  s_md == True        |> ]
             } |]


universal_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/universal"
(universe_rep, universe_md) = unsafePerformIO $ universal_d_load  universal_dir

host_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/Simple"
(simple_rep, simple_md) = unsafePerformIO $ universal_d_load  host_dir

fInfo :: (Language.Forest.MetaData.Forest_md, Universal_d_inner_md)
fInfo = (unsafePerformIO $ getForestMD (universal_dir ++ "/link"), undefined)

testValue :: IO (Universal_d, (Language.Forest.MetaData.Forest_md, Universal_d_inner_md))
testValue = return (undefined fInfo)

uload = \ path -> -- checkPath path
                            (do { dir_md <- getForestMD path
                                ; directories_files <- getMatchingFiles path (GL "*")
                                ; ~(directories, directories_md, directories_bmd) <- Language.Forest.CodeGen.insertRepMDsList'
                                                                         [(d, -- checkPath (concatPath path d)
--                                                                                        (universal_d_load (concatPath path d)), 
                                                                          testValue,
                                                                           \ ~(d, d_md) -> (get_kind d_md == DirectoryK)) |
                                                                            d <- directories_files ]
                                ; let top_md = updateForestMDwith dir_md [directories_bmd]
                                ; return  (Universal_d {directories = directories}, 
                                              (top_md, Universal_d_inner_md {directories_md = directories_md})) 
                                })

