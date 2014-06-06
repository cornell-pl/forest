{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}
module Examples.Universal where
import Language.Pads.Padsc
import Language.Forest.Forestc
import Language.Pads.GenPretty
import Language.Forest.Graph
import Language.Forest.Infer
import Language.Forest.Pretty
import Data.Maybe

import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files
import Control.Concurrent
import Control.Concurrent.Async
import System.Directory

[forest| type Universal_d = Directory 
             { ascii_files  is [ f :: TextFile     | f <- matches <|GL "*"|>, <| get_kind  f_att == AsciiK      |> ]
             , binary_files is [ b :: BinaryFile   | b <- matches <|GL "*"|>, <| get_kind  b_att == BinaryK     |> ]
             , directories  is [ d :: Universal_d  | d <- matches <|GL "*"|>, <| get_kind  d_att == DirectoryK  |> ]
             , symLinks     is [ s :: SymLink      | s <- matches <|GL "*"|>, <| isJust (get_symLink s_att)     |> ]
             } |]


mkPrettyInstance ''Universal_d
mkPrettyInstance ''Universal_d_md

universal_dir = "data/universal"
(universe_rep, universe_md) = unsafePerformIO $ universal_d_load  universal_dir

universal_pretty = putStrLn (pretty 120 (universal_d_ppr universe_rep))
universalIO =  mdToPDF universe_md "Universal.pdf"

decls = buildDesc universe_md
decls_pretty = putStrLn(pretty 80 ( ppr_decls decls))

simple_dir = "data/Simple"
(simple_rep, simple_md) = unsafePerformIO $ universal_d_load  simple_dir
simple_decls = buildDesc simple_md
simple_pretty = putStrLn(pretty 80 ( ppr_decls simple_decls))

classof11_dir = "data/CS/classof11"
(classof11_rep, classof11_md) = unsafePerformIO $ universal_d_load  classof11_dir
classof11_decls = buildDesc classof11_md
classof11_pretty = putStrLn(pretty 80 ( ppr_decls classof11_decls))

getDesc :: FilePath -> IO String
getDesc path = do
 { (rep,md) <- universal_d_load path
 ; let decls = buildDesc md
 ; return (pretty 80 (ppr_decls decls))
 }

loadUniversal :: FilePath -> IO Universal_d
loadUniversal path = do
	(rep,md) <- load path
	return rep

dtree :: Universal_d -> IO ()
dtree uni = do
	putStrLn "ascii_files"
	mapM_ (putStrLn . fst) $ ascii_files uni
	putStrLn "binary_files"
	mapM_ (putStrLn . fst) $ binary_files uni
	putStrLn "directories"
	mapM_ (putStrLn . fst) $ directories uni
	putStrLn "symLinks"
	mapM_ (putStrLn . fst) $ symLinks uni
	return ()
	
loadTest :: IO Universal_d_md
loadTest = do
	let th1 = do
		threadDelay 0
		(rep,md) <- load "test"
		dtree rep
		return md
	let th2 = do
		threadDelay $ fromEnum $ 10^6 * 0.05
		createSymbolicLink "a.txt" "test/0.txt"
	(md,()) <- concurrently th1 th2
--	md <- th1
	removeFile "test/0.txt"
	return md
	
--mangled result:
-- ascii_files
-- binary_files
-- a.txt
-- a1.txt
-- a2.txt
-- a3.txt
-- a4.txt
-- a5.txt
-- a6.txt
-- b.txt
-- directories
-- symLinks
-- 0.txt
-- b.txt

	
	
	
	
	
	
	
	