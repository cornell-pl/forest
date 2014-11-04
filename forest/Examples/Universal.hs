{-# LANGUAGE DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}
module Examples.Universal where
import Language.Pads.Padsc hiding (numErrors)
import Language.Forest.Class
import Language.Pads.GenPretty
import Language.Forest.Graph
--import Language.Forest.Infer
import Language.Forest.Pretty
import Language.Forest.FS.NILFS
import Data.Maybe
import Data.DeepTypeable
import Data.IORef
import Control.Monad.Incremental
import Language.Haskell.TH.Syntax

import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files
import Control.Concurrent
import Control.Concurrent.Async
import System.Directory
import Data.WithClass.MData
import Language.Forest.FS.FSRep
import System.TimeIt
import Control.Monad.IO.Class
import Language.Forest.MetaData
--import Language.Forest.Show
import Language.Forest.Draw
import Language.Forest.FS.FSRep

[forest| type Universal_d = Directory 
             { ascii_files  is [ f :: TextFile     | f <- matches <| return $ GL "*" |>, <| return $ kind  f_att == AsciiK      |> ]
             , binary_files is [ b :: BinaryFile   | b <- matches <| return $ GL "*" |>, <| return $ kind  b_att == BinaryK     |> ]
             , directories  is [ d :: Universal_d  | d <- matches <| return $ GL "*" |>, <| return $ kind  d_att == DirectoryK  |> ]
             , symLinks     is [ s :: SymLink      | s <- matches <| return $ GL "*" |>, <| return $ isJust (symLink s_att)     |> ]
             } |]

[forest| type Universal_zip = Gzip (Tar Universal_d) |]

myDir = "/home/hpacheco/Forest"
home = "/home/hpacheco"
universal_zip_root = "/media/hpacheco/nilfs/judy1.tar.gz"
universal_zip_root' = "/media/hpacheco/nilfs/judy2.tar.gz"

universal_zip_Errors :: FSRep fs => ((Universal_zip fs,Universal_zip_md fs),LoadInfo fs) -> ForestO fs ()
universal_zip_Errors ((rep,md),_) = do
	err <- get_errors md
	forestIO $ print (numErrors err)
	forestIO $ print (errorMsg err)

universal_zip :: ForestO NILFS ()
universal_zip = do
	dta@(repmd::(Universal_zip NILFS,Universal_zip_md NILFS),_) <- load () universal_zip_root
	universal_zip_Errors dta
	forestDrawToPDF proxyNILFS repmd $ "/home/hpacheco/1.pdf"
	 
	-- reload
	reload universal_zip_root' dta
	universal_zip_Errors dta
	forestDrawToPDF proxyNILFS repmd $ "/home/hpacheco/2.pdf"
	
	return ()

universal_zip_NILFS = runForest (NILFSForestConfig "/media/hpacheco/nilfs/" myDir) $ universal_zip
--mkPrettyInstance ''Universal_d
--mkPrettyInstance ''Universal_d_md

--universal_dir = "data/universal"
----(universe_rep, universe_md) = unsafePerformIO $ universal_d_load  universal_dir
--
----universal_pretty = putStrLn (pretty 120 (universal_d_ppr universe_rep))
----universalIO =  mdToPDF universe_md "Universal.pdf"
--
--decls = buildDesc universe_md
--decls_pretty = putStrLn(pretty 80 ( ppr_decls decls))
--
----simple_dir = "data/Simple"
----(simple_rep, simple_md) = unsafePerformIO $ universal_d_load  simple_dir
----simple_decls = buildDesc simple_md
----simple_pretty = putStrLn(pretty 80 ( ppr_decls simple_decls))
--
----classof11_dir = "data/CS/classof11"
----(classof11_rep, classof11_md) = unsafePerformIO $ universal_d_load  classof11_dir
----classof11_decls = buildDesc classof11_md
----classof11_pretty = putStrLn(pretty 80 ( ppr_decls classof11_decls))
--
--getDesc :: FilePath -> IO String
--getDesc path = do
-- { (rep,md) <- universal_d_load path
-- ; let decls = buildDesc md
-- ; return (pretty 80 (ppr_decls decls))
-- }
--
--loadUniversal :: FilePath -> IO Universal_d
--loadUniversal path = do
--	(rep,md) <- load path
--	return rep
--
--dtree :: Universal_d -> IO ()
--dtree uni = do
--	putStrLn "ascii_files"
--	mapM_ (putStrLn . fst) $ ascii_files uni
--	putStrLn "binary_files"
--	mapM_ (putStrLn . fst) $ binary_files uni
--	putStrLn "directories"
--	mapM_ (putStrLn . fst) $ directories uni
--	putStrLn "symLinks"
--	mapM_ (putStrLn . fst) $ symLinks uni
--	return ()
--	
--loadTest :: IO Universal_d_md
--loadTest = do
--	let th1 = do
--		threadDelay 0
--		(rep,md) <- load "test"
--		dtree rep
--		return md
--	let th2 = do
--		threadDelay $ fromEnum $ 10^6 * 0.05
--		createSymbolicLink "a.txt" "test/0.txt"
--	(md,()) <- concurrently th1 th2
----	md <- th1
--	removeFile "test/0.txt"
--	return md
--	
----mangled result:
---- ascii_files
---- binary_files
---- a.txt
---- a1.txt
---- a2.txt
---- a3.txt
---- a4.txt
---- a5.txt
---- a6.txt
---- b.txt
---- directories
---- symLinks
---- 0.txt
---- b.txt
--
--	
--	
--	
--	
--	
--	
--	
--	