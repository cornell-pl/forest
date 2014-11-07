{-# LANGUAGE DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.Universal where
	
import Language.Pads.Padsc hiding (numErrors)
import Data.Maybe
import Data.IORef
import Language.Haskell.TH.Syntax

import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files
import Control.Concurrent
import Control.Concurrent.Async
import System.Directory
import Language.Forest.Pure
import System.TimeIt
import Control.Monad.IO.Class


[forest| type Universal_d = Directory 
             { ascii_files  is [ f :: TextFile     | f <- matches (GL "*"), (kind  f_att == AsciiK) ]
             , binary_files is [ b :: BinaryFile   | b <- matches (GL "*"), (kind  b_att == BinaryK) ]
             , directories  is [ d :: Universal_d  | d <- matches (GL "*"), (kind  d_att == DirectoryK) ]
             , symLinks     is [ s :: SymLink      | s <- matches (GL "*"), (isJust (symLink s_att)) ]
             } |]

[forest| type Universal_zip = Gzip (Tar Universal_d) |]

home = "/home/hpacheco"
universal_zip_root = "/media/hpacheco/nilfs/judy1.tar.gz"
universal_zip_root' = "/media/hpacheco/nilfs/judy2.tar.gz"

universal_zip_Errors :: FSRep fs => (Universal_zip,Universal_zip_md) -> ForestM fs ()
universal_zip_Errors (rep,md) = do
	let err = get_errors md
	forestIO $ print (numErrors err)
	forestIO $ print (errorMsg err)

universal_zip :: FSRep fs => ForestM fs ()
universal_zip = do
	(dta::(Universal_zip,Universal_zip_md)) <- load () universal_zip_root
	universal_zip_Errors dta
	
	return ()

universal_zip_TxFS = runForest TxFSForestCfg $ universal_zip
