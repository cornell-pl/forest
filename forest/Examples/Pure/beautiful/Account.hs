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


[pads|
 data Account = Account (StringME <|RE "[0-9]+"|>)

 |]

[forest|
 type Account_d = Directory 
             { accs  is [ f :: File Account    | f <- matches (GL "*") ]} 
|]

-- Change this to the forest directory to make the example work for you!
forestDir = "/home/dilorenzo/everything/research/forest/forest/forest" 

accountDir = forestDir ++ "/Examples/Pure/beautiful/Account"

getAccounts :: FSRep fs => ForestM fs ()
getAccounts = do
  {
    (rep,md) :: (Account_d, Account_d_md) <- load () accountDir;
    let err = get_errors md in
    do
      {
        forestIO $ print (numErrors err);
        forestIO $ print (errorMsg err)
      }
  }
listPrint :: FSRep fs => [(String,Account)] -> ForestM fs ()
listPrint [] = return ()
listPrint ((str,Account x):xs) = do
  {
    forestIO $ print x;
    listPrint xs
  }
countFiles :: FSRep fs => ForestM fs ()
countFiles = do
    (rep,md) :: (Account_d, Account_d_md) <- load () accountDir
    let (Account_d_inner lst) = rep
    forestIO $ print $ length lst

pureTransfer1 :: FSRep fs => String -> String -> Int -> ForestM fs ()
pureTransfer1 from to amount = do
  {
    ((Account_d_inner lst),md) :: (Account_d, Account_d_md) <- load () accountDir;
    case ((lookup from lst), (lookup to lst)) of
      (Just (Account f), Just (Account t)) -> do
        mani <- manifest () ((Account_d_inner [(from, (Account (show ((read f)-amount)))),(to, (Account (show ((read t)+amount))))]),md)
        -- store mani
        forestIO $ print (from ++ " has " ++ f ++ " and " ++ to ++ " has " ++ t)
      _ -> forestIO $ print "At least one of the accounts does not exist"
  }

pureTrans from to amount = runForest PureFSForestCfg $ pureTransfer1 from to amount

pureTransAcc amount = pureTrans "acc1" "acc2" amount

{-
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
-}