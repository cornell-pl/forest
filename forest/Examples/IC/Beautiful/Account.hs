{-# LANGUAGE ConstraintKinds, TypeFamilies, DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.IC.Beautiful.Account where

import System.Random

import Language.Pads.Padsc hiding (numErrors)
import Data.Maybe
import Data.IORef
import Language.Haskell.TH.Syntax

import System.Posix.Files
import Control.Concurrent
import Control.Concurrent.Async
import System.Directory
import System.TimeIt
import Control.Monad.IO.Class
import Data.WithClass.MData
import Data.DeepTypeable

import Data.WithClass.Derive.DeepTypeable
import Data.DeriveTH
import Data.WithClass.Derive.MData
import System.FilePath.Posix

import Control.Monad.Incremental.Display
import Data.List as List hiding (delete)
import Control.Monad.Incremental hiding (read,new)
import Prelude hiding (read)
import Language.Forest.IC hiding (writeFile)


[ipads|
	data Account = Account Int
|]

[txforest|
	data Account_d = Directory {
		accs is [ f :: AccountFile | f <- matches (GL "*") ]
	} 
	data AccountFile = File Account
|]

-- Change this to the forest directory to make the example work for you!
--Hugo: made it a relative path (it is fine as long as we use the Makefile)
rootDir = "."

accountDir = rootDir </> "Examples/IC/Beautiful/Account"

-- Transactional Stuff

newAcc :: String -> Int -> IO ()
newAcc name bal = do
  status <- atomically $ do
    (rep :: AccountFile TxVarFS) <- new () (accountDir </> name)
    ((main_info,acc_md),_) <- read rep
    err <- validate rep
    case errorMsg err of
      Just (MissingFile _) -> do
        let my_info = cleanFileInfo (accountDir </> name)
        status <- writeOrElse rep ((my_info,acc_md),Account bal) ("Created account " ++ name ++ " and deposited " ++ show bal) (return . show)
        return status
      _ -> return "This account appears to already exist."
  putStrLn status

delAcc :: String -> IO ()
delAcc name = do
  atomically $ do
    (rep :: AccountFile TxVarFS) <- new () (accountDir </> name)
    delete rep
  putStrLn ("Account " ++ name ++ " deleted.")

tTrans :: String -> String -> Int -> IO ()
tTrans from to amount = do
  (status1,status2) <- atomically $ do
    (rep :: Account_d TxVarFS) <- new () accountDir
    status1 <- tWithHelp from amount rep
    status2 <- tWithHelp to (- amount) rep
    return (status1,status2)                                                                      
  putStrLn status1
  putStrLn status2

tWith :: String -> Int -> IO ()
tWith acc amount = do
  status <- atomically $ do
    rep <- new () accountDir
    status <- tWithHelp acc amount rep
    return status
  putStrLn status

tWith2 :: String -> String -> Int -> IO ()
tWith2 acc1 acc2 amount = do
  status <- atomically $ do
    (rep :: Account_d TxVarFS) <- new () accountDir
    status <- orElse (tWithHelp acc1 amount rep) (tWithHelp acc2 amount rep)
    return status
  putStrLn status

tDepo acc amount = tWith acc (- amount)

-- Transactional Helpers

tWithHelp :: String -> Int -> Account_d TxVarFS -> FTM TxVarFS String
tWithHelp acc amount rep =
  do
    (main_fmd, accdir) <- read rep
    case lookup acc $ accs accdir of
      Just account -> do
        ((accfmd,account_md),Account bal) <- read account
        check (amount < 0 || bal >= amount)
        message <- writeOrElse account ((accfmd,account_md),Account (bal - amount))
                   (acc ++ " had " ++ show bal ++ " and changed by " ++ show (- amount)) (return . show)
        return message
      otherwise -> return $ "Failure: The account does not exist in " ++ show (map fst $ accs accdir)

check :: Bool -> FTM TxVarFS ()
check True = return ()
check False = retry

forever :: IO () -> IO ()
forever act = do
 act
 forever act

randomDelay :: IO ()
randomDelay = do
 waitTime <- getStdRandom (randomR (1000000, 10000000))
 threadDelay waitTime

-- Test function

withdrawer = forkIO (forever (do {tWith2 "acc1" "acc2" 100; randomDelay }))

depositer = forkIO (forever (do {tDepo "acc1" 50; randomDelay }))

transferer = forkIO (forever (do {tTrans "acc1" "acc2" 30; randomDelay }))

runTest = do
 withdrawer
 depositer
 transferer
--
--
--
--
--
--
--
--
--
---- Removes the first account
--transRemTop :: ForestM TxFS ()
--transRemTop = do
--  (rep,md) :: (Account_d, Account_d_md) <- load () accountDir
--  let x:lst = reverse (accs rep)
--  let y:mdlst = reverse (accs_md (snd md))
--  mani <- manifest () ((Account_d_inner lst),((fst md),(Account_d_inner_md mdlst)))
--  store mani
--
---- Changes MetaData
--transMeta :: ForestM TxFS ()
--transMeta = do
--  (rep,md) :: (Account_d, Account_d_md) <- load () accountDir
--  let (str,(fmd,amd)):mdlst = accs_md (snd md)
--  let newFmd = fmd {fileInfo = (fileInfo fmd) {kind = UnknownK}} 
--  let newm = (str,(newFmd,amd))
----  listPrintt (accs_md (snd md))
--  mani <- manifest () (rep,((fst md),(Account_d_inner_md (newm:mdlst))))
--  forestIO $ print "YAY"
--  store mani
--
--
--





---- Random helpers for helping Jonathan figure out Forest
--
--listPrintt :: FSRep fs => [(String,(Forest_md,Account_md))] -> ForestM fs ()
--listPrintt [] = return ()
--listPrintt ((str,(fmd,amd)):xs) = do
--  {
--    forestIO $ putStr (str ++ ": " ++ (show (fileInfo fmd)) ++ "\n" ++ (show amd) ++ "\n");
--    listPrintt xs
--  }
--
--getAccounts :: FSRep fs => ForestM fs ()
--getAccounts = do
--  {
--    (rep,md) :: (Account_d, Account_d_md) <- load () accountDir;
--    let err = get_errors md in
--    do
--      {
--        forestIO $ print (numErrors err);
--        forestIO $ print (errorMsg err)
--      }
--  }
--
--listPrint :: FSRep fs => [(String,Account)] -> ForestM fs ()
--listPrint [] = return ()
--listPrint ((str,Account x):xs) = do
--  {
--    forestIO $ print x;
--    listPrint xs
--  }
--countFiles :: FSRep fs => ForestM fs ()
--countFiles = do
--    (rep,md) :: (Account_d, Account_d_md) <- load () accountDir
--    let (Account_d_inner lst) = rep
--    forestIO $ print $ length lst






---- OLD PURE STUFF:
--
--pureWithdraw :: FSRep fs => String -> Int -> ForestM fs ()
--pureWithdraw acc amount = do
--  {
--    (rep,md) :: (Account_d, Account_d_md) <- load () accountDir;
--    case (lookup acc (accs rep)) of
--      Just (Account newbal) -> do 
--        if (amount < 0 || newbal >= amount) then do
--          let result = map (\ (name, a) -> if name == acc then (acc, (Account $ newbal-amount)) else (name,a)) (accs rep)
----          listPrintt (accs_md (snd md))
--          mani <- manifest () ((Account_d_inner result),md)
--          forestIO $ print (acc ++ " had " ++ show newbal ++ " and changed by " ++ (show (- amount)))
--          store mani
--         else forestIO $ print "The account does not have enough money"
--      _ -> forestIO $ print "The account does not exist"
--  }
--
--pureDeposit acc amount = pureWithdraw acc (- amount)
--
--pureTransfer :: FSRep fs => String -> String -> Int -> ForestM fs ()
--pureTransfer from to amount = do
--  {
--    ((Account_d_inner lst),md) :: (Account_d, Account_d_md) <- load () accountDir;
--    case ((lookup from lst), (lookup to lst)) of
--      (Just (Account f), Just (Account t)) -> do
--        forestIO $ print (from ++ " has " ++ show f ++ " and " ++ to ++ " has " ++ show t)
--        if ((amount >= 0 && f >= amount) || (amount < 0 && t >= (- amount)))
--          then do
--            pureWithdraw from amount
--            pureDeposit to amount
--          else forestIO $ print "The account does not have enough money"
--      _ -> forestIO $ print "At least one of the accounts does not exist"
--  }
--
---- Pure Helpers
--
--pureTrans from to amount = runForest PureFSForestCfg $ pureTransfer from to amount
--pureWith acc amount = runForest PureFSForestCfg $ pureWithdraw acc amount
--pureDepo acc amount = runForest PureFSForestCfg $ pureDeposit acc amount
--
--pureTransAcc = pureTrans "acc1" "acc2"
--
--
--
--
--
--
--
--