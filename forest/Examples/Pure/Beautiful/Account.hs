{-# LANGUAGE DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.Pure.Beautiful.Account where

import System.Random

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
 data Account = Account Int
 |]

[forest|
 type Account_d = Directory 
             { accs  is [ f :: File Account    | f <- matches (GL "*") ]} 
|]

-- Change this to the forest directory to make the example work for you!
--Hugo: made it a relative path (it is fine as long as we use the Makefile)
--forestDir = "/home/dilorenzo/everything/research/forest/forest/forest" 
forestDir = "."

accountDir = forestDir ++ "/Examples/Pure/Beautiful/Account"

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

-- PURE STUFF:

pureTransfer :: FSRep fs => String -> String -> Int -> ForestM fs ()
pureTransfer from to amount = do
  {
    ((Account_d_inner lst),md) :: (Account_d, Account_d_md) <- load () accountDir;
    case ((lookup from lst), (lookup to lst)) of
      (Just (Account f), Just (Account t)) -> do
        forestIO $ print (from ++ " has " ++ show f ++ " and " ++ to ++ " has " ++ show t)
        if ((amount >= 0 && f >= amount) || (amount < 0 && t >= (- amount)))
          then do
            pureWithdraw from amount
            pureDeposit to amount
          else forestIO $ print "The account does not have enough money"
      _ -> forestIO $ print "At least one of the accounts does not exist"
  }

pureWithdraw :: FSRep fs => String -> Int -> ForestM fs ()
pureWithdraw acc amount = do
  {
    ((Account_d_inner lst),md) :: (Account_d, Account_d_md) <- load () accountDir;
    case (lookup acc lst) of
      Just (Account newbal) -> do 
        if (amount < 0 || newbal >= amount) then do
          let result = [(acc, (Account $ newbal-amount))] ++ (filter (\ (name, _) -> name /= acc) lst) 
          mani <- manifest () ((Account_d_inner result),md)
          store mani
          forestIO $ print (acc ++ " had " ++ show newbal ++ " and lost " ++ (show amount))
         else forestIO $ print "The account does not have enough money"
      _ -> forestIO $ print "The account does not exist"
  }

pureDeposit acc amount = pureWithdraw acc (- amount)

-- Pure Helpers

pureTrans from to amount = runForest PureFSForestCfg $ pureTransfer from to amount
pureWith acc amount = runForest PureFSForestCfg $ pureWithdraw acc amount
pureDepo acc amount = runForest PureFSForestCfg $ pureDeposit acc amount

pureTransAcc = pureTrans "acc1" "acc2"

-- Transactional Stuff

transTransfer :: String -> String -> Int -> IO ()
transTransfer from to amount = atomically (do {transWithdraw from amount; transDeposit to amount})

transWithdraw :: String -> Int -> ForestM TxFS () -- How do I make this take any TransactionalPureForest FS?
transWithdraw acc amount = do  -- Now with retry!
  {
    ((Account_d_inner lst),md) :: (Account_d, Account_d_md) <- load () accountDir;
    case (lookup acc lst) of
      Just (Account newbal) -> do 
        check (amount < 0 || newbal >= amount)
        let result = [(acc, (Account $ newbal-amount))]
        mani <- manifest () ((Account_d_inner result),md)
        -- store mani
        forestIO $ print (acc ++ " had " ++ show newbal ++ " and lost " ++ (show amount))
      _ -> forestIO $ print "The account does not exist"
  }


transWithdraw2 :: String -> String -> Int -> ForestM TxFS () -- How do I make this take any TransactionalPureForest FS?
transWithdraw2 acc1 acc2 amount = orElse (transWithdraw acc1 amount) (transWithdraw acc2 amount)

transDeposit acc amount = transWithdraw acc (- amount)

-- Transactional Helpers

check :: Bool -> ForestM TxFS ()
check True = return ()
check False = retry

-- Note: retry only works when called from an atomically block
transWith acc amount = atomically (transWithdraw acc amount)
transWith2 acc1 acc2 amount = atomically (transWithdraw2 acc1 acc2 amount) 
transDepo acc amount = atomically (transDeposit acc amount)

transTransAcc = transTransfer "acc1" "acc2"
transWith2Acc = transWith2 "acc1" "acc2"

forever :: IO () -> IO ()
forever act = do
  act
  forever act

randomDelay :: IO ()
randomDelay = do
  waitTime <- getStdRandom (randomR (1, 1000000))
  threadDelay waitTime

-- Test function

withdrawer = forkIO (forever (do {transWith2Acc 100; randomDelay }))

depositer = forkIO (forever (do {transDepo "acc1" 50; randomDelay }))

transferer = forkIO (forever (do {transTransAcc 30; randomDelay }))

runTest = do
  withdrawer
  depositer
  transferer
