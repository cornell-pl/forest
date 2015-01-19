{-# LANGUAGE DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.IC.Toy.Toy where

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
import System.TimeIt
import Control.Monad.IO.Class
import Data.WithClass.MData
import Data.DeepTypeable

import Data.WithClass.Derive.DeepTypeable
import Data.DeriveTH
import Data.WithClass.Derive.MData
import System.FilePath.Posix

import Control.Monad.Incremental.Display
import Data.List as List
import Control.Monad.Incremental hiding (read)
import Prelude hiding (read)
import Language.Forest.IC hiding (writeFile)
import Data.ByteString.Char8 as B hiding (putStrLn)


-- [pads|
-- 	data Account = Account Int
-- |]

-- $( derive makeMData ''Account )
-- $( derive makeMData ''Account_imd )
-- $( derive makeDeepTypeable ''Account )
-- $( derive makeDeepTypeable ''Account_imd )

-- TextFile = File Text
[iforest|
	type Toy1_d = Directory {
		a is "a" :: TextFile
              , b is "b" :: TextFile
	} 
	type Toy2_d = Directory {
		c is "a" :: TextFile
              , d is "a" :: TextFile
	} 
|]

-- Change this to the forest directory to make the example work for you!
--Hugo: made it a relative path (it is fine as long as we use the Makefile)
rootDir = "."

toyDir = rootDir </> "Examples/IC/Toy/toyex"

-- Tests storing to a variable then printing it
toy1 :: IO ()
toy1 = do
  str <- atomically () toyDir $ \ (rep :: Toy1_d TxVarFS) -> do
    (_,Toy1_d_inner a b) <- read rep
    (fmd,(Text binit,bmd)) <- read b
    tryWrite b (fmd,(Text $ B.pack "B declares victory",bmd))
    (fmd,(Text binit,bmd)) <- read b -- If you do not read, binit doesn't update
    return $ show binit
  putStrLn str
  
-- Tests variables escaping transactions - No change
toy2 :: IO ()
toy2 = do
  (a,aval) <- atomically () toyDir $ \ (rep :: Toy1_d TxVarFS) -> do
    (fmd,Toy1_d_inner a b) <- read rep
    (fmd,(Text ainit,bmd)) <- read a
    return (a,show ainit)
  putStrLn aval
  putStrLn "Arbitrary things could happen here (but don't)"
  str <- atomically () toyDir $ \ (rep :: Toy1_d TxVarFS) -> do
    (fmd,(Text afinal,bmd)) <- read a
    return $ show afinal
  putStrLn str

-- Tests variables escaping transactions - Change
toy3 :: IO ()
toy3 = do
  (a,aval) <- atomically () toyDir $ \ (rep :: Toy1_d TxVarFS) -> do
         (fmd,Toy1_d_inner a b) <- read rep
         (fmd,(Text ainit,bmd)) <- read a
         return (a,show ainit)
  putStrLn aval
  putStrLn "Arbitrary things do happen here"
  val <- randomNum
  atomically () toyDir $ \ (rep :: Toy1_d TxVarFS) -> do
         (fmd,(_,bmd)) <- read a
         tryWrite a (fmd,((Text $ B.pack $ show val),bmd))
  str <- atomically () toyDir $ \ (rep :: Toy1_d TxVarFS) -> do
         (fmd,(Text afinal,bmd)) <- read a
         return $ show afinal
  putStrLn str

-- Tests variables escaping transactions - Change, without forcing
toy4 :: IO ()
toy4 = do
  a <- atomically () toyDir $ \ (rep :: Toy1_d TxVarFS) -> do
         (fmd,Toy1_d_inner a b) <- read rep
         return a
  putStrLn "Arbitrary things do happen here"
  val <- randomNum
  atomically () toyDir $ \ (rep :: Toy1_d TxVarFS) -> do
         (fmd,Toy1_d_inner c d) <- read rep
         (fmd,(_,bmd)) <- read c
         tryWrite c (fmd,((Text $ B.pack $ show val),bmd))
  str <- atomically () toyDir $ \ (rep :: Toy1_d TxVarFS) -> do
         (fmd,(Text afinal,bmd)) <- read a
         return $ show afinal
  putStrLn str

-- Helpers

check :: Bool -> FTM TxVarFS ()
check True = return ()
check False = retry

forever :: IO () -> IO ()
forever act = do
 act
 forever act

randomNum :: IO Int
randomNum = do
  getStdRandom $ randomR (1,100)

randomDelay :: IO ()
randomDelay = do
 waitTime <- getStdRandom $ randomR (1000000, 10000000)
 threadDelay waitTime

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