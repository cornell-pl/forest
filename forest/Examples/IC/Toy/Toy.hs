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
import Control.Monad.Incremental hiding (read,new)
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
[txforest|
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
  str <- atomically $ do
    (rep :: Toy1_d TxVarFS) <- new () toyDir
    (_,Toy1_d_inner a b) <- read rep
    (fmd,(Text binit,bmd)) <- read b
    tryWrite b (fmd,(Text $ B.pack "B declares victory",bmd))
    (fmd,(Text binit,bmd)) <- read b -- If you do not read, binit doesn't update
    return $ show binit
  putStrLn str
  
-- Tests variables escaping transactions - No change
toy2 :: IO ()
toy2 = do
  (a,aval) <- atomically $ do
    (rep :: Toy1_d TxVarFS) <- new () toyDir
    (fmd,Toy1_d_inner a b) <- read rep
    (fmd,(Text ainit,bmd)) <- read a
    return (a,show ainit)
  putStrLn aval
  putStrLn "Arbitrary things could happen here (but don't)"
  str <- atomically $ do
    (fmd,(Text afinal,bmd)) <- read a
    return $ show afinal
  putStrLn str

-- Tests variables escaping transactions - Change
toy3 :: IO ()
toy3 = do
  (a,aval) <- atomically $ do
    (rep :: Toy1_d TxVarFS) <- new () toyDir
    (fmd,Toy1_d_inner a b) <- read rep
    (fmd,(Text ainit,bmd)) <- read a
    return (a,show ainit)
  putStrLn aval
  putStrLn "Arbitrary things do happen here"
  val <- randomNum
  atomically $ do
    (fmd,(_,bmd)) <- read a
    tryWrite a (fmd,((Text $ B.pack $ show val),bmd))
  str <- atomically $ do
    (fmd,(Text afinal,bmd)) <- read a
    return $ show afinal
  putStrLn str

-- Tests variables escaping transactions - Change, without forcing
toy4 :: IO ()
toy4 = do
  a <- atomically $ do
    (rep :: Toy1_d TxVarFS) <- new () toyDir
    (fmd,Toy1_d_inner a b) <- read rep
    return a
  putStrLn "Arbitrary things do happen here"
  val <- randomNum
  atomically $ do
    (rep :: Toy1_d TxVarFS) <- new () toyDir
    (fmd,Toy1_d_inner c d) <- read rep
    (fmd,(_,bmd)) <- read c
    tryWrite c (fmd,((Text $ B.pack $ show val),bmd))
  str <- atomically $ do
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