{-# LANGUAGE ConstraintKinds, TypeFamilies, DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.IC.Beautiful.Accounts where

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
import Control.Monad

import Control.Monad.Incremental.Display
import Data.List as List hiding (delete)
import Control.Monad.Incremental hiding (read,new)
import Prelude hiding (read)
import Language.Forest.IC hiding (writeFile)
import Data.Map as Map
import Data.WithClass.MGenerics

-- * Specification

[ipads|
	data AccInfo = AccInfo { accBalance :: Int }
|]

[iforest|
	data Bank = Directory { clients is Map [c :: Client | c <- matches (GL "*") ] }
	data Client = Directory {
		  savings :: Accounts
		, checking :: Accounts
	}
	data Accounts = Map [ acc :: Account | acc <- matches (GL "*.acc") ]
	data Account = File AccInfo
|]

-- * Manipulation functions

bankClient clientid = do
	bank :: Bank TxVarFS <- new () "bank"
	liftM ((!clientid) . clients) (readData bank)

balance :: String -> FTM TxVarFS Int
balance clientid = bankClient clientid >>= gbalance
	
withdraw :: String -> Int -> FTM TxVarFS ()
withdraw clientid amount = do
	client <- bankClient clientid
	c <- readData client
	totalChecking <- gbalance (checking c)
	unless (totalChecking >= amount) $ transferMany (savings c) (checking c) (amount - totalChecking)
	withdrawMany (checking c) amount

-- transfers money from savings accounts to the first checking account, for a given minimal amount; moves savings balance in its entirety
transferMany savings checking amount = do
	ss <- readData savings
	cs <- readData checking
	go (Map.elems ss) (snd $ findMin cs) amount
  where
	go [] c a = return ()
	go (s:ss) c a = when (a > 0) $ do
		(s_md,s_d) <- read s
		let s_balance = accBalance s_d
		writeOrError s (s_md,s_d { accBalance = 0 }) ""
		(c_md,c_d) <- read c
		writeOrError c (c_md,c_d { accBalance = accBalance c_d + s_balance }) ""
		go ss c (a-s_balance)

-- withdraws money from a series of accounts
withdrawMany accounts amount = do
	as <- readData accounts
	go (Map.elems as) amount
  where
	go [] i = unless (i == 0) $ error "not enough funds"
	go (a:as) i = when (i >= 0) $ do
		(a_md,a_d) <- read a
		if (accBalance a_d >= i)
			then writeOrError a (a_md,a_d { accBalance = accBalance a_d - i }) ""
			else writeOrError a (a_md,a_d { accBalance = 0 }) "" >> go as (i - accBalance a_d)
		
gbalance :: (MData NoCtx (l inc) a) => a -> l inc Int
gbalance = everything proxyNoCtx (\x y -> return (x+y)) (mkQ 0 (return . accBalance))

-- * Threads

main = race_
	(forever $ putStrLn "balance" >> atomically (balance "nate") >>= print)
	(forever $ putStrLn "withdraw" >> threadDelay 100 >> atomically (withdraw "nate" 200))

-- * Data Generation

genBank = do
	createDirectoryIfMissing True "bank"
	createDirectoryIfMissing True "bank/nate"
	createDirectoryIfMissing True "bank/nate/savings"
	writeFile "bank/nate/savings/s1.acc" "5000"
	writeFile "bank/nate/savings/s2.acc" "5000"
	createDirectoryIfMissing True "bank/nate/checking"
	writeFile "bank/nate/checking/c1.acc" "500"
	writeFile "bank/nate/checking/c2.acc" "500"
	
