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

main = race_ (forever $ atomically $ balance "nate") (forever $ atomically $ withdraw "nate" 200)

bankClient :: String -> FTM TxVarFS (Client TxVarFS)
bankClient clientid = do
	bank :: Bank TxVarFS <- new () "."
	Bank_inner clients <- readData bank
	return (clients!clientid)

balance :: String -> FTM TxVarFS Int
balance clientid = do
	client <- bankClient clientid
	totalBalance client
	
withdraw :: String -> Int -> FTM TxVarFS ()
withdraw clientid amount = do
	client <- bankClient clientid
	client_info <- readData client
	let clientSavings = savings client_info
	let clientChecking = checking client_info
	totalChecking <- totalBalance clientChecking
	unless (totalChecking >= amount) $ transferMany clientSavings clientChecking (amount - totalChecking)
	withdrawMany clientChecking amount

-- transfers money from savings to checking accounts, for a given minimal amount
transferMany :: Accounts TxVarFS -> Accounts TxVarFS -> Int -> FTM TxVarFS ()
transferMany savings checking amount = do
	savings_info <- readData savings
	checking_info <- readData checking
	let checkingAcc = snd $ findMin checking_info
	go (Map.elems savings_info) checkingAcc amount
  where
	go [] c a = return ()
	go (s:ss) c a = when (a > 0) $ do
		s_balance <- liftM accBalance (readData s)
		(c_md,c_d) <- read c
		writeOrRetry c (c_md,c_d { accBalance = accBalance c_d + s_balance }) ()
		go ss c (a-s_balance)

-- withdraws money from a series of accounts
withdrawMany :: Accounts TxVarFS -> Int -> FTM TxVarFS ()
withdrawMany accounts amount = do
	accs_info <- readData accounts
	go (Map.elems accs_info) amount
  where
	go [] i = unless (i == 0) $ error "not enough funds"
	go (a:as) i = when (i >= 0) $ do
		(a_md,a_d) <- read a
		if (accBalance a_d >= i)
			then writeOrRetry a (a_md,a_d { accBalance = accBalance a_d - i }) ()
			else writeOrRetry a (a_md,a_d { accBalance = 0 }) () >> go as (i - accBalance a_d)
		

totalBalance :: (MData NoCtx (l inc) a) => a -> l inc Int
totalBalance = everything proxyNoCtx
	(\x y -> return (x+y))
	(mkQ 0 (return . accBalance))


