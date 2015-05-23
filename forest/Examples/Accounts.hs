{-# LANGUAGE ConstraintKinds, TypeFamilies, DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.Accounts where

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


import System.FilePath.Posix
import Control.Monad

import Data.List as List hiding (delete)
import Prelude hiding (read)
import Language.Forest.Forestc
import Data.Map as Map
import Data.Generics

-- * Specification

[pads|
	data AccInfo = AccInfo { accBalance :: Int }
|]

[forest|
	type Bank = Directory { clients is Map [c :: Client | c <- matches <| GL "*" |> ] }
	type Client = Directory {
		  checking :: Accounts
		, savings :: Accounts
	}
	type Accounts = Map [ acc :: Account | acc <- matches <| GL "*.acc" |> ]
	type Account = File AccInfo
|]

-- * Manipulation functions

balance :: String -> IO Int
balance clientid = do
	(bank :: Bank,bank_md) <- load "bank"
	return $ gbalance ((clients bank)!clientid)
	
withdraw :: String -> Int -> IO ()
withdraw clientid amount = do
	(Bank clients,bank_md) <- load "bank"
	let c = clients!clientid
	let totalChecking = gbalance (checking c)
	(savings',checking') <- if (totalChecking >= amount)
		then return (savings c,checking c)
		else transferMany (savings c) (checking c) (amount - totalChecking)
	checking'' <- withdrawMany checking' amount
	let clients' = Map.insert clientid (c { savings = savings', checking = checking'' }) clients
	store (Bank clients',bank_md)

-- transfers money from savings accounts to the first checking account, for a given minimal amount; moves savings balance in its entirety
transferMany :: Accounts -> Accounts -> Int -> IO (Accounts,Accounts)
transferMany (Accounts savings) (Accounts checking) amount = do
	let (k,Account c) = findMin checking
	(savings',c') <- go (Map.toList savings) c amount
	return (Accounts savings',Accounts $ Map.insert k (Account c') checking)
  where
	go [] c a = return (Map.empty,c)
	go ((k,Account s):ss) c a = if (a > 0)
		then do
			let s_balance = accBalance s
			    s' = s { accBalance = 0 }
			    c' = c { accBalance = accBalance c + s_balance }
			(ss',c'') <- go ss c' (a - s_balance)
			return (Map.insert k (Account s') ss',c'')
		else return (Map.insert k (Account s) $ Map.fromList ss,c)

-- withdraws money from a series of accounts
withdrawMany :: Accounts -> Int -> IO Accounts
withdrawMany (Accounts accounts) amount =
	liftM Accounts $ go (Map.toList accounts) amount
  where
	go [] i = if (i == 0) then return Map.empty else error "not enough funds"
	go ((k,Account a):as) i = if (i >= 0)
		then do
			if (accBalance a >= i)
				then return $ Map.insert k (Account (a { accBalance = accBalance a - i })) $ Map.fromList as
				else liftM (Map.insert k (Account (a { accBalance = 0 }))) (go as (i - accBalance a))
		else return $ Map.insert k (Account a) $ Map.fromList as
		
gbalance :: Data a => a -> Int
gbalance = everything (+) (mkQ 0 accBalance)

--- * Threads

main = race_
	(forever $ threadDelay 100 >> balance "nate" >>= print)
	(forever $ threadDelay 100 >> withdraw "nate" 200)

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
	
