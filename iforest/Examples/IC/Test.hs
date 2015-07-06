{-# LANGUAGE TupleSections, ConstraintKinds, TypeFamilies, DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Test where

import Control.Concurrent
import System.Directory
import System.IO.Temp
import Examples.IC.Universal
import Language.Forest.IC hiding (writeFile)

import Language.Pads.Padsc

test :: IO ()
test = do
  createDirectoryIfMissing True "/tmp/A"
  createDirectoryIfMissing True "/tmp/B"
  writeFile "/tmp/A/file1.txt" "Two\nLines"
  err <- atomically $ ((do
    (orig :: TextFile TxVarFS) <- new () "/tmp/A/file1.txt"
    (copy :: TextFile TxVarFS) <- new () "/tmp/B/file1.txt"
    copyOrElse orig copy "" $ return . show) :: FTM TxVarFS String)
  putStrLn err

testPads :: IO ()
testPads = do
	createDirectoryIfMissing True "/tmp/A"
	createDirectoryIfMissing True "/tmp/B"
	writeFile "/tmp/A/file1.txt" "Two\nLines"
	(rep :: Text,md) <- parseFile "/tmp/A/file1.txt"
	print rep