{-# LANGUAGE TupleSections, ConstraintKinds, TypeFamilies, DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Test where

import Control.Concurrent
import System.Directory
import System.IO.Temp
import Examples.IC.Universal
import Language.Forest.IC hiding (writeFile)

test :: IO ()
test = do
  createDirectoryIfMissing True "/tmp/A"
  createDirectoryIfMissing True "/tmp/B"
  writeFile "/tmp/A/file1.txt" "Two\nLines"
  err <- atomically $ ((do
    (orig :: Universal_d TxVarFS) <- new () "/tmp/A"
    (copy :: Universal_d TxVarFS) <- new () "/tmp/B"
    copyOrElse orig copy "" $ return . show) :: FTM TxVarFS String)
  putStrLn err
