{-# LANGUAGE TupleSections, FlexibleContexts, ScopedTypeVariables, GADTs, FlexibleInstances,MultiParamTypeClasses,UndecidableInstances, ViewPatterns #-}

module Language.Forest.IO.Storing where

import Prelude hiding (const,read,mod)
import qualified Prelude
import Language.Forest.IO.Utils
import Language.Forest.Syntax
import Language.Forest.FS.Diff
import Language.Forest.ValueDelta
import Language.Pads.Padsc
import Language.Forest.MetaData
import Language.Forest.Generic
import Language.Forest.Errors
import Language.Forest.FS.FSDelta
import Language.Forest.MetaData
import Control.Monad.Incremental hiding (memo)
import Data.IORef

import qualified System.FilePath.Posix
import System.FilePath.Glob
import System.Posix.Env
import System.Posix.Files
import System.Process
import System.Exit
import System.Directory
import System.IO
import System.IO.Unsafe
import Text.Regex
import System.FilePath.Posix
import Control.Monad
import Data.List
import Language.Forest.FS.FSRep
--import Control.Monad.IO.Class
import Data.WithClass.MData

import Language.Forest.IO.Memo

import qualified Control.Exception as CE

import Data.Data
import Data.Maybe
import System.Random
import Data.Proxy

