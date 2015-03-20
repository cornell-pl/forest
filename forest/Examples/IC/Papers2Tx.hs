{-# LANGUAGE RankNTypes, TupleSections, OverlappingInstances, TypeFamilies, StandaloneDeriving, TypeOperators, ConstraintKinds, DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables, ViewPatterns #-}

module Main where

import qualified Data.ByteString as B
import qualified Control.Exception as E
import Data.Hashable
import Prelude hiding (mod,read,const)
import qualified Prelude
import Control.Monad.Incremental.Adapton
import Control.Monad.Lazy
import Control.Monad.Incremental.Display
import Control.Monad.Reader (ReaderT(..))
import qualified Control.Monad.Reader as Reader
import Language.Forest.Pure.MetaData (cleanFileInfo)
import Data.List
import Data.Char
import Data.Maybe
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.List.Split
import System.Posix.Types
import System.Mem.WeakKey
import Data.Int
import Data.IORef
import Control.Monad.Incremental as Inc hiding (new)
import qualified Data.Set as Set
import Control.Monad.State as State

import System.TimeIt

import Control.Concurrent
import Control.Monad
import System.IO
import System.Directory
import Test.QuickCheck.Gen
import Control.Monad.State (State(..),StateT(..),MonadState(..))
import qualified Control.Monad.State as State
import Control.Monad.Trans
import Data.WithClass.MData
import System.FilePath.Posix
import Data.WithClass.MGenerics
import Control.Monad.Incremental.Draw
import Control.Exception.Base
import Control.Monad.Incremental.Generics
import Control.Monad.Incremental.List
import System.Mem.MemoTable hiding (memo)
import System.IO.Unsafe
import System.Mem.StableName
import System.Mem.Weak as Weak
import qualified System.Mem.WeakTable as WeakTable
import Language.Pads.Padsc as Pads hiding (numErrors)

import Data.Typeable.Internal
import Debug.Trace
import Data.DeepTypeable
import Language.Haskell.TH.Syntax

import Language.Forest.IC hiding (Id)
import Examples.IC.Papers2


papersDefaultRoot = "/Users/hpacheco/Documents/Papers2"
papersNILFSRoot = "/media/hpacheco/nilfs/SmallPapers2"
smallpapersNILFSRoot = "/media/hpacheco/nilfs/SmallPapers2"
myDir = "/home/hpacheco/Forest"

-- transactions

--articlesPaperNamesVar :: Articles TxVarFS -> FTM TxVarFS [String]
--articlesPaperNamesVar articles = do
--	years <- liftM (Map.elems) $ readData articles
--	liftM concat $ mapM yearPaperNamesVar years
--	
--articlesPaperNamesIC :: Articles TxICFS -> FTM TxICFS [String]
--articlesPaperNamesIC articles = do
--	years <- liftM (Map.elems) $ readData articles
--	liftM concat $ mapM yearPaperNamesIC years

articlesPaperNamesNILFS :: Articles TxNILFS -> FTM TxNILFS [String]
articlesPaperNamesNILFS articles = do
	years <- liftM (Map.elems) $ readData articles
	liftM concat $ mapM yearPaperNamesNILFS years

--yearPaperNamesVar :: Year TxVarFS -> FTM TxVarFS [String]
--yearPaperNamesVar year = do
--	authors <- liftM (Map.elems . authors) $ readData year
--	liftM concat $ mapM authorPaperNamesVar authors
--
--yearPaperNamesIC :: Year TxICFS -> FTM TxICFS [String]
--yearPaperNamesIC year = do
--	authors <- liftM (Map.elems . authors) $ readData year
--	liftM concat $ mapM authorPaperNamesIC authors

yearPaperNamesNILFS :: Year TxNILFS -> FTM TxNILFS [String]
yearPaperNamesNILFS year = do
	authors <- liftM (Map.elems . authors) $ readData year
	liftM concat $ mapM authorPaperNamesNILFS authors

--authorPaperNamesVar :: Author TxVarFS -> FTM TxVarFS [String]
--authorPaperNamesVar author = liftM (Map.keys . authorPapers) (readData author)
--
--authorPaperNamesIC :: Author TxICFS -> FTM TxICFS [String]
--authorPaperNamesIC author = do
--	liftM (Map.keys . authorPapers) (readData author)

authorPaperNamesNILFS :: Author TxNILFS -> FTM TxNILFS [String]
authorPaperNamesNILFS author = do
	liftM (Map.keys . authorPapers) (readData author)

--addArticleVar :: YearId -> AuthorId -> FilePath -> Binary -> FTM TxVarFS ()
--addArticleVar year author name content = do
--	let path = papersDefaultRoot </> "Articles" </> Pads.printRep year </> Pads.printRep author </> name
--	paper :: Paper TxVarFS <- new (year :*: author) path
--	let paper_md = (cleanFileInfo path,cleanBasePD)
--	writeOrThrow paper (paper_md,content) E.Deadlock
--
--addArticleIC :: YearId -> AuthorId -> FilePath -> Binary -> FTM TxICFS ()
--addArticleIC year author name content = do
--	let path = papersDefaultRoot </> "Articles" </> Pads.printRep year </> Pads.printRep author </> name
--	paper :: Paper TxICFS <- new (year :*: author) path
--	let paper_md = (cleanFileInfo path,cleanBasePD)
--	writeOrThrow paper (paper_md,content) E.Deadlock

addArticleNILFS :: YearId -> AuthorId -> FilePath -> Binary -> FTM TxNILFS ()
addArticleNILFS year author name content = do
	let path = papersNILFSRoot </> "Articles" </> Pads.printRep year </> Pads.printRep author </> name
	paper :: Paper TxNILFS <- new (year :*: author) path
	let paper_md = (cleanFileInfo path,cleanBasePD)
	writeOrThrow paper (paper_md,content) E.Deadlock

--mainVar :: IO ()
--mainVar = timeIt $ do
--	names <- atomically $ do
--		papers2 :: Papers2 TxVarFS <- new () papersDefaultRoot
--		Just articles2 <- liftM articles $ readData papers2
--		names <- articlesPaperNamesVar articles2
----		forestDrawToPDF "" proxyTxVarFS papers2 "/Users/hpacheco/Desktop/graph.pdf"
--
--		addArticleVar (Id 2015) (Id "Forest") "forest.pdf" (Binary B.empty)
--	
--		Just articles2 <- liftM articles $ readData papers2
--		names2 <- articlesPaperNamesVar articles2
--
--		return (names,names2)
--
----		return names
--	print names
--
--mainIC :: IO ()
--mainIC = timeIt $ do
--	names <- atomicallyTxICFS papersDefaultRoot $ do
--		papers2 :: Papers2 TxICFS <- new () papersDefaultRoot
--		Just articles2 <- liftM articles $ readData papers2
--		names <- articlesPaperNamesIC articles2
----		forestDrawToPDF "" proxyTxVarFS papers2 "/Users/hpacheco/Desktop/graph.pdf"
--		
--		addArticleIC (Id 2015) (Id "Forest") "forest.pdf" (Binary B.empty)
--	
--		Just articles2 <- liftM articles $ readData papers2
--		names2 <- articlesPaperNamesIC articles2
--
--		return (names,names2)
--
----		return names
--		
--	print names

--mainIC2 :: IO ()
--mainIC2 = timeIt $ do
--	names <- atomicallyTxICFS papersDefaultRoot $ do
--		papers2 :: Papers2 TxICFS <- new () papersDefaultRoot
--		Just articles2 <- liftM articles $ readData papers2
--		articlesPaperNamesIC articles2
--	print names
--	
--	names2 <- atomicallyTxICFS papersDefaultRoot $ do
--		addArticleIC (Id 2015) (Id "Forest") "forest.pdf" (Binary B.empty)
--		papers2 :: Papers2 TxICFS <- new () papersDefaultRoot
--		Just articles2 <- liftM articles $ readData papers2
--		articlesPaperNamesIC articles2
--	print names2

mainNILFS :: IO ()
mainNILFS = timeIt $ do
	res <- atomicallyTxNILFS papersNILFSRoot $ do
		papers2 :: Papers2 TxNILFS <- new () papersNILFSRoot
		Just articles2 <- liftM articles $ readData papers2
		names <- articlesPaperNamesNILFS articles2
	
--		addArticleNILFS (Id 2015) (Id "Forest") "forest.pdf" (Binary B.empty)
		papers2 :: Papers2 TxNILFS <- new () papersNILFSRoot
		Just articles2 <- liftM articles $ readData papers2
		names2 <- articlesPaperNamesNILFS articles2
		return (names,names2)
	print res

mainNILFS2 :: IO ()
mainNILFS2 = timeIt $ do
	names <- atomicallyTxNILFS papersNILFSRoot $ do
		papers2 :: Papers2 TxNILFS <- new () papersNILFSRoot
		Just articles2 <- liftM articles $ readData papers2
		articlesPaperNamesNILFS articles2
	print names
	
	names2 <- atomicallyTxNILFS papersNILFSRoot $ do
		addArticleNILFS (Id 2015) (Id "Forest") "forest.pdf" (Binary B.empty)
		papers2 :: Papers2 TxNILFS <- new () papersNILFSRoot
		Just articles2 <- liftM articles $ readData papers2
		articlesPaperNamesNILFS articles2
	print names2

main = mainNILFS


