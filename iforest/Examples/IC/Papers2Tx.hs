{-# LANGUAGE RankNTypes, TupleSections, OverlappingInstances, TypeFamilies, StandaloneDeriving, TypeOperators, ConstraintKinds, DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables, ViewPatterns #-}

module Main where

import qualified Data.ByteString as B
import qualified Control.Exception as E
import Data.Hashable
import Prelude hiding (mod,read,const)
import qualified Prelude
import Control.Monad.Incremental.Adapton hiding (new)

import Control.Monad.Incremental.Display
import Control.Monad.Reader (ReaderT(..))
import qualified Control.Monad.Reader as Reader
--import Language.Forest.Pure.MetaData (cleanFileInfo)
import Data.List
import Data.Char
import Data.Maybe
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.List.Split
import System.Posix.Types

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

--import qualified System.Mem.MemoTable as MemoTable
import System.IO.Unsafe
import System.Mem.StableName
import System.Mem.Weak as Weak
--import qualified System.Mem.WeakTable as WeakTable
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

--main = mainNILFS
main = mainGen

nilfsCfg = NILFSForestConfig { rootPath = papersNILFSRoot, forestDir = myDir, incParams = defaultIncParams }

mainGen :: IO ()
mainGen = runIncrementalWithParams (NILFSIncParams nilfsCfg) $ do
	papers2 :: Papers2 NILFS <- new () papersNILFSRoot
	topk <- inside $ topkProlificAuthors 3 papers2
	str <- display topk
	forestM $ forestIO $ putStrLn str

-- * Generic forest queries

articlePaperNamesWithYear :: (MData (MemoCtx NoCtx) (ForestI NILFS) (Papers2 NILFS),MData (MemoCtx NoCtx) (ForestI NILFS) (Articles NILFS),MData (MemoCtx NoCtx) (ForestI NILFS) (Year NILFS))
	=> Integer -> Papers2 NILFS -> ForestI NILFS (ForestJoinListICThunkI NILFS FilePath)
articlePaperNamesWithYear year = articlePapersWithYear year >=> mapJoinListInc (\paper -> liftM (fullpath . fst) $ readMeta paper)

articlePapersWithYear :: (MData (MemoCtx NoCtx) (ForestI NILFS) (Papers2 NILFS),MData (MemoCtx NoCtx) (ForestI NILFS) (Articles NILFS),MData (MemoCtx NoCtx) (ForestI NILFS) (Year NILFS))
	=> Integer -> Papers2 NILFS -> ForestI NILFS (ForestJoinListICThunkI NILFS (Paper NILFS))
articlePapersWithYear = garticlePapersWithYear proxyNoCtx

-- recursive query that collects papers within specific years within articles
garticlePapersWithYear :: (Typeable ctx,MData (MemoCtx ctx) (ForestI NILFS) (Articles NILFS),MData (MemoCtx ctx) (ForestI NILFS) (Year NILFS))
	=> Proxy ctx -> Integer -> ForestGenericQMemo ctx NILFS Inside (ForestJoinListICThunkI' NILFS (Paper NILFS))
garticlePapersWithYear ctx year = everythingButInc ctx joinListInc'' $ mkQButTypeInc (return EmptyMod) (f ctx) where
	f :: (Typeable ctx,MData (MemoCtx ctx) (ForestI NILFS) (Articles NILFS),MData (MemoCtx ctx) (ForestI NILFS) (Year NILFS))
	  => Proxy ctx -> Articles NILFS -> ForestI NILFS (ForestJoinListICThunkI' NILFS (Paper NILFS))
	f ctx x = gpapersWithYear ctx year x >>= force

-- recursive query that collects papers within specific years
gpapersWithYear :: (Typeable ctx,MData (MemoCtx ctx) (ForestI NILFS) (Year NILFS))
	=> Proxy ctx -> Integer -> ForestGenericQMemo ctx NILFS Inside (ForestJoinListICThunkI' NILFS (Paper NILFS))
gpapersWithYear ctx year = everythingButInc ctx joinListInc'' $ mkQButTypeInc (return EmptyMod) (f ctx) where
	f :: (Typeable ctx,MData (MemoCtx ctx) (ForestI NILFS) (Year NILFS))
	  => Proxy ctx -> Year NILFS -> ForestI NILFS (ForestJoinListICThunkI' NILFS (Paper NILFS))
	f ctx y = do
		(yearid,_) <- args y
		case yearid of
			Id ((==year) -> True) -> gpapers ctx y >>= force
			otherwise -> return EmptyMod

-- recursive query that collects papers
gpapers :: Typeable ctx => Proxy ctx -> ForestGenericQMemo ctx NILFS Inside (ForestJoinListICThunkI' NILFS (Paper NILFS))
gpapers ctx = listifyInc ctx (\(x :: Paper NILFS) -> return True)

paperNames :: Author NILFS -> ForestI NILFS (ForestJoinListICThunkI NILFS FilePath)
paperNames x = everythingButInc proxyNoCtx joinListInc''
	(mkQButTypeInc (return EmptyMod) (\(paper::Paper NILFS) -> liftM (SingleMod . fullpath . fst) $ readMeta paper))
	x
	
papersNames :: Papers2 NILFS -> ForestI NILFS (ForestJoinListICThunkI NILFS FilePath)
papersNames x = everythingButInc proxyNoCtx joinListInc''
	(mkQButTypeInc (return EmptyMod) $ \(paper::Paper NILFS) -> liftM (SingleMod . fullpath . fst) $ readMeta paper)
	x

--joinListInc'' tx my = thunk my >>= \ty -> return $ JoinMod tx ty
joinListInc'' tx my = thunk my >>= \ty -> joinListPruneInc tx ty >>= force

paperCount :: Papers2 NILFS -> ForestI NILFS (ForestICThunkI NILFS Int)
paperCount papers2 = everythingButInc proxyNoCtx
	(\tx my -> force tx >>= \x -> liftM (x+) my)
	(mkQButTypeInc (return 0) (\(paper::Paper NILFS) -> return 1))
	papers2

topkLargestFiles :: Int -> Papers2 NILFS -> ForestI NILFS (ForestListICThunkI NILFS (String,Int))
topkLargestFiles k papers2 = fileSizes papers2 >>= takeInc k

-- | computes a list of (authorname,numpapers) sorted from most to least prolific author
fileSizes :: Papers2 NILFS -> ForestI NILFS (ForestListICThunkI NILFS (String,Int))
fileSizes x = everythingButInc proxyNoCtx (mergeInc' cmp) q x
  where
	cmp (name1,count1) (name2,count2) = return $ compare count2 count1 -- sort in reverse by highest count
	q :: GenericQ (MemoCtx NoCtx) (ForestI NILFS) (ForestListICThunkI NILFS (String,Int),Bool)
	q = mkQButTypeInc (return NilMod) $ \(bin::BinaryFile NILFS) -> readMeta bin >>= \(info,_) -> do
		liftM (ConsMod (fullpath info,fromEnum $ size info)) $ const NilMod

-- merge two sorted lists
mergeInc' :: (IncK inc (ListMod' mod l inc a),Memo (ListMod mod l inc a),Eq a,Output mod l inc,Eq (ListMod mod l inc a))
	=> (a -> a -> l inc Ordering) -> ListMod mod l inc a -> l inc (ListMod' mod l inc a) -> l inc (ListMod' mod l inc a)
mergeInc' cmp tx my = thunk my >>= mergeInc cmp tx >>= force

topkProlificAuthors :: Int -> Papers2 NILFS -> ForestI NILFS (ForestListICThunkI NILFS (String,Int))
topkProlificAuthors k papers2 = authorPaperCount papers2 >>= takeInc k

authorPaperCount :: Papers2 NILFS -> ForestI NILFS (ForestListICThunkI NILFS (String,Int))
authorPaperCount x = everythingButInc proxyNoCtx
	(mergeMapInc' cmp mrg)
	q
	x
  where
	cmp (name1,count1) (name2,count2) = compare count2 count1 -- sort in reverse by highest count
	mrg count1 count2 = return $ count1 + count2 -- sum counts for the same author
	q :: GenericQ (MemoCtx NoCtx) (ForestI NILFS) (ForestListICThunkI NILFS (String,Int),Bool)
	q = mkQButTypeInc (return NilMod) $ \(author::Author NILFS) -> do
		(yearid :*: authorid,_) <- args author
		case authorid of
			Unknown -> return NilMod
			Id name -> do
				count <- liftM (Map.size . authorPapers) $ readData author
				liftM (ConsMod (name,count)) $ const NilMod

mergeMapInc' :: (IncK inc (ListMod' mod l inc (k, v)),IncK inc (Maybe (k, v)),Eq (ListMod mod l inc (ListMod mod l inc (k, v))),Hashable (ListMod mod l inc (ListMod mod l inc (k, v))),Memo k,Memo v,Memo (ListMod mod l inc (ListMod mod l inc (k, v))),Memo (ListMod mod l inc (k,v)),Eq k,Eq v,Output mod l inc,Eq (ListMod mod l inc (k,v)))
	=> ((k,v) -> (k,v) -> Ordering) -> (v -> v -> l inc v) -> ListMod mod l inc (k,v) -> l inc (ListMod' mod l inc (k,v)) -> l inc (ListMod' mod l inc (k,v))
mergeMapInc' cmp mrg tx my = thunk my >>= mergeMapInc cmp mrg tx >>= force
