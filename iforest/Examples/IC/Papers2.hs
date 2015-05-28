{-# LANGUAGE RankNTypes, TupleSections, OverlappingInstances, TypeFamilies, StandaloneDeriving, TypeOperators, ConstraintKinds, DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables, ViewPatterns #-}

module Examples.IC.Papers2 where

import qualified Data.ByteString as B
import qualified Control.Exception as E
import Data.Hashable
import Prelude hiding (mod,read,const)
import qualified Prelude
import Control.Monad.Incremental.Adapton

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
import System.Mem.MemoTable hiding (memo)
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

[ipads|
	data Id a = Id a | Unknown "Unknown"
	type AuthorId = Id String
	type YearId = Id Integer
|]

[iforest|
	data Paper (y :: YearId) (a :: AuthorId) = File Binary where (isPaperOf (fullpath this_att) y a)

    data Supplemental (y :: YearId) (a :: AuthorId) = Directory {
		supplementalFiles is Map [ p :: Paper y a | p <- matches (GL "*"), (isNotHidden p) ] 
	} 

	data Author (y :: YearId) (a :: AuthorId) = Directory {
		authorPapers is Map [ p :: Paper y a | p <- matches (GL "*"), (isNotHidden p) ] 
	,   supplemental is "Supplemental" :: Maybe (Supplemental y a)
	}

	data Year (y :: YearId) = Directory {
		authors is Map [ a :: Author y a | a :: AuthorId <- matches (GL "*"), (isNotHidden $ show a) ]
	}

	data Articles = Map [ y :: Year y | y :: YearId <- matches yearRE ] where True
	data Books    = Map [ y :: Year y | y :: YearId <- matches yearRE ]
	data Media    = Map [ y :: Year y | y :: YearId <- matches yearRE ]
	data Reports  = Map [ y :: Year y | y :: YearId <- matches yearRE ]

	data Library (articles :: [String]) = Directory {
		database is "Database.papersdb" :: BinaryFile
	}

	data Papers2 = Directory {
		articles is "Articles" :: Maybe Articles
	,   books is "Books" :: Maybe Books
	,   media is "Media" :: Maybe Media
	,   reports is "Reports" :: Maybe Reports where True
	,	library matches libraryRE :: Maybe (Library <| allPaperNames articles books media reports |>)
	} where True
|]

libraryRE = RE "Library.papers2|Library.papers"

allPaperNames articles books media reports = return []

hidden :: String -> Bool
hidden = isPrefixOf "."

isNotHidden :: String -> Bool
isNotHidden p = not (hidden p)

fromId :: Id a -> Maybe a
fromId (Id x) = Just x
fromId Unknown = Nothing

isPaperOf :: FilePath -> YearId -> AuthorId -> Bool
isPaperOf (toLowerString -> file) (fromId -> myear) ((fmap toLowerString . fromId) -> mname) = isSuffixOf suffix filename || (isSuffixOf suffix name && isNumberString num)
    where (filename,ext) = splitExtension (takeFileName file)
          (name,num) = splitOnTwoRear "-" filename -- when multiple papers with the same year and author exist
          suffix = foldr1Safe (\x y -> x ++" "++y) (maybeToList (fmap show myear) ++ maybeToList mname)

splitOnTwoRear :: String -> String -> (String,String)
splitOnTwoRear tok str = case toks of
	[x] -> (x,"")
	otherwise -> (foldr1 (\x y -> x++tok++y) $ init toks,last toks)
	where toks = splitOn tok str

yearRE = RE "[0-9][0-9][0-9][0-9]|Unknown"

getYear :: String -> Maybe Integer
getYear "Unknown" = Nothing
getYear str = Just $ Prelude.read str

getAuthor :: String -> Maybe String
getAuthor "Unknown" = Nothing
getAuthor str = Just str

isPDF :: String -> Bool
isPDF str = map toLower str == "pdf"

isFile :: FileInfo -> Bool
isFile att = kind att /= DirectoryK

isNumberString :: String -> Bool
isNumberString = and . map isNumber

toLowerString :: String -> String
toLowerString = map toLower

foldr1Safe f [] = ""
foldr1Safe f l = foldr1 f l

--papersErrors :: ICRep fs => ((Papers2 fs,Papers2_md fs),LoadInfo fs) -> ForestO fs ()
--papersErrors ((rep,md),_) = do
--	err <- get_errors md
--	forestM $ forestIO $ print (numErrors err)
--	forestM $ forestIO $ print (errorMsg err)
--
--timeForestNILFS (NILFSForestO m) = NILFSForestO $ Reader.mapReaderT (Outer . timeIt . runOuter) m
--
--papers2_dot :: Bool -> FilePath -> ForestO NILFS ()
--papers2_dot toPDF papersRoot = do
----	let (draw,ext) = if toPDF then (forestDrawToPDF,"pdf") else (forestDrawToDot,"dot")
--	(dta@(repmd@(rep,md),_),thunk) <- timeForestNILFS $ do
--		dta@(repmd@(rep::Papers2 NILFS,md::Papers2_md NILFS),_) <- load () papersRoot
----		papersErrors dta
----		thunk <- inside $ topkProlificAuthors 3 md
--		thunk <- inside $ topkLargestFiles 3 md
----		thunk <- inside $ paperCount rep -- counts the number of papers
----		thunk <- inside $ articlePaperNamesWithYear 2005 (rep :.: md) -- returns articles published in 2005
--		forestM $ forestIO $ putStrLn "drawing"
----		draw proxyNILFS (Merge repmd thunk) $ addExtension "/home/hpacheco/0" ext
--		displayAs "Load: " thunk
--		forestM $ forestIO $ putStrLn "drawing"
----		draw proxyNILFS (Merge repmd thunk) $ addExtension "/home/hpacheco/1" ext
--		return (dta,thunk)
----	mdToPDF (snd repmd) "/home/hpacheco/papers2.pdf"
----	forestPrint rep
----	forestPrint md
--	
--	forever $ do
--		forestM $ forestIO $ putStrLn "Press ENTER for reloading"
--		forestM $ forestIO $ getLine
--		timeForestNILFS $ do
--			reload papersRoot dta
--			forestM $ forestIO $ putStrLn "drawing"
----			draw proxyNILFS (Merge repmd thunk) $ addExtension "/home/hpacheco/2" ext
--			forestM $ forestIO $ putStrLn "counting"
----			papersErrors dta
--			displayAs "Reload: " thunk
----			liftIO $ putStrLn "Printing result NOW..."
----			forestPrint md
--			forestM $ forestIO $ putStrLn "drawing"
----			draw proxyNILFS (Merge repmd thunk) $ addExtension "/home/hpacheco/3" ext
--			return ()
--	return ()
--	
----papers2_between :: FSTree NILFS -> FSTree NILFS -> Bool -> FilePath -> ForestO NILFS ()
----papers2_between oldTree newTree toPDF papersRoot = do
------	let (draw,ext) = if toPDF then (forestDrawToPDF,"pdf") else (forestDrawToDot,"dot")
----	(dta@(repmd@(rep,md),_),thunk) <- timeForestO $ do
----		dta@(repmd@(rep::Papers2 NILFS,md::Papers2_md NILFS),_) <- loadTree oldTree () papersRoot
------		papersErrors dta
------		thunk <- inside $ articlePaperNamesWithYear 2005 (rep :.: md)
----		thunk <- inside $ authorPaperCount md
----		liftIO $ putStrLn "drawing0"
------		draw proxyNILFS (Merge repmd thunk) $ addExtension "/home/hpacheco/0" ext
----		liftIO $ putStrLn "evaluating"
----		display thunk
----		liftIO $ putStrLn "drawing1"
------		draw proxyNILFS (Merge repmd thunk) $ addExtension "/home/hpacheco/1" ext
----		return (dta,thunk)
------	mdToPDF (snd repmd) "/home/hpacheco/papers2.pdf"
------	forestPrint rep
------	forestPrint md
----	
----	do
----		liftIO $ putStrLn "Press ENTER for reloading"
----		liftIO $ getLine
----		timeForestO $ do
----			reloadTree newTree Proxy papersRoot dta
----			liftIO $ putStrLn "drawing2"
------			draw proxyNILFS (Merge repmd thunk) $ addExtension "/home/hpacheco/2" ext
----			liftIO $ putStrLn "counting"
------			papersErrors dta
----			display thunk
------			liftIO $ putStrLn "Printing result NOW..."
------			forestPrint md
----			liftIO $ putStrLn "drawing3"
------			draw proxyNILFS (Merge repmd thunk) $ addExtension "/home/hpacheco/3" ext
----			return ()
----	return ()
--
----papers2_dot_Default = timeIt $ flip finally (mergePDFs $ papersDefaultRoot </> "forest.pdf") $ runForest DefaultForestCfg $ papers2_dot Proxy papersDefaultRoot
--papers2_dot_NILFS = do
--	let path = papersNILFSRoot
--	runIncrementalForest (NILFSForestConfig False path myDir) $ papers2_dot True path
--smallpapers2_pdf_NILFS = runIncrementalForest (NILFSForestConfig False smallpapersNILFSRoot myDir) $ papers2_dot True smallpapersNILFSRoot
--
--main = papers2_dot_NILFS
--
----smallpapers2_static = do
----	time116 <- readNILFSTime "2014-10-16 13:53:09"
----	time120 <- readNILFSTime "2014-10-16 14:01:41"
----	time154 <- readNILFSTime "2014-10-17 09:43:30"
----	time391 <- readNILFSTime "2014-10-21 11:25:33"
----	time393 <- readNILFSTime "2014-10-21 11:38:55"
----	time397 <- readNILFSTime "2014-10-21 11:48:19"
----	let path = smallpapersNILFSRoot
----	runForest (NILFSForestConfig path myDir) $ papers2_between (NILFSTree 393 time391) (NILFSTree 397 time393) True path
--
--testD = inDeepTypeable (Proxy :: Proxy (Articles NILFS :.: Articles_md NILFS)) (Proxy :: Proxy (Papers2 NILFS :.: Papers2_md NILFS))
--testD2 = inDeepTypeable (Proxy :: Proxy (Author_md NILFS)) (Proxy :: Proxy (Articles_md NILFS))
--
----showTypeTree :: TypeTree -> String
----showTypeTree (MkTypeTree name args _) = "(" ++ foldl1 (\s1 s2 -> s1 ++' ':s2) (showName name:map showTypeTree args) ++ ")"
----
----inDeepTypeable2 :: (DeepTypeable a,DeepTypeable b) => Proxy a -> Proxy b -> Bool
----inDeepTypeable2 a b = typeTree a `inTypeTree2` typeTree b
----
----inTypeTree2 :: TypeTree -> TypeTree -> Bool
----inTypeTree2 t1 t2 = State.evalState (recur t2) Set.empty where
----	recur t2 = do
----		s <- State.gets (Set.member $ showTypeTree t2)
----		if s
----			then return False -- We've already seen and checked this type
----			else if t1 == t2
----				then return True -- We found a matching type
----				else do -- Remember that we were here
----					State.modify (Set.insert $ showTypeTree t2) -- Remember that we were here
----					let ctors = typeTreeCons t2 -- We need to recur on the constructors
----					check (concatMap conTreeTypes ctors) -- Now we recur
----	check [] = return False
----	check (t:ts) = do
----		t' <- recur t
----		if t' then return True else check ts
--
----
------ > (rep,md) <- timeIt $ papers2_dot
------ > (drep,dmd) <- timeIt $ papers2Delta repmd [AddFile "..."]
------ >  show drep
------ > rep' <- timeIt $ applySValueDeltas drep rep
----
----papers2Sizes :: IO Int64
----papers2Sizes = do
----	(rep,md) <- papers2
----	return $ filesizes md
----	
----paperCount :: IO Int
----paperCount = do
----	(rep,md) <- papers2
----	return $ numpapers rep
----
----yearsList :: Papers2 -> [String]
----yearsList = undefined -- $(everything [| (++) |] (mkQ [| [] |] 'yearVals) [t| Papers2 |])
----
----staticYears = ["1970","1981","1982","1983","1985","1986","1987","1988","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2015","1979"]
----
----authorsList :: Papers2 -> [String]
----authorsList = undefined -- $(everything [| (++) |] (mkQ [| [] |] 'authorVals) [t| Papers2 |])
----
----staticAuthors = ["Huet","Jansson","Gottlob","Davidson","Bancilhon","Codd","Dayal","Keller","Chandra","Wadler","Bru\776ggemann-Klein","Hainaut","Sheard","Fokkinga","Larson","Meijer","Desai","Morgan","Meertens","Jay","Jones","Schu\776rr","Jay\8230","Doornbos","Hu","Cunha","Fisher","Foster","Frias","Hosoya","La\776mmel","Massoni","M\248ller","Onlamp.Com","Taentzer","Visser","Nailburg","Demuth","Hinze","KENNEDY","Bentayeb","Oliveira","Abramov","Buneman","Egyed","Gibbons","van Deursen","Klop","Marcos","Pardo","Jackson","Mens","Alimarine","Broberg","Gheyi","Laemmel","Mu","Bernard","Berstel","Ferreira","Hidaka","Lempsink","Nakano","Preoteasa","Ruby","Voigtla\776nder","Xiong","Yakushev","ACTION","ATANASSOW","Anastasakis","Antkiewicz","Backus","Comon","Ehrig","Ennals","Fagin","Johann","Matsuda","Mitchell","Moro","Nitsche","Raymond","Schrijvers","Terwilliger","Acar","Ambler","Barbosa","Berdaguer","Be\769zivin","Black","Bohannon","COLAZZO","Capretta","Czarnecki","Fowler","Ghelli","Hegner","Jouault","Kawanaka","Konigs","Liu","Marshall","Brabrand","Cheney","Diskin","Frisch","Lutterkort","Melnik","Rodriguez","Stevens","Vermolen","Hammer","Rajkumar","Cicchetti","Claessen","Hermann","Hoffmann","Hofmann","Inaba","Macedo","Wang","Pacheco","Abbott","Backhouse","Feathers","Fegaras","Giese","Lano","McBride","Silva","Sittampalam","Strommer","Takeichi","Yokoyama","Pierce","Culik","Hopcroft","Bird","Sasano","Rumbaugh","Hoogendijk","LU","jno","lsb"]
----
----filesizes :: Papers2_md -> Int64
----filesizes = undefined -- $(everything [| (+) |] (mkQ [| 0 |] 'offset) [t| Papers2_md |])
----
----numpapers :: Papers2 -> Int
----numpapers = undefined -- $(everything [| (+) |] (mkQ [| 0 |] 'onepaper) [t| Papers2 |])
----
----mkRandomPaper :: StateT (Int,[FilePath],[FilePath]) IO ()
----mkRandomPaper = do
----	let root = "/Users/hpacheco/Documents/Papers2"
----	folder <- lift $ generate $ elements ["Articles","Books","Media","Reports"]
----	year <- lift $ generate $ elements staticYears
----	author <- lift $ generate $ elements staticAuthors
----	i <- newPaperId
----	let dir = root ++ "/" ++ folder ++ "/" ++ year ++ "/" ++ author
----	b <- lift $ doesDirectoryExist dir
----	if b
----		then return ()
----		else addDir dir >> lift (createDirectoryIfMissing True dir)
----	let paper = dir ++ "/paper" ++ show i ++ ".pdf"
----	addPaper paper
----	lift $ copyFile template paper
----	lift $ putStrLn $ "created " ++ show paper
----	return ()
----
----addPaper :: MonadState (Int,[FilePath],[FilePath]) m => FilePath -> m ()
----addPaper paper = State.get >>= \(x,ys,s) -> State.put (x,paper:ys,s)
----
----addDir :: MonadState (a,b,[FilePath]) m => FilePath -> m ()
----addDir dir = State.get >>= \(x,y,s) -> State.put (x,y,dir : s)
----
----newPaperId :: MonadState (Int,a,b) m => m Int
----newPaperId = State.get >>= \(i,y,z) -> State.put (succ i,y,z) >> return i
----
----removeRandomPaper :: StateT (Int,[FilePath],[FilePath]) IO ()
----removeRandomPaper = do
----	(x,papers,z) <- State.get
----	if null papers
----		then return ()
----		else do
----			paper <- lift $ generate $ elements papers
----			lift $ removeFile paper
----			State.put (x,papers \\ [paper],z)
----			lift $ putStrLn $ "removed " ++ paper
----			return ()
----
----noise :: StateT (Int,[FilePath],[FilePath]) IO ()
----noise = do
------	coin <- lift $ generate $ choose (True,False)
------	if coin
----	mkRandomPaper
------		else removeRandomPaper
----
----noiseLoop :: Int -> StateT (Int,[FilePath],[FilePath]) IO ()
----noiseLoop 0 = return ()
----noiseLoop i = do
----	noise
----	noiseLoop (pred i)
----	
----template = "/Users/hpacheco/Documents/Papers2/Articles/1990/Desai/1990 Desai.pdf"
----
----bookkeeping :: StateT (Int,[FilePath],[FilePath]) IO ()
----bookkeeping = do
----	(_,papers,folders) <- State.get
----	mapM_ (\paper -> lift $ removeFile paper) papers
----	mapM_ (\dir -> lift $ removeDirectory dir) folders
----
----changePapers :: StateT (Int,[FilePath],[FilePath]) IO ()
----changePapers = do
------	lift $ threadDelay (fromEnum $ 10^6 / 5)
----	noiseLoop 50
----	
----sizesTest :: IO Int64
----sizesTest = do
----	hSetBuffering stdout NoBuffering       
------	forkIO $ State.evalStateT changePapers (0,[],[]) 
----	i <- papers2Sizes
----	return i
----
----countTest :: IO Int
----countTest = do
----	hSetBuffering stdout NoBuffering       
------	forkIO $ State.evalStateT changePapers (0,[],[]) 
----	i <- paperCount
----	return i
----	
----	--old:240
----	--new: 290
----	--mangled:281
----	
----
------transaction: add papers in groups of 5; we should never get 241,242,etc



