{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables, ViewPatterns #-}

module Examples.Papers2 where

import Language.Pads.Padsc hiding (take,numErrors,head,lift)
import Language.Forest.Forestc hiding (sources,addDir)
import Language.Forest.Graph
import Data.List
import Data.Char
import Language.Forest.Delta
import Data.Maybe
import Data.List.Split
import System.Posix.Types
import Data.Int
import Data.Generics.TH
import Language.Forest.TH.StageDefs
import Examples.Papers2Types

import Debug.Trace
import System.TimeIt

import Control.Concurrent
import Control.Monad
import System.IO
import System.Directory
import Test.QuickCheck.Gen
import Control.Monad.State (State(..),StateT(..),MonadState(..))
import qualified Control.Monad.State as State
import Control.Monad.Trans

-----------------------

writeLog :: String -> IO ()
writeLog str = writeFile "/Users/hpacheco/Documents/Papers2/log.txt" str

papers2 :: IO (Papers2,Papers2_md)
papers2 = load "/Users/hpacheco/Documents/Papers2"

papers2Delta :: (Papers2,Papers2_md) -> FileSystemDeltas -> IO (ValueDeltas Papers2,ValueDeltas Papers2_md)
papers2Delta repmd df = loadDelta "/Users/hpacheco/Documents/Papers2" repmd df

papers2_dot :: IO (Papers2,Papers2_md)
papers2_dot = do
	(rep,md) <- papers2
	let fmd = get_fmd_header md
	print (numErrors fmd)
	print (errorMsg fmd)
--	mdToPDF md "/Users/hpacheco/Documents/Papers2/papers2.pdf"
	return (rep,md)

-- > (rep,md) <- timeIt $ papers2_dot
-- > (drep,dmd) <- timeIt $ papers2Delta repmd [AddFile "..."]
-- >  show drep
-- > rep' <- timeIt $ applyValueDeltas drep rep

papers2Sizes :: IO Int64
papers2Sizes = do
	(rep,md) <- papers2
	return $ filesizes md
	
paperCount :: IO Int
paperCount = do
	(rep,md) <- papers2
	return $ numpapers rep

yearsList :: Papers2 -> [String]
yearsList = $(everything [| (++) |] (mkQ [| [] |] 'yearVals) [t| Papers2 |])

staticYears = ["1970","1981","1982","1983","1985","1986","1987","1988","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2015","1979"]

authorsList :: Papers2 -> [String]
authorsList = $(everything [| (++) |] (mkQ [| [] |] 'authorVals) [t| Papers2 |])

staticAuthors = ["Huet","Jansson","Gottlob","Davidson","Bancilhon","Codd","Dayal","Keller","Chandra","Wadler","Bru\776ggemann-Klein","Hainaut","Sheard","Fokkinga","Larson","Meijer","Desai","Morgan","Meertens","Jay","Jones","Schu\776rr","Jay\8230","Doornbos","Hu","Cunha","Fisher","Foster","Frias","Hosoya","La\776mmel","Massoni","M\248ller","Onlamp.Com","Taentzer","Visser","Nailburg","Demuth","Hinze","KENNEDY","Bentayeb","Oliveira","Abramov","Buneman","Egyed","Gibbons","van Deursen","Klop","Marcos","Pardo","Jackson","Mens","Alimarine","Broberg","Gheyi","Laemmel","Mu","Bernard","Berstel","Ferreira","Hidaka","Lempsink","Nakano","Preoteasa","Ruby","Voigtla\776nder","Xiong","Yakushev","ACTION","ATANASSOW","Anastasakis","Antkiewicz","Backus","Comon","Ehrig","Ennals","Fagin","Johann","Matsuda","Mitchell","Moro","Nitsche","Raymond","Schrijvers","Terwilliger","Acar","Ambler","Barbosa","Berdaguer","Be\769zivin","Black","Bohannon","COLAZZO","Capretta","Czarnecki","Fowler","Ghelli","Hegner","Jouault","Kawanaka","Konigs","Liu","Marshall","Brabrand","Cheney","Diskin","Frisch","Lutterkort","Melnik","Rodriguez","Stevens","Vermolen","Hammer","Rajkumar","Cicchetti","Claessen","Hermann","Hoffmann","Hofmann","Inaba","Macedo","Wang","Pacheco","Abbott","Backhouse","Feathers","Fegaras","Giese","Lano","McBride","Silva","Sittampalam","Strommer","Takeichi","Yokoyama","Pierce","Culik","Hopcroft","Bird","Sasano","Rumbaugh","Hoogendijk","LU","jno","lsb"]

filesizes :: Papers2_md -> Int64
filesizes = $(everything [| (+) |] (mkQ [| 0 |] 'offset) [t| Papers2_md |])

numpapers :: Papers2 -> Int
numpapers = $(everything [| (+) |] (mkQ [| 0 |] 'onepaper) [t| Papers2 |])

mkRandomPaper :: StateT (Int,[FilePath],[FilePath]) IO ()
mkRandomPaper = do
	let root = "/Users/hpacheco/Documents/Papers2"
	folder <- lift $ generate $ elements ["Articles","Books","Media","Reports"]
	year <- lift $ generate $ elements staticYears
	author <- lift $ generate $ elements staticAuthors
	i <- newPaperId
	let dir = root ++ "/" ++ folder ++ "/" ++ year ++ "/" ++ author
	b <- lift $ doesDirectoryExist dir
	if b
		then return ()
		else addDir dir >> lift (createDirectoryIfMissing True dir)
	let paper = dir ++ "/paper" ++ show i ++ ".pdf"
	addPaper paper
	lift $ copyFile template paper
	lift $ putStrLn $ "created " ++ show paper
	return ()

addPaper :: MonadState (Int,[FilePath],[FilePath]) m => FilePath -> m ()
addPaper paper = State.get >>= \(x,ys,s) -> State.put (x,paper:ys,s)

addDir :: MonadState (a,b,[FilePath]) m => FilePath -> m ()
addDir dir = State.get >>= \(x,y,s) -> State.put (x,y,dir : s)

newPaperId :: MonadState (Int,a,b) m => m Int
newPaperId = State.get >>= \(i,y,z) -> State.put (succ i,y,z) >> return i

removeRandomPaper :: StateT (Int,[FilePath],[FilePath]) IO ()
removeRandomPaper = do
	(x,papers,z) <- State.get
	if null papers
		then return ()
		else do
			paper <- lift $ generate $ elements papers
			lift $ removeFile paper
			State.put (x,papers \\ [paper],z)
			lift $ putStrLn $ "removed " ++ paper
			return ()

noise :: StateT (Int,[FilePath],[FilePath]) IO ()
noise = do
--	coin <- lift $ generate $ choose (True,False)
--	if coin
	mkRandomPaper
--		else removeRandomPaper

noiseLoop :: Int -> StateT (Int,[FilePath],[FilePath]) IO ()
noiseLoop 0 = return ()
noiseLoop i = do
	noise
	noiseLoop (pred i)
	
template = "/Users/hpacheco/Documents/Papers2/Articles/1990/Desai/1990 Desai.pdf"

bookkeeping :: StateT (Int,[FilePath],[FilePath]) IO ()
bookkeeping = do
	(_,papers,folders) <- State.get
	mapM_ (\paper -> lift $ removeFile paper) papers
	mapM_ (\dir -> lift $ removeDirectory dir) folders

changePapers :: StateT (Int,[FilePath],[FilePath]) IO ()
changePapers = do
--	lift $ threadDelay (fromEnum $ 10^6 / 5)
	noiseLoop 50
	
sizesTest :: IO Int64
sizesTest = do
	hSetBuffering stdout NoBuffering       
--	forkIO $ State.evalStateT changePapers (0,[],[]) 
	i <- papers2Sizes
	return i

countTest :: IO Int
countTest = do
	hSetBuffering stdout NoBuffering       
--	forkIO $ State.evalStateT changePapers (0,[],[]) 
	i <- paperCount
	return i
	
	--old:240
	--new: 290
	--mangled:281
	

--transaction: add papers in groups of 5; we should never get 241,242,etc








