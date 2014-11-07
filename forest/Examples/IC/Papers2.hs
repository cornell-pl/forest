{-# LANGUAGE RankNTypes, TupleSections, OverlappingInstances, TypeFamilies, StandaloneDeriving, TypeOperators, ConstraintKinds, DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables, ViewPatterns #-}

module Examples.IC.Papers2 where

import Data.Hashable
import Prelude hiding (mod,read,const)
import qualified Prelude
import Control.Monad.Incremental.Adapton
import Control.Monad.Lazy
import Control.Monad.Incremental.Display
import Control.Monad.Reader (ReaderT(..))
import qualified Control.Monad.Reader as Reader
import Data.List
import Data.Char
import Data.Maybe
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.List.Split
import System.Posix.Types
import System.Mem.WeakRef
import Data.Int
import Data.IORef
import Control.Monad.Incremental as Inc
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
import System.Mem.WeakTable as WeakTable
import Language.Pads.Padsc hiding (numErrors)

import Data.Typeable.Internal
import Debug.Trace
import Data.DeepTypeable
import Language.Haskell.TH.Syntax

import Language.Forest.IC

[iforest|
	type Paper (y :: Maybe Integer) (n :: Maybe String) = BinaryFile where (isPaperOf (fullpath this_att) y n)

    type Supplemental (y :: Maybe Integer) (n :: Maybe String) = Directory {
		supplementalFiles is Map [ p :: Paper y n | p <- matches (GL "*"), (isNotHiddenFile p p_att) ] 
	} 

	type Author (y :: Maybe Integer) (n :: Maybe String) = Directory {
		authorPapers is Map [ p :: Paper y n | p <- matches (GL "*"), (isNotHiddenFile p p_att) ] 
	,   supplemental is "Supplemental" :: Maybe (Supplemental y n) where True
	}

	type Year (y :: Maybe Integer) = Directory {
		authors is Map [ n :: Author y (getAuthor n) | n <- matches (GL "*"), (not (hidden n)) ]
	}

	type Articles = Map [ y :: Year (getYear y) | y <- matches yearRE ] where True
	type Books    = Map [ y :: Year (getYear y) | y <- matches yearRE ]
	type Media    = Map [ y :: Year (getYear y) | y <- matches yearRE ]
	type Reports  = Map [ y :: Year (getYear y) | y <- matches yearRE ]

	type Library (articles :: [String]) = Directory {
		database is "Database.papersdb" :: BinaryFile
	}

	type Papers2 = Directory {
		articles is "Articles" :: Maybe Articles
	,   books is "Books" :: Maybe Books
	,   media is "Media" :: Maybe Media
	,   reports is "Reports" :: Maybe Reports where True
	,	library matches libraryRE :: Maybe Library <| allPaperNames articles books media reports |>
	} where True
|]

libraryRE = RE "Library.papers2|Library.papers"

allPaperNames articles books media reports = return []

hidden :: String -> Bool
hidden = isPrefixOf "."

isNotHiddenFile :: String -> FileInfo -> Bool
isNotHiddenFile p p_att = isFile p_att && not (hidden p)

isPaperOf :: FilePath -> Maybe Integer -> Maybe String -> Bool
isPaperOf (toLowerString -> file) myear (fmap toLowerString -> mname) = isSuffixOf suffix filename || (isSuffixOf suffix name && isNumberString num)
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


papersDefaultRoot = "/home/hpacheco/Documents/Papers2"
papersNILFSRoot = "/media/hpacheco/nilfs/Papers2"
smallpapersNILFSRoot = "/media/hpacheco/nilfs/SmallPapers2"
myDir = "/home/hpacheco/Forest"

papersErrors :: ICRep fs => ((Papers2 fs,Papers2_md fs),LoadInfo fs) -> ForestO fs ()
papersErrors ((rep,md),_) = do
	err <- get_errors md
	forestM $ forestIO $ print (numErrors err)
	forestM $ forestIO $ print (errorMsg err)

timeForestNILFS (NILFSForestO m) = NILFSForestO $ Reader.mapReaderT (Outer . timeIt . runOuter) m

papers2_dot :: Bool -> FilePath -> ForestO NILFS ()
papers2_dot toPDF papersRoot = do
--	let (draw,ext) = if toPDF then (forestDrawToPDF,"pdf") else (forestDrawToDot,"dot")
	(dta@(repmd@(rep,md),_),thunk) <- timeForestNILFS $ do
		dta@(repmd@(rep::Papers2 NILFS,md::Papers2_md NILFS),_) <- load () papersRoot
--		papersErrors dta
--		thunk <- inside $ topkProlificAuthors 3 md
		thunk <- inside $ topkLargestFiles 3 md
--		thunk <- inside $ paperCount rep -- counts the number of papers
--		thunk <- inside $ articlePaperNamesWithYear 2005 (rep :.: md) -- returns articles published in 2005
		forestM $ forestIO $ putStrLn "drawing"
--		draw proxyNILFS (Merge repmd thunk) $ addExtension "/home/hpacheco/0" ext
		displayAs "Load: " thunk
		forestM $ forestIO $ putStrLn "drawing"
--		draw proxyNILFS (Merge repmd thunk) $ addExtension "/home/hpacheco/1" ext
		return (dta,thunk)
--	mdToPDF (snd repmd) "/home/hpacheco/papers2.pdf"
--	forestPrint rep
--	forestPrint md
	
	forever $ do
		forestM $ forestIO $ putStrLn "Press ENTER for reloading"
		forestM $ forestIO $ getLine
		timeForestNILFS $ do
			reload papersRoot dta
			forestM $ forestIO $ putStrLn "drawing"
--			draw proxyNILFS (Merge repmd thunk) $ addExtension "/home/hpacheco/2" ext
			forestM $ forestIO $ putStrLn "counting"
--			papersErrors dta
			displayAs "Reload: " thunk
--			liftIO $ putStrLn "Printing result NOW..."
--			forestPrint md
			forestM $ forestIO $ putStrLn "drawing"
--			draw proxyNILFS (Merge repmd thunk) $ addExtension "/home/hpacheco/3" ext
			return ()
	return ()
	
--papers2_between :: FSTree NILFS -> FSTree NILFS -> Bool -> FilePath -> ForestO NILFS ()
--papers2_between oldTree newTree toPDF papersRoot = do
----	let (draw,ext) = if toPDF then (forestDrawToPDF,"pdf") else (forestDrawToDot,"dot")
--	(dta@(repmd@(rep,md),_),thunk) <- timeForestO $ do
--		dta@(repmd@(rep::Papers2 NILFS,md::Papers2_md NILFS),_) <- loadTree oldTree () papersRoot
----		papersErrors dta
----		thunk <- inside $ articlePaperNamesWithYear 2005 (rep :.: md)
--		thunk <- inside $ authorPaperCount md
--		liftIO $ putStrLn "drawing0"
----		draw proxyNILFS (Merge repmd thunk) $ addExtension "/home/hpacheco/0" ext
--		liftIO $ putStrLn "evaluating"
--		display thunk
--		liftIO $ putStrLn "drawing1"
----		draw proxyNILFS (Merge repmd thunk) $ addExtension "/home/hpacheco/1" ext
--		return (dta,thunk)
----	mdToPDF (snd repmd) "/home/hpacheco/papers2.pdf"
----	forestPrint rep
----	forestPrint md
--	
--	do
--		liftIO $ putStrLn "Press ENTER for reloading"
--		liftIO $ getLine
--		timeForestO $ do
--			reloadTree newTree Proxy papersRoot dta
--			liftIO $ putStrLn "drawing2"
----			draw proxyNILFS (Merge repmd thunk) $ addExtension "/home/hpacheco/2" ext
--			liftIO $ putStrLn "counting"
----			papersErrors dta
--			display thunk
----			liftIO $ putStrLn "Printing result NOW..."
----			forestPrint md
--			liftIO $ putStrLn "drawing3"
----			draw proxyNILFS (Merge repmd thunk) $ addExtension "/home/hpacheco/3" ext
--			return ()
--	return ()

--papers2_dot_Default = timeIt $ flip finally (mergePDFs $ papersDefaultRoot </> "forest.pdf") $ runForest DefaultForestCfg $ papers2_dot Proxy papersDefaultRoot
papers2_dot_NILFS = do
	let path = papersNILFSRoot
	runIncrementalForest (NILFSForestConfig False path myDir) $ papers2_dot True path
smallpapers2_pdf_NILFS = runIncrementalForest (NILFSForestConfig False smallpapersNILFSRoot myDir) $ papers2_dot True smallpapersNILFSRoot

main = papers2_dot_NILFS

--smallpapers2_static = do
--	time116 <- readNILFSTime "2014-10-16 13:53:09"
--	time120 <- readNILFSTime "2014-10-16 14:01:41"
--	time154 <- readNILFSTime "2014-10-17 09:43:30"
--	time391 <- readNILFSTime "2014-10-21 11:25:33"
--	time393 <- readNILFSTime "2014-10-21 11:38:55"
--	time397 <- readNILFSTime "2014-10-21 11:48:19"
--	let path = smallpapersNILFSRoot
--	runForest (NILFSForestConfig path myDir) $ papers2_between (NILFSTree 393 time391) (NILFSTree 397 time393) True path

testD = inDeepTypeable (Proxy :: Proxy (Articles NILFS :.: Articles_md NILFS)) (Proxy :: Proxy (Papers2 NILFS :.: Papers2_md NILFS))
testD2 = inDeepTypeable (Proxy :: Proxy (Author_md NILFS)) (Proxy :: Proxy (Articles_md NILFS))

--showTypeTree :: TypeTree -> String
--showTypeTree (MkTypeTree name args _) = "(" ++ foldl1 (\s1 s2 -> s1 ++' ':s2) (showName name:map showTypeTree args) ++ ")"
--
--inDeepTypeable2 :: (DeepTypeable a,DeepTypeable b) => Proxy a -> Proxy b -> Bool
--inDeepTypeable2 a b = typeTree a `inTypeTree2` typeTree b
--
--inTypeTree2 :: TypeTree -> TypeTree -> Bool
--inTypeTree2 t1 t2 = State.evalState (recur t2) Set.empty where
--	recur t2 = do
--		s <- State.gets (Set.member $ showTypeTree t2)
--		if s
--			then return False -- We've already seen and checked this type
--			else if t1 == t2
--				then return True -- We found a matching type
--				else do -- Remember that we were here
--					State.modify (Set.insert $ showTypeTree t2) -- Remember that we were here
--					let ctors = typeTreeCons t2 -- We need to recur on the constructors
--					check (concatMap conTreeTypes ctors) -- Now we recur
--	check [] = return False
--	check (t:ts) = do
--		t' <- recur t
--		if t' then return True else check ts

--
---- > (rep,md) <- timeIt $ papers2_dot
---- > (drep,dmd) <- timeIt $ papers2Delta repmd [AddFile "..."]
---- >  show drep
---- > rep' <- timeIt $ applySValueDeltas drep rep
--
--papers2Sizes :: IO Int64
--papers2Sizes = do
--	(rep,md) <- papers2
--	return $ filesizes md
--	
--paperCount :: IO Int
--paperCount = do
--	(rep,md) <- papers2
--	return $ numpapers rep
--
--yearsList :: Papers2 -> [String]
--yearsList = undefined -- $(everything [| (++) |] (mkQ [| [] |] 'yearVals) [t| Papers2 |])
--
--staticYears = ["1970","1981","1982","1983","1985","1986","1987","1988","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2015","1979"]
--
--authorsList :: Papers2 -> [String]
--authorsList = undefined -- $(everything [| (++) |] (mkQ [| [] |] 'authorVals) [t| Papers2 |])
--
--staticAuthors = ["Huet","Jansson","Gottlob","Davidson","Bancilhon","Codd","Dayal","Keller","Chandra","Wadler","Bru\776ggemann-Klein","Hainaut","Sheard","Fokkinga","Larson","Meijer","Desai","Morgan","Meertens","Jay","Jones","Schu\776rr","Jay\8230","Doornbos","Hu","Cunha","Fisher","Foster","Frias","Hosoya","La\776mmel","Massoni","M\248ller","Onlamp.Com","Taentzer","Visser","Nailburg","Demuth","Hinze","KENNEDY","Bentayeb","Oliveira","Abramov","Buneman","Egyed","Gibbons","van Deursen","Klop","Marcos","Pardo","Jackson","Mens","Alimarine","Broberg","Gheyi","Laemmel","Mu","Bernard","Berstel","Ferreira","Hidaka","Lempsink","Nakano","Preoteasa","Ruby","Voigtla\776nder","Xiong","Yakushev","ACTION","ATANASSOW","Anastasakis","Antkiewicz","Backus","Comon","Ehrig","Ennals","Fagin","Johann","Matsuda","Mitchell","Moro","Nitsche","Raymond","Schrijvers","Terwilliger","Acar","Ambler","Barbosa","Berdaguer","Be\769zivin","Black","Bohannon","COLAZZO","Capretta","Czarnecki","Fowler","Ghelli","Hegner","Jouault","Kawanaka","Konigs","Liu","Marshall","Brabrand","Cheney","Diskin","Frisch","Lutterkort","Melnik","Rodriguez","Stevens","Vermolen","Hammer","Rajkumar","Cicchetti","Claessen","Hermann","Hoffmann","Hofmann","Inaba","Macedo","Wang","Pacheco","Abbott","Backhouse","Feathers","Fegaras","Giese","Lano","McBride","Silva","Sittampalam","Strommer","Takeichi","Yokoyama","Pierce","Culik","Hopcroft","Bird","Sasano","Rumbaugh","Hoogendijk","LU","jno","lsb"]
--
--filesizes :: Papers2_md -> Int64
--filesizes = undefined -- $(everything [| (+) |] (mkQ [| 0 |] 'offset) [t| Papers2_md |])
--
--numpapers :: Papers2 -> Int
--numpapers = undefined -- $(everything [| (+) |] (mkQ [| 0 |] 'onepaper) [t| Papers2 |])
--
--mkRandomPaper :: StateT (Int,[FilePath],[FilePath]) IO ()
--mkRandomPaper = do
--	let root = "/Users/hpacheco/Documents/Papers2"
--	folder <- lift $ generate $ elements ["Articles","Books","Media","Reports"]
--	year <- lift $ generate $ elements staticYears
--	author <- lift $ generate $ elements staticAuthors
--	i <- newPaperId
--	let dir = root ++ "/" ++ folder ++ "/" ++ year ++ "/" ++ author
--	b <- lift $ doesDirectoryExist dir
--	if b
--		then return ()
--		else addDir dir >> lift (createDirectoryIfMissing True dir)
--	let paper = dir ++ "/paper" ++ show i ++ ".pdf"
--	addPaper paper
--	lift $ copyFile template paper
--	lift $ putStrLn $ "created " ++ show paper
--	return ()
--
--addPaper :: MonadState (Int,[FilePath],[FilePath]) m => FilePath -> m ()
--addPaper paper = State.get >>= \(x,ys,s) -> State.put (x,paper:ys,s)
--
--addDir :: MonadState (a,b,[FilePath]) m => FilePath -> m ()
--addDir dir = State.get >>= \(x,y,s) -> State.put (x,y,dir : s)
--
--newPaperId :: MonadState (Int,a,b) m => m Int
--newPaperId = State.get >>= \(i,y,z) -> State.put (succ i,y,z) >> return i
--
--removeRandomPaper :: StateT (Int,[FilePath],[FilePath]) IO ()
--removeRandomPaper = do
--	(x,papers,z) <- State.get
--	if null papers
--		then return ()
--		else do
--			paper <- lift $ generate $ elements papers
--			lift $ removeFile paper
--			State.put (x,papers \\ [paper],z)
--			lift $ putStrLn $ "removed " ++ paper
--			return ()
--
--noise :: StateT (Int,[FilePath],[FilePath]) IO ()
--noise = do
----	coin <- lift $ generate $ choose (True,False)
----	if coin
--	mkRandomPaper
----		else removeRandomPaper
--
--noiseLoop :: Int -> StateT (Int,[FilePath],[FilePath]) IO ()
--noiseLoop 0 = return ()
--noiseLoop i = do
--	noise
--	noiseLoop (pred i)
--	
--template = "/Users/hpacheco/Documents/Papers2/Articles/1990/Desai/1990 Desai.pdf"
--
--bookkeeping :: StateT (Int,[FilePath],[FilePath]) IO ()
--bookkeeping = do
--	(_,papers,folders) <- State.get
--	mapM_ (\paper -> lift $ removeFile paper) papers
--	mapM_ (\dir -> lift $ removeDirectory dir) folders
--
--changePapers :: StateT (Int,[FilePath],[FilePath]) IO ()
--changePapers = do
----	lift $ threadDelay (fromEnum $ 10^6 / 5)
--	noiseLoop 50
--	
--sizesTest :: IO Int64
--sizesTest = do
--	hSetBuffering stdout NoBuffering       
----	forkIO $ State.evalStateT changePapers (0,[],[]) 
--	i <- papers2Sizes
--	return i
--
--countTest :: IO Int
--countTest = do
--	hSetBuffering stdout NoBuffering       
----	forkIO $ State.evalStateT changePapers (0,[],[]) 
--	i <- paperCount
--	return i
--	
--	--old:240
--	--new: 290
--	--mangled:281
--	
--
----transaction: add papers in groups of 5; we should never get 241,242,etc
--

-- * Generic code (to be automated in the near future!)

instance (Monad (ForestI fs),Sat (ctx (Binary :.: Binary_md))) => MData ctx (ForestI fs) (Binary :.: Binary_md)

-- for pads core types we no longer support twin traversal
instance (DeepTypeable (Binary),DeepTypeable (Binary_md)) => DeepTypeable (Binary :.: Binary_md) where
	typeTree _ = MkTypeTree (mkName ":.:") [] [MkConTree (mkName ":.:") [typeTree (Proxy::Proxy Binary),typeTree (Proxy::Proxy Binary_md)]]

instance (DeepTypeable (BinaryFile fs :.: BinaryFile_md fs),ICRep fs,Sat (ctx (BinaryFile fs :.: BinaryFile_md fs)),MData ctx (ForestI fs) (Binary :.: Binary_md)) => MData ctx (ForestI fs) (BinaryFile fs :.: BinaryFile_md fs) where
	gfoldl ctx k z x = z (\m1 -> putM binaryFileLensM (return x) m1) >>= flip k (getM binaryFileLensM $ return x)

instance (DeepTypeable fs,DeepTypeable (Binary :.: Binary_md)) => DeepTypeable (BinaryFile fs :.: BinaryFile_md fs) where
	typeTree (_::Proxy (BinaryFile fs :.: BinaryFile_md fs)) = MkTypeTree (mkName ":.:") [typeTree (Proxy::Proxy fs)] [MkConTree (mkName ":.:") [typeTree (Proxy::Proxy (Binary :.: Binary_md))]]

binaryFileLensM :: ICRep fs => LensM (ForestI fs) (BinaryFile fs :.: BinaryFile_md fs) (Binary :.: Binary_md)
binaryFileLensM = (lensM (Lens unBinaryFile (\s v -> s { unBinaryFile = v })) `compLensM` fsThunkLensI) `prodFLensM` (fsThunkLensI `compLensM` sndLensM `compLensM` fsThunkLensI)

instance (DeepTypeable (Paper fs :.: Paper_md fs),ICRep fs,Sat (ctx (Paper fs :.: Paper_md fs)),MData ctx (ForestI fs) (BinaryFile fs :.: BinaryFile_md fs)) => MData ctx (ForestI fs) (Paper fs :.: Paper_md fs) where
	gfoldl ctx k z x = z (\m1 -> putM paperBinaryFileLensM (return x) m1) >>= flip k (getM paperBinaryFileLensM $ return x)

instance (DeepTypeable fs,DeepTypeable (BinaryFile fs :.: BinaryFile_md fs),DeepTypeable [Paper fs :.: Paper_md fs]) => DeepTypeable (Paper fs :.: Paper_md fs) where
	typeTree (_::Proxy (Paper fs :.: Paper_md fs)) = MkTypeTree (mkName ":.:") [typeTree (Proxy::Proxy fs)] [MkConTree (mkName ":.:") [typeTree (Proxy::Proxy (BinaryFile fs :.: BinaryFile_md fs))]]

paperBinaryFileLensM :: ICRep fs => LensM (ForestI fs) (Paper fs :.: Paper_md fs) (BinaryFile fs :.: BinaryFile_md fs)
paperBinaryFileLensM = lensM (Lens unPaper (\s v -> s { unPaper = v })) `prodFLensM` (fstLensM `compLensM` fstLensM)

instance (DeepTypeable (Supplemental fs :.: Supplemental_md fs),ICRep fs,Sat (ctx (Supplemental fs :.: Supplemental_md fs)),MData ctx (ForestI fs) [Paper fs :.: Paper_md fs]) => MData ctx (ForestI fs) (Supplemental fs :.: Supplemental_md fs) where
	gfoldl ctx k z x = z (\m1 -> putM supplementaFilesLensM (return x) m1) >>= flip k (getM supplementaFilesLensM $ return x)

instance (DeepTypeable fs,DeepTypeable [Paper fs :.: Paper_md fs],DeepTypeable [Paper fs :.: Paper_md fs]) => DeepTypeable (Supplemental fs :.: Supplemental_md fs) where
	typeTree (_::Proxy (Supplemental fs :.: Supplemental_md fs)) = MkTypeTree (mkName ":.:") [typeTree (Proxy::Proxy fs)] [MkConTree (mkName ":.:") [typeTree (Proxy::Proxy [Paper fs :.: Paper_md fs])]]
	
supplementaFilesLensM :: ICRep fs => LensM (ForestI fs) (Supplemental fs :.: Supplemental_md fs) [Paper fs :.: Paper_md fs]
supplementaFilesLensM = ((fsThunkLensI `compLensM` lensM (Lens supplementalFiles (\s v -> s { supplementalFiles = v })) `compLensM` mapElemsLensM) `prodFLensM` (fstLensM `compLensM` fsThunkLensI `compLensM` sndLensM `compLensM` lensM (Lens supplementalFiles_md (\s v -> s { supplementalFiles_md = v })) `compLensM` mapElemsLensM `compLensM` mapLensM fstLensM)) `compLensM` zipFLensM
	
instance (ICRep fs,Sat (ctx (Author fs :.: Author_md fs)),MData ctx (ForestI fs) (Maybe (Supplemental fs :.: Supplemental_md fs)),MData ctx (ForestI fs) [Paper fs :.: Paper_md fs]) => MData ctx (ForestI fs) (Author fs :.: Author_md fs) where
	gfoldl ctx k z x = z (\m1 -> return $ \m2 -> putM authorSupplementalLensM (putM authorPapersLensM (return x) m1) m2) >>= flip k (getM authorPapersLensM $ return x) >>= flip k (getM authorSupplementalLensM $ return x)

instance (DeepTypeable fs,DeepTypeable (Maybe (Supplemental fs :.: Supplemental_md fs)),DeepTypeable [Paper fs :.: Paper_md fs]) => DeepTypeable (Author fs :.: Author_md fs) where
	typeTree (_::Proxy (Author fs :.: Author_md fs)) = MkTypeTree (mkName ":.:") [typeTree (Proxy::Proxy fs)] [MkConTree (mkName ":.:") [typeTree (Proxy::Proxy (Maybe (Supplemental fs :.: Supplemental_md fs))),typeTree (Proxy::Proxy [Paper fs :.: Paper_md fs])]]

authorPapersLensM :: ICRep fs => LensM (ForestI fs) (Author fs :.: Author_md fs) [Paper fs :.: Paper_md fs]
authorPapersLensM = ((fsThunkLensI `compLensM` lensM (Lens authorPapers (\s v -> s { authorPapers = v })) `compLensM` mapElemsLensM) `prodFLensM` (fstLensM `compLensM` fsThunkLensI `compLensM` sndLensM `compLensM` lensM (Lens authorPapers_md (\s v -> s { authorPapers_md = v })) `compLensM` mapElemsLensM `compLensM` mapLensM fstLensM)) `compLensM` zipFLensM

authorSupplementalLensM :: ICRep fs => LensM (ForestI fs) (Author fs :.: Author_md fs) (Maybe (Supplemental fs :.: Supplemental_md fs))
authorSupplementalLensM = ((fsThunkLensI `compLensM` lensM (Lens supplemental (\s v -> s { supplemental = v })) `compLensM` fsThunkLensI) `prodFLensM` (fstLensM `compLensM` fsThunkLensI `compLensM` sndLensM `compLensM` lensM (Lens supplemental_md (\s v -> s { supplemental_md = v })) `compLensM` fstLensM `compLensM` fsThunkLensI `compLensM` sndLensM)) `compLensM` zipFMaybeLensM

instance (DeepTypeable (Year fs :.: Year_md fs),ICRep fs,Sat (ctx (Year fs :.: Year_md fs)),MData ctx (ForestI fs) [Author fs :.: Author_md fs]) => MData ctx (ForestI fs) (Year fs :.: Year_md fs) where
	gfoldl ctx k z x = z (\m1 -> putM yearAuthorsLensM (return x) m1) >>= flip k (getM yearAuthorsLensM $ return x)

instance (DeepTypeable fs,DeepTypeable [Author fs :.: Author_md fs]) => DeepTypeable (Year fs :.: Year_md fs) where
	typeTree (_::Proxy (Year fs :.: Year_md fs)) = MkTypeTree (mkName ":.:") [typeTree (Proxy::Proxy fs)] [MkConTree (mkName ":.:") [typeTree (Proxy::Proxy [Author fs :.: Author_md fs])]]

yearAuthorsLensM :: ICRep fs => LensM (ForestI fs) (Year fs :.: Year_md fs) [Author fs :.: Author_md fs]
yearAuthorsLensM = ((fsThunkLensI `compLensM` lensM (Lens authors (\s v -> s { authors = v })) `compLensM` mapElemsLensM) `prodFLensM` (fstLensM `compLensM` fsThunkLensI `compLensM` sndLensM `compLensM` lensM (Lens authors_md (\s v -> s { authors_md = v })) `compLensM` mapElemsLensM `compLensM` mapLensM fstLensM)) `compLensM` zipFLensM
	
instance (DeepTypeable (Articles fs :.: Articles_md fs),ICRep fs,Sat (ctx (Articles fs :.: Articles_md fs)),MData ctx (ForestI fs) [Year fs :.: Year_md fs]) => MData ctx (ForestI fs) (Articles fs :.: Articles_md fs) where
	gfoldl ctx k z x = z (\m1 -> putM articleYearsLensM (return x) m1) >>= flip k (getM articleYearsLensM $ return x)

instance (DeepTypeable fs,DeepTypeable [Year fs :.: Year_md fs]) => DeepTypeable (Articles fs :.: Articles_md fs) where
	typeTree (_::Proxy (Articles fs :.: Articles_md fs)) = MkTypeTree (mkName ":.:") [typeTree (Proxy::Proxy fs)] [MkConTree (mkName ":.:") [typeTree (Proxy::Proxy [Year fs :.: Year_md fs])]]

articleYearsLensM :: ICRep fs => LensM (ForestI fs) (Articles fs :.: Articles_md fs) [Year fs :.: Year_md fs]
articleYearsLensM = ((lensM (Lens unArticles (\s v -> s { unArticles = v })) `compLensM` fsThunkLensI `compLensM` mapElemsLensM) `prodFLensM` (fstLensM `compLensM` fsThunkLensI `compLensM` sndLensM `compLensM` mapElemsLensM `compLensM` mapLensM fstLensM)) `compLensM` zipFLensM

instance (DeepTypeable (Books fs :.: Books_md fs),ICRep fs,Sat (ctx (Books fs :.: Books_md fs)),MData ctx (ForestI fs) [Year fs :.: Year_md fs]) => MData ctx (ForestI fs) (Books fs :.: Books_md fs) where
	gfoldl ctx k z x = z (\m1 -> putM bookYearsLensM (return x) m1) >>= flip k (getM bookYearsLensM $ return x)

instance (DeepTypeable fs,DeepTypeable [Year fs :.: Year_md fs]) => DeepTypeable (Books fs :.: Books_md fs) where
	typeTree (_::Proxy (Books fs :.: Books_md fs)) = MkTypeTree (mkName ":.:") [typeTree (Proxy::Proxy fs)] [MkConTree (mkName ":.:") [typeTree (Proxy::Proxy [Year fs :.: Year_md fs])]]

bookYearsLensM :: ICRep fs => LensM (ForestI fs) (Books fs :.: Books_md fs) [Year fs :.: Year_md fs]
bookYearsLensM = ((lensM (Lens unBooks (\s v -> s { unBooks = v })) `compLensM` fsThunkLensI `compLensM` mapElemsLensM) `prodFLensM` (fsThunkLensI `compLensM` sndLensM `compLensM` mapElemsLensM `compLensM` mapLensM fstLensM)) `compLensM` zipFLensM

instance (DeepTypeable (Media fs :.: Media_md fs),ICRep fs,Sat (ctx (Media fs :.: Media_md fs)),MData ctx (ForestI fs) [Year fs :.: Year_md fs]) => MData ctx (ForestI fs) (Media fs :.: Media_md fs) where
	gfoldl ctx k z x = z (\m1 -> putM mediaYearsLensM (return x) m1) >>= flip k (getM mediaYearsLensM $ return x)

instance (DeepTypeable fs,DeepTypeable [Year fs :.: Year_md fs]) => DeepTypeable (Media fs :.: Media_md fs) where
	typeTree (_::Proxy (Media fs :.: Media_md fs)) = MkTypeTree (mkName ":.:") [typeTree (Proxy::Proxy fs)] [MkConTree (mkName ":.:") [typeTree (Proxy::Proxy [Year fs :.: Year_md fs])]]

mediaYearsLensM :: ICRep fs => LensM (ForestI fs) (Media fs :.: Media_md fs) [Year fs :.: Year_md fs]
mediaYearsLensM = ((lensM (Lens unMedia (\s v -> s { unMedia = v })) `compLensM` fsThunkLensI `compLensM` mapElemsLensM) `prodFLensM` (fsThunkLensI `compLensM` sndLensM `compLensM` mapElemsLensM `compLensM` mapLensM fstLensM)) `compLensM` zipFLensM

instance (DeepTypeable (Reports fs :.: Reports_md fs),ICRep fs,Sat (ctx (Reports fs :.: Reports_md fs)),MData ctx (ForestI fs) [Year fs :.: Year_md fs]) => MData ctx (ForestI fs) (Reports fs :.: Reports_md fs) where
	gfoldl ctx k z x = z (\m1 -> putM reportYearsLensM (return x) m1) >>= flip k (getM reportYearsLensM $ return x)

instance (DeepTypeable fs,DeepTypeable [Year fs :.: Year_md fs]) => DeepTypeable (Reports fs :.: Reports_md fs) where
	typeTree (_::Proxy (Reports fs :.: Reports_md fs)) = MkTypeTree (mkName ":.:") [typeTree (Proxy::Proxy fs)] [MkConTree (mkName ":.:") [typeTree (Proxy::Proxy [Year fs :.: Year_md fs])]]

reportYearsLensM :: ICRep fs => LensM (ForestI fs) (Reports fs :.: Reports_md fs) [Year fs :.: Year_md fs]
reportYearsLensM = ((lensM (Lens unReports (\s v -> s { unReports = v })) `compLensM` fsThunkLensI `compLensM` mapElemsLensM) `prodFLensM` (fsThunkLensI `compLensM` sndLensM `compLensM` mapElemsLensM `compLensM` mapLensM fstLensM)) `compLensM` zipFLensM
	
instance (DeepTypeable (Papers2 fs :.: Papers2_md fs),ICRep fs,Sat (ctx (Papers2 fs :.: Papers2_md fs)),MData ctx (ForestI fs) (Maybe (Articles fs :.: Articles_md fs)),MData ctx (ForestI fs) (Maybe (Books fs :.: Books_md fs)),MData ctx (ForestI fs) (Maybe (Media fs :.: Media_md fs)),MData ctx (ForestI fs) (Maybe (Reports fs :.: Reports_md fs)),MData ctx (ForestI fs) (Maybe (Library fs :.: Library_md fs)))
		=> MData ctx (ForestI fs) (Papers2 fs :.: Papers2_md fs) where
	gfoldl ctx k z x = z (\m1 -> return $ \m2 -> return $ \m3 -> return $ \m4 -> return $ \m5 -> putM papers2LibraryLensM (putM papers2ReportsLensM (putM papers2MediaLensM (putM papers2BooksLensM (putM papers2ArticlesLensM (return x) m1) m2) m3) m4) m5) >>= flip k (getM papers2ArticlesLensM $ return x) >>= flip k (getM papers2BooksLensM $ return x) >>= flip k (getM papers2MediaLensM $ return x) >>= flip k (getM papers2ReportsLensM $ return x) >>= flip k (getM papers2LibraryLensM $ return x)

instance (DeepTypeable fs,DeepTypeable (Maybe (Articles fs :.: Articles_md fs)),DeepTypeable (Maybe (Books fs :.: Books_md fs)),DeepTypeable (Maybe (Media fs :.: Media_md fs)),DeepTypeable (Maybe (Reports fs :.: Reports_md fs)),DeepTypeable (Maybe (Library fs :.: Library_md fs))) => DeepTypeable (Papers2 fs :.: Papers2_md fs) where
	typeTree (_::Proxy (Papers2 fs :.: Papers2_md fs)) = MkTypeTree (mkName ":.:") [typeTree (Proxy::Proxy fs)] [MkConTree (mkName ":.:") [typeTree (Proxy::Proxy (Maybe (Articles fs :.: Articles_md fs))),typeTree (Proxy::Proxy (Maybe (Books fs :.: Books_md fs))),typeTree (Proxy::Proxy (Maybe (Media fs :.: Media_md fs))),typeTree (Proxy::Proxy (Maybe (Reports fs :.: Reports_md fs))),typeTree (Proxy::Proxy (Maybe (Library fs :.: Library_md fs)))]]

papers2ArticlesLensM :: ICRep fs => LensM (ForestI fs) (Papers2 fs :.: Papers2_md fs) (Maybe (Articles fs :.: Articles_md fs))
papers2ArticlesLensM = (((fsThunkLensI `compLensM` lensM (Lens articles (\s v -> s { articles = v }))) `compLensM` fsThunkLensI) `prodFLensM` (fstLensM `compLensM` fsThunkLensI `compLensM` sndLensM `compLensM` lensM (Lens articles_md (\s v -> s { articles_md = v })) `compLensM` fsThunkLensI `compLensM` sndLensM)) `compLensM` zipFMaybeLensM

papers2BooksLensM :: ICRep fs => LensM (ForestI fs) (Papers2 fs :.: Papers2_md fs) (Maybe (Books fs :.: Books_md fs))
papers2BooksLensM = ((fsThunkLensI `compLensM` lensM (Lens books (\s v -> s { books = v })) `compLensM` fsThunkLensI) `prodFLensM` (fstLensM `compLensM` fsThunkLensI `compLensM` sndLensM `compLensM` lensM (Lens books_md (\s v -> s { books_md = v })) `compLensM` fsThunkLensI `compLensM` sndLensM)) `compLensM` zipFMaybeLensM

papers2MediaLensM :: ICRep fs => LensM (ForestI fs) (Papers2 fs :.: Papers2_md fs) (Maybe (Media fs :.: Media_md fs))
papers2MediaLensM = ((fsThunkLensI `compLensM` lensM (Lens media (\s v -> s { media = v })) `compLensM` fsThunkLensI) `prodFLensM` (fstLensM `compLensM` fsThunkLensI `compLensM` sndLensM `compLensM` lensM (Lens media_md (\s v -> s { media_md = v })) `compLensM` fsThunkLensI `compLensM` sndLensM)) `compLensM` zipFMaybeLensM

papers2ReportsLensM :: ICRep fs => LensM (ForestI fs) (Papers2 fs :.: Papers2_md fs) (Maybe (Reports fs :.: Reports_md fs))
papers2ReportsLensM = ((fsThunkLensI `compLensM` lensM (Lens reports (\s v -> s { reports = v })) `compLensM` fsThunkLensI) `prodFLensM` (fstLensM `compLensM` fsThunkLensI `compLensM` sndLensM `compLensM` lensM (Lens reports_md (\s v -> s { reports_md = v })) `compLensM` fstLensM `compLensM` fsThunkLensI `compLensM` sndLensM)) `compLensM` zipFMaybeLensM

papers2LibraryLensM :: ICRep fs => LensM (ForestI fs) (Papers2 fs :.: Papers2_md fs) (Maybe (Library fs :.: Library_md fs))
papers2LibraryLensM = ((fsThunkLensI `compLensM` lensM (Lens library (\s v -> s { library = v })) `compLensM` fsThunkLensI) `prodFLensM` (fstLensM `compLensM` fsThunkLensI `compLensM` sndLensM `compLensM` lensM (Lens library_md (\s v -> s { library_md = v })) `compLensM` fsThunkLensI `compLensM` sndLensM)) `compLensM` zipFMaybeLensM

instance (ICRep fs,Sat (ctx (Library fs :.: Library_md fs)),MData ctx (ForestI fs) (BinaryFile fs :.: BinaryFile_md fs)) => MData ctx (ForestI fs) (Library fs :.: Library_md fs) where
	gfoldl ctx k z x = z (\m1 -> putM libraryDatabaseLensM (return x) m1) >>= flip k (getM libraryDatabaseLensM $ return x)

instance (DeepTypeable fs,DeepTypeable (BinaryFile fs :.: BinaryFile_md fs)) => DeepTypeable (Library fs :.: Library_md fs) where
	typeTree (_::Proxy (Library fs :.: Library_md fs)) = MkTypeTree (mkName ":.:") [typeTree (Proxy::Proxy fs)] [MkConTree (mkName ":.:") [typeTree (Proxy::Proxy (BinaryFile fs :.: BinaryFile_md fs))]]

libraryDatabaseLensM :: ICRep fs => LensM (ForestI fs) (Library fs :.: Library_md fs) (BinaryFile fs :.: BinaryFile_md fs)
libraryDatabaseLensM = (fsThunkLensI `compLensM` lensM (Lens database (\s v -> s { database = v }))) `prodFLensM` (fstLensM `compLensM` fsThunkLensI `compLensM` sndLensM `compLensM` lensM (Lens database_md (\s v -> s { database_md = v })))

instance Memo (BinaryFile fs) => Memo (Paper fs) where
	type Key (Paper fs) = Key (BinaryFile fs)
	{-# INLINE memoKey #-}
	memoKey = memoKey . unPaper

instance Memo (ForestFSThunkI fs (Map String (Year fs))) => Memo (Articles fs) where
	type Key (Articles fs) = Key (ForestFSThunkI fs (Map String (Year fs)))
	{-# INLINE memoKey #-}
	memoKey = memoKey . unArticles

instance Memo (Supplemental_inner fs) where
	type Key (Supplemental_inner fs) = StableName (Supplemental_inner fs)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo (Author_inner fs) where
	type Key (Author_inner fs) = StableName (Author_inner fs)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo (Year_inner fs) where
	type Key (Year_inner fs) = StableName (Year_inner fs)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo (Library_inner fs) where
	type Key (Library_inner fs) = StableName (Library_inner fs)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo (Papers2_inner fs) where
	type Key (Papers2_inner fs) = StableName (Papers2_inner fs)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo (ForestFSThunkI fs (Map String (Year fs))) => Memo (Books fs) where
	type Key (Books fs) = Key (ForestFSThunkI fs (Map String (Year fs)))
	{-# INLINE memoKey #-}
	memoKey = memoKey . unBooks

instance Memo (ForestFSThunkI fs (Map String (Year fs))) => Memo (Media fs) where
	type Key (Media fs) = Key (ForestFSThunkI fs (Map String (Year fs)))
	{-# INLINE memoKey #-}
	memoKey = memoKey . unMedia

instance Memo (ForestFSThunkI fs (Map String (Year fs))) => Memo (Reports fs) where
	type Key (Reports fs) = Key (ForestFSThunkI fs (Map String (Year fs)))
	{-# INLINE memoKey #-}
	memoKey = memoKey . unReports
	
instance Memo (Supplemental_inner_md fs) where
	type Key (Supplemental_inner_md fs) = StableName (Supplemental_inner_md fs)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ Weak.mkWeak x,unsafePerformIO $ makeStableName x)
	
instance Memo (Author_inner_md fs) where
	type Key (Author_inner_md fs) = StableName (Author_inner_md fs)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ Weak.mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo (Year_inner_md fs) where
	type Key (Year_inner_md fs) = StableName (Year_inner_md fs)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ Weak.mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo (Library_inner_md fs) where
	type Key (Library_inner_md fs) = StableName (Library_inner_md fs)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ Weak.mkWeak x,unsafePerformIO $ makeStableName x)
	
instance Memo (Papers2_inner_md fs) where
	type Key (Papers2_inner_md fs) = StableName (Papers2_inner_md fs)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ Weak.mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo (ForestFSThunkI fs Binary) => Memo (BinaryFile fs) where
	type Key (BinaryFile fs) = Key (ForestFSThunkI fs Binary)
	{-# INLINE memoKey #-}
	memoKey = memoKey . unBinaryFile
	

instance Display l inc r m (ForestFSThunkI fs Binary) => Display l inc r m (Paper fs) where
	displaysPrec (Paper x) rest = do
		sx <- displaysPrec x (')':rest)
		return $ "(Paper " ++ sx
instance Display l inc r m (ForestFSThunkI fs (Map String (Year fs))) => Display l inc r m (Articles fs) where
	displaysPrec (Articles x) rest = do
		sx <- displaysPrec x (')':rest)
		return $ "(Articles " ++ sx
instance Display l inc r m (ForestFSThunkI fs (Map String (Year fs))) => Display l inc r m (Books fs) where
	displaysPrec (Books x) rest = do
		sx <- displaysPrec x (')':rest)
		return $ "(Books " ++ sx
instance Display l inc r m (ForestFSThunkI fs (Map String (Year fs))) => Display l inc r m (Media fs) where
	displaysPrec (Media x) rest = do
		sx <- displaysPrec x (')':rest)
		return $ "(Media " ++ sx
instance Display l inc r m (ForestFSThunkI fs (Map String (Year fs))) => Display l inc r m (Reports fs) where
	displaysPrec (Reports x) rest = do
		sx <- displaysPrec x (')':rest)
		return $ "(Reports " ++ sx
instance Display l inc r m (ForestFSThunkI fs Binary) => Display l inc r m (BinaryFile fs) where
	displaysPrec (BinaryFile x) rest = do
		sx <- displaysPrec x (')':rest)
		return $ "(BinaryFile " ++ sx

---


articlePaperNamesWithYear :: (Memo (ForestJoinListICThunkI fs (Paper fs :.: Paper_md fs)),ICRep fs,ForestOutput fs ICThunk Inside,MData (MemoCtx NoCtx) (ForestI fs) (Papers2 fs :.: Papers2_md fs),MData (MemoCtx NoCtx) (ForestI fs) (Articles fs :.: Articles_md fs),MData (MemoCtx NoCtx) (ForestI fs) (Year fs :.: Year_md fs))
                      => Integer -> Papers2 fs :.: Papers2_md fs -> ForestI fs (ForestJoinListICThunkI fs FilePath)
articlePaperNamesWithYear year = articlePapersWithYear year >=> mapJoinListInc (\(paper :.: paper_md) -> liftM (fullpath . fileInfo) $ get_fmd_header paper_md)

articlePapersWithYear :: (ICRep fs,ForestOutput fs ICThunk Inside,MData (MemoCtx NoCtx) (ForestI fs) (Papers2 fs :.: Papers2_md fs),MData (MemoCtx NoCtx) (ForestI fs) (Articles fs :.: Articles_md fs),MData (MemoCtx NoCtx) (ForestI fs) (Year fs :.: Year_md fs))
                      => Integer -> Papers2 fs :.: Papers2_md fs -> ForestI fs (ForestJoinListICThunkI fs (Paper fs :.: Paper_md fs))
articlePapersWithYear = garticlePapersWithYear proxyNoCtx

-- recursive query that collects papers within specific years within articles
garticlePapersWithYear :: (ICRep fs,ForestOutput fs ICThunk Inside,MData (MemoCtx ctx) (ForestI fs) (Articles fs :.: Articles_md fs),MData (MemoCtx ctx) (ForestI fs) (Year fs :.: Year_md fs))
                       => Proxy ctx -> Integer -> ForestGenericQMemo ctx fs Inside (ForestJoinListICThunkI' fs (Paper fs :.: Paper_md fs))
garticlePapersWithYear ctx year = everythingButInc ctx joinListInc'' $ mkQButTypeInc (return EmptyMod) (f ctx) where
	f :: (ICRep fs,ForestOutput fs ICThunk Inside,MData (MemoCtx ctx) (ForestI fs) (Articles fs :.: Articles_md fs),MData (MemoCtx ctx) (ForestI fs) (Year fs :.: Year_md fs))
	  => Proxy ctx -> Articles fs :.: Articles_md fs -> ForestI fs (ForestJoinListICThunkI' fs (Paper fs :.: Paper_md fs))
	f ctx x = gpapersWithYear ctx year x >>= force

-- recursive query that collects papers within specific years
gpapersWithYear :: (ICRep fs,ForestOutput fs ICThunk Inside,MData (MemoCtx ctx) (ForestI fs) (Year fs :.: Year_md fs))
                => Proxy ctx -> Integer -> ForestGenericQMemo ctx fs Inside (ForestJoinListICThunkI' fs (Paper fs :.: Paper_md fs))
gpapersWithYear ctx year = everythingButInc ctx joinListInc'' $ mkQButTypeInc (return EmptyMod) (f ctx) where
	f :: (ICRep fs,ForestOutput fs ICThunk Inside,MData (MemoCtx ctx) (ForestI fs) (Year fs :.: Year_md fs))
	  => Proxy ctx -> Year fs :.: Year_md fs -> ForestI fs (ForestJoinListICThunkI' fs (Paper fs :.: Paper_md fs))
	f ctx x@(y_rep :.: y_md@(_,arg_y)) = force arg_y >>= \mb -> case mb of
		Just ((==year) -> True) -> gpapers ctx x >>= force
		otherwise -> return EmptyMod

-- recursive query that collects papers
gpapers :: (ICRep fs,ForestOutput fs ICThunk Inside) => Proxy ctx -> ForestGenericQMemo ctx fs Inside (ForestJoinListICThunkI' fs (Paper fs :.: Paper_md fs))
gpapers ctx = listifyInc ctx (\(x::(Paper fs :.: Paper_md fs)) -> return True)

paperNames :: (ICRep fs,MData (MemoCtx NoCtx) (ForestI fs) (Author_md fs),ForestOutput fs ICThunk Inside) => Author_md fs -> ForestI fs (ForestJoinListICThunkI fs FilePath)
paperNames (x::Author_md fs) = everythingButInc proxyNoCtx joinListInc''
	(mkQButTypeInc (return EmptyMod) (\(paper_md::Paper_md fs) -> liftM (SingleMod . fullpath . fileInfo) $ get_fmd_header paper_md))
	x
	
papersNames :: (ICRep fs,MData (MemoCtx NoCtx) (ForestI fs) (Papers2_md fs),ForestOutput fs ICThunk Inside) => Papers2_md fs -> ForestI fs (ForestJoinListICThunkI fs FilePath)
papersNames (x::Papers2_md fs) = everythingButInc proxyNoCtx joinListInc''
	(mkQButTypeInc (return EmptyMod) (\(paper_md::Paper_md fs) -> liftM (SingleMod . fullpath . fileInfo) $ get_fmd_header paper_md))
	x

--joinListInc'' tx my = thunk my >>= \ty -> return $ JoinMod tx ty
joinListInc'' tx my = thunk my >>= \ty -> joinListPruneInc tx ty >>= force

paperCount :: (ICRep fs,MData (MemoCtx NoCtx) (ForestI fs) (Papers2 fs),ForestOutput fs ICThunk Inside) => Papers2 fs -> ForestI fs (ForestICThunkI fs Int)
paperCount (papers2::Papers2 fs) = everythingButInc proxyNoCtx
	(\tx my -> force tx >>= \x -> liftM (x+) my)
	(mkQButTypeInc (return 0) (\(paper::Paper fs) -> return 1))
	papers2

topkLargestFiles :: (Memo (ForestICThunkI fs (ForestListICThunkI' fs (String, Int))),ICRep fs,MData (MemoCtx NoCtx) (ForestI fs) (Papers2_md fs),ForestOutput fs ICThunk Inside)
	=> Int -> Papers2_md fs -> ForestI fs (ForestListICThunkI fs (String,Int))
topkLargestFiles k papers2_md = fileSizes papers2_md >>= takeInc k

-- | computes a list of (authorname,numpapers) sorted from most to least prolific author
fileSizes :: (Memo (ForestICThunkI fs (ForestListICThunkI' fs (String,Int))),ICRep fs,MData (MemoCtx NoCtx) (ForestI fs) (Papers2_md fs),ForestOutput fs ICThunk Inside)
	=> Papers2_md fs -> ForestI fs (ForestListICThunkI fs (String,Int))
fileSizes (x::Papers2_md fs) = everythingButInc proxyNoCtx (mergeInc' cmp)
	(q (Proxy::Proxy fs)) x
  where
	cmp (name1,count1) (name2,count2) = compare count2 count1 -- sort in reverse by highest count
	q :: Proxy fs -> GenericQ (MemoCtx NoCtx) (ForestI fs) (ForestListICThunkI fs (String,Int),Bool)
	q fs = mkQButTypeInc (return NilMod) $ \(md::BinaryFile_md fs) -> Inc.get md >>= \(fmd,_) -> do
		let info = fileInfo fmd
		liftM (ConsMod (fullpath info,fromEnum $ size info)) $ const NilMod

-- merge two sorted lists
mergeInc' :: (Memo (ListMod mod l inc r m a),Eq a,Output mod l inc r m,Eq (ListMod mod l inc r m a))
	=> (a -> a -> Ordering) -> ListMod mod l inc r m a -> l inc r m (ListMod' mod l inc r m a) -> l inc r m (ListMod' mod l inc r m a)
mergeInc' cmp tx my = thunk my >>= mergeInc cmp tx >>= force

topkProlificAuthors :: (Eq (ForestListICThunkI fs (ForestListICThunkI fs (String,Int))),Hashable (ForestListICThunkI fs (ForestListICThunkI fs (String,Int))),Memo (ForestListICThunkI fs (ForestListICThunkI fs (String,Int))),Memo (ForestICThunkI fs (ForestListICThunkI' fs (String,Int))),ICRep fs,MData (MemoCtx NoCtx) (ForestI fs) (Papers2_md fs),ForestOutput fs ICThunk Inside)
	=> Int -> Papers2_md fs -> ForestI fs (ForestListICThunkI fs (String,Int))
topkProlificAuthors k papers2_md = authorPaperCount papers2_md >>= takeInc k

authorPaperCount :: (Eq (ForestListICThunkI fs (ForestListICThunkI fs (String,Int))),Hashable (ForestListICThunkI fs (ForestListICThunkI fs (String,Int))),Memo (ForestListICThunkI fs (ForestListICThunkI fs (String,Int))),Memo (ForestICThunkI fs (ForestListICThunkI' fs (String,Int))),ICRep fs,MData (MemoCtx NoCtx) (ForestI fs) (Papers2_md fs),ForestOutput fs ICThunk Inside)
	=> Papers2_md fs -> ForestI fs (ForestListICThunkI fs (String,Int))
authorPaperCount (x::Papers2_md fs) = everythingButInc proxyNoCtx
	(mergeMapInc' cmp mrg)
	(q (Proxy::Proxy fs))
	x
  where
	cmp (name1,count1) (name2,count2) = compare count2 count1 -- sort in reverse by highest count
	mrg count1 count2 = return $ count1 + count2 -- sum counts for the same author
	q :: Proxy fs -> GenericQ (MemoCtx NoCtx) (ForestI fs) (ForestListICThunkI fs (String,Int),Bool)
	q fs = mkQButTypeInc (return NilMod) $ \((md,tyear :*: tname)::Author_md fs) -> force tname >>= \mb -> case mb of
		Nothing -> return NilMod
		Just name -> do
			count <- liftM (Map.size . authorPapers_md . snd) $ Inc.get md
			liftM (ConsMod (name,count)) $ const NilMod

mergeMapInc' :: (Eq (ListMod mod l inc r m (ListMod mod l inc r m (k, v))),Hashable (ListMod mod l inc r m (ListMod mod l inc r m (k, v))),Memo k,Memo v,Memo (ListMod mod l inc r m (ListMod mod l inc r m (k, v))),MonadIO m,Memo (ListMod mod l inc r m (k,v)),Eq k,Eq v,Output mod l inc r m,Eq (ListMod mod l inc r m (k,v)))
	=> ((k,v) -> (k,v) -> Ordering) -> (v -> v -> l inc r m v) -> ListMod mod l inc r m (k,v) -> l inc r m (ListMod' mod l inc r m (k,v)) -> l inc r m (ListMod' mod l inc r m (k,v))
mergeMapInc' cmp mrg tx my = thunk my >>= mergeMapInc cmp mrg tx >>= force


