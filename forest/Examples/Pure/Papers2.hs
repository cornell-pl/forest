{-# LANGUAGE RankNTypes, TupleSections, OverlappingInstances, TypeFamilies, StandaloneDeriving, TypeOperators, ConstraintKinds, DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables, ViewPatterns #-}

module Examples.Pure.Papers2 where

import Data.Hashable
import Prelude hiding (mod,read,const)
import qualified Prelude
import Language.Forest.Pure
import Language.Pads.Padsc hiding (take,numErrors,head,lift)

import Data.List
import Data.Char
import Data.Maybe
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.List.Split
import System.Posix.Types
import Data.Int
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
import System.FilePath.Posix
import Control.Exception.Base
import System.IO.Unsafe

import Data.Typeable.Internal
import Debug.Trace
import Language.Haskell.TH.Syntax

[forest|
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

papersErrors :: FSRep fs => (Papers2,Papers2_md) -> ForestM fs ()
papersErrors (rep,md) = do
	let err = get_errors md
	forestIO $ print (numErrors err)
	forestIO $ print (errorMsg err)

load_papers2 :: FSRep fs => FilePath -> ForestM fs ()
load_papers2 papersRoot = do
	dta@(rep::Papers2,md::Papers2_md) <- load () papersRoot
	papersErrors dta
	return ()

