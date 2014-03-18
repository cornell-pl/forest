{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables, ViewPatterns #-}

module Examples.Papers2 where

import Language.Pads.Padsc hiding (take,numErrors)
import Language.Forest.Forestc hiding (sources)
import Language.Forest.Graph
import Data.List
import Data.Char
import Language.Forest.Delta
import Data.Maybe
import Data.List.Split

import Debug.Trace
import System.TimeIt

[forest|
	type Paper (y :: Maybe Integer,n :: Maybe String) = BinaryFile where <| isPaperOf (get_fullpath this_att) y n |>

    type Supplemental (y :: Maybe Integer,n :: Maybe String) = Directory {
		supplementalFiles is Map [ p :: Paper <|(y,n)|> | p <- matches <| GL "*" |>, <| isFile p_att && not (hidden p) |> ] 
	}

	type Author (y :: Maybe Integer,n :: Maybe String) = Directory {
		authorPapers is Map [ p :: Paper <|(y,n)|> | p <- matches <| GL "*" |>, <| isFile p_att && not (hidden p) |> ] 
	,   supplemental is "Supplemental" :: Maybe (Supplemental <|(y,n)|>)
	}

	type Year (y :: Maybe Integer) = Directory {
		authors is Map [ n :: Author <| (y,getAuthor n) |> | n <- matches <| GL "*" |>, <| not (hidden n) |> ]
	}

	type Articles = Map [ y :: Year <| getYear y |> | y <- matches yearRE  ]
	type Books    = Map [ y :: Year <| getYear y |> | y <- matches yearRE  ]
	type Media    = Map [ y :: Year <| getYear y |> | y <- matches yearRE  ]
	type Reports  = Map [ y :: Year <| getYear y |> | y <- matches yearRE  ]

	type Papers2 = Directory {
		articles is "Articles" :: Articles
	,   books is "Books" :: Books
	,   media is "Media" :: Media
	,   reports is "Reports" :: Reports
	}
|]

hidden :: String -> Bool
hidden = isPrefixOf "."

isPaperOf :: FilePath -> Maybe Integer -> Maybe String -> Bool
isPaperOf (toLowerString -> file) myear (fmap toLowerString -> mname) = isSuffixOf suffix filename || (isSuffixOf suffix name && and (map (isNumber) num))
    where (filename,ext) = fileExtension (localname file)
          (name,num) = splitOnTwo "-" filename -- when multiple papers with the same year and author exist
          suffix = foldr1Safe (\x y -> x ++" "++y) (maybeToList (fmap show myear) ++ maybeToList mname)

yearRE = RE "[0-9][0-9][0-9][0-9]|Unknown"

getYear :: String -> Maybe Integer
getYear "Unknown" = Nothing
getYear str = Just $ read str

getAuthor :: String -> Maybe String
getAuthor "Unknown" = Nothing
getAuthor str = Just str

isPDF :: String -> Bool
isPDF str = map toLower str == "pdf"

isFile :: (ForestMD md) => md -> Bool
isFile att = get_kind att /= DirectoryK

isNumberString :: String -> Bool
isNumberString = and . map isNumber

toLowerString :: String -> String
toLowerString = map toLower

foldr1Safe f [] = ""
foldr1Safe f l = foldr1 f l

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
	mdToPDF md "/Users/hpacheco/Documents/Papers2/papers2.pdf"
	return (rep,md)

-- > (rep,md) <- timeIt $ papers2_dot
-- > (drep,dmd) <- timeIt $ papers2Delta repmd [AddFile "..."]
-- >  show drep
-- > rep' <- timeIt $ applyValueDeltas drep rep




















