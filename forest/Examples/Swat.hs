{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}
module Swat where
import Language.Pads.Padsc hiding (take, rest)
import Language.Pads.BaseTypes
import Language.Forest.Forestc
import Language.Pads.GenPretty
import Language.Forest.Graph
import Language.Forest.Infer
import Language.Forest.Pretty
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set(..))

import System.IO.Unsafe (unsafePerformIO)
ws = REd "[ \t]*" " "
uc = REd "[A-Z]+" "A"


[pads|
data Entry = Entry {
	  ws
	, val :: Double
	, ws, "|", ws
	, tag :: StringME uc
	, ws, ":", ws
	, desc :: StringLn
	} 

type SwatLines = [Line SwatLine] terminator EOF
data SwatLine = SwatEntry Entry | SwatLine StringLn
data Preamble = Preamble
	{ f :: [Line StringLn] length 2
	, title :: (Line StringLn, Line StringLn, Line StringLn)
	, c :: Line StringLn
	, swatLines :: SwatLines
	}
type CIO = (Preamble, EOF)
|]

isEntry :: SwatLine -> Bool
isEntry (SwatEntry s) = True
isEntry (SwatLine l) = False

numberPCP :: SwatLines -> Int
numberPCP s = case s!!11 of
  SwatEntry e -> fromEnum $ val e

pcpFiles :: SwatLines -> [FilePath]
pcpFiles s = case drop 28 (take 31 s)  of
  xs -> foldr (\(SwatLine l1) l2 -> words l1 ++ l2) [] xs

validPCP :: [(FilePath,PCP)] -> SwatLines -> Bool
validPCP pcps swatlines =
	length pcps == numberPCP swatlines
	&&
	Set.fromList (map fst pcps) == Set.fromList (pcpFiles swatlines)

[forest|
type PCP = TextFile
type PCPs (cio :: Preamble) = [f :: PCP | f <- matches <|GL "*.pcp" |>] where <| validPCP this (swatLines cio) |>

type Swat_d = Directory 
	{ all_files  is [ f :: TextFile     | f <- matches <|GL "*"|> ]
	, cio is "file.cio" :: File Preamble
	, pcps :: PCPs cio
} |]

get :: FilePath -> IO Swat_d
get path = do
	(rep, md) <- swat_d_load path
	return rep
	

go :: IO ()
go = do
	swat <- get "/home/richard/Documents/forest/TxtInOut"
	let swatlines = swatLines $ cio swat
	print $ numberPCP swatlines
	print $ pcpFiles swatlines
	print $ pcps swat
	
