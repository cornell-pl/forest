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
data Entry = Entry
             { w1 :: ws,
               val :: Double,
               w2 :: ws, bar :: "|", w3 :: ws, 
               tag :: StringME uc,  
               w4 :: ws, colon :: ":", w5 :: ws,
               desc :: StringLn
} 
type SwatLines = [Line SwatLine] terminator EOF
data SwatLine = SwatEntry Entry | SwatLine StringLn
data Preamble = Preamble
         { f :: [Line StringLn] length 2,
           title :: (Line StringLn, Line StringLn, Line StringLn),
           c :: Line StringLn,
           rest :: SwatLines
           
         }
type CIO = (Preamble, EOF)
|]

isEntry :: SwatLine -> Bool
isEntry (SwatEntry s) = True
isEntry (SwatLine l) = False

numberPCP :: SwatLines -> Int
numberPCP s = case s!!11 of
  SwatEntry e -> fromEnum $ val e

pcpFiles :: SwatLines -> Set FilePath
pcpFiles s = case drop 28 (take 31 s)  of
  xs -> foldr (\(SwatLine l1) l2 -> Set.fromList (words l1) `Set.union` l2) [] xs

validPCP :: Pcps -> Preamble -> Bool
validPCP this cio =
  length this == numberPCP (cioSwatLines cio)
  && Set.fromList (map fst this) == pcpFiles $ rest cio 

cioSwatLines :: Preamble -> SwatLines
cioSwatLines p = rest p

[forest| 
type Pcps (cio :: Preamble) = [f :: TextFile | f <- matches <|GL "*.pcp" |>] where <| validPCP this cio |>

type Swat_d = Directory 
             { all_files  is [ f :: TextFile     | f <- matches <|GL "*"|> ],
               cio is "file.cio" :: File Preamble
             , pcps :: Pcps cio
} |]

get :: FilePath -> IO Swat_d
get path = do
  { (rep, md) <- swat_d_load path
  ; return rep
  }

go :: IO ()
go = do
  { swat <- get "/home/richard/Documents/forest/TxtInOut"
  ; let swatLines = Swat.rest $ cio swat
  ; print $ numberPCP swatLines
  ; print $ pcpFiles swatLines
  ; print $ pcps swat
}
