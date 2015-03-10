{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}
module Swat where
import Language.Pads.Padsc hiding (take, rest, head)
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
fileName = REd "[a-zA-Z.]*" "a"
uc = REd "[A-Z]+" "A"

[pads|
       data NumEntry = NumEntry {
             ws
           , numVal :: Double
           , ws, "|", ws
           , numTag :: StringME uc
           , ws, ":", ws
           , numDesc :: StringLn
           } 

       data StrEntry = StrEntry {
             ws
           , strVal :: StringME fileName
           , ws, "|", ws
           , strTag :: StringME uc
           , ws, ":", ws
           , strDesc :: StringLn
           }
           
       type SwatLines = [Line SwatLine] terminator EOF
       data SwatLine = SwatDouble NumEntry | SwatString StrEntry | SwatLine StringLn
       data Preamble = Preamble
           { f :: [Line StringLn] length 2
           , title :: (Line StringLn, Line StringLn, Line StringLn)
           , c :: Line StringLn
           , swatLines :: SwatLines
           }
       type CIO = (Preamble, EOF)


       data SubComm = SubComm {
             "subbasin"
           , StringLn
           , ws
           , subFile :: StringME fileName
           }

 
 |]

isEntry :: SwatLine -> Bool
isEntry (SwatLine l) = False
isEntry (_) = True

figFile :: SwatLines -> FilePath
figFile s = case s !! 0 of
              SwatLine e -> head $ words e

getStringVal :: Int -> SwatLines -> FilePath
getStringVal n s = case s !! n of
                SwatLine _ -> ""
                SwatString s -> strVal s

cstFile :: SwatLines -> FilePath
cstFile = getStringVal 38

slrFile :: SwatLines -> FilePath
slrFile = getStringVal 35

rhFile :: SwatLines -> FilePath
rhFile = getStringVal 36

wndFile :: SwatLines -> FilePath
wndFile = getStringVal 37

bsnFile :: SwatLines -> FilePath
bsnFile = getStringVal 40

plantFile :: SwatLines -> FilePath
plantFile = getStringVal 42

tillFile :: SwatLines -> FilePath
tillFile = getStringVal 43

pestFile :: SwatLines -> FilePath
pestFile = getStringVal 44

fertFile :: SwatLines -> FilePath
fertFile = getStringVal 45

urbanFile :: SwatLines -> FilePath
urbanFile = getStringVal 46

pcpNumber :: SwatLines -> Int
pcpNumber s = case s!!11 of
                SwatDouble e -> fromEnum $ numVal e

pcpFiles :: SwatLines -> [FilePath]
pcpFiles s = case drop 28 (take 31 s)  of
               xs -> foldr (\(SwatLine l1) l2 -> words l1 ++ l2) [] xs

pcpValid :: [(FilePath,PCP)] -> SwatLines -> Bool
pcpValid pcps swatlines =
    length pcps == pcpNumber swatlines
               && Set.fromList (map fst pcps) == Set.fromList (pcpFiles swatlines)

tmpNumber :: SwatLines -> Int
tmpNumber s = case s !! 15 of
                SwatDouble e -> fromEnum $ numVal e

tmpFiles :: SwatLines -> [FilePath]
tmpFiles s = case drop 32 (take 35 s) of
               xs -> foldr (\(SwatLine l1) l2 -> words l1 ++ l2) [] xs

tmpValid :: [(FilePath, TMP)] -> SwatLines -> Bool
tmpValid tmps swatlines =
    length tmps == tmpNumber swatlines
             && Set.fromList (map fst tmps) == Set.fromList (tmpFiles swatlines)

[forest|
         type PCP = TextFile

         type FIG = TextFile
         type CST = TextFile
         type SLR = TextFile
         type WND = TextFile
         type RH = TextFile
         type BSN = TextFile
         type PLANT = TextFile
         type TILL = TextFile
         type PEST = TextFile
         type FERT = TextFile
         type URBAN = TextFile

         type TMP = TextFile
         type TMPs (cio :: Preamble) = [f :: TMP | f <- matches <|GL "*.tmp" |>] where <| tmpValid this (swatLines cio) |>

         type Swat_d = Directory 
	     { cio is "file.cio" :: File Preamble
	     , pcps is [f :: PCP | f <- matches <|GL "*.pcp" |>]
         , tmps :: TMPs cio
         , fig is <| figFile $ swatLines cio |> :: FIG
         , cst matches <| RE (cstFile $ swatLines cio) |> :: Maybe CST
         , wnd matches <| RE (wndFile $ swatLines cio) |> :: Maybe WND
         , rhf matches <| RE (rhFile $ swatLines cio) |> :: Maybe RH
         , slr matches <| RE (slrFile $ swatLines cio) |> :: Maybe SLR
         , bsn is <| bsnFile $ swatLines cio |> :: BSN
         , plant is <| plantFile $ swatLines cio |> :: PLANT
         , till is <| tillFile $ swatLines cio |> :: TILL
         , pest is <| pestFile $ swatLines cio |> :: PEST
         , fert is <| fertFile $ swatLines cio |> :: FERT
         , urban is <| urbanFile $ swatLines cio |> :: URBAN
         , all_files  is [ f :: TextFile     | f <- matches <|GL "*"|> ]
         } where <| pcpValid (pcps this) (swatLines $ cio this) |>
|]

get :: FilePath -> IO Swat_d
get path = do
  (rep, md) <- swat_d_load path
  return rep
	

go :: IO ()
go = do
  swat <- get "/home/richard/Documents/forest/TxtInOut"
  let swatInfo = swatLines $ cio swat
  print $ pcpNumber swatInfo
  print $ pcpFiles swatInfo
  print $ tmpNumber swatInfo
  print $ tmpFiles swatInfo
  print $ pcps swat -- Does not work
  print $ figFile swatInfo
  print $ tmps swat -- Does not work
  print $ swatInfo !! 38
  print $ slrFile swatInfo
  print $ slr swat
  print $ wnd swat
  print $ cst swat
  print $ urban swat
--  print $ bsn swat
--  print $ fig swat
	
