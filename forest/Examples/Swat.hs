{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}
module Swat where
import Language.Pads.Padsc hiding (take, rest, head, numErrors)
import Language.Pads.BaseTypes
import Language.Forest.Forestc
import Language.Pads.GenPretty
import Language.Forest.Graph
import Language.Forest.Infer
import Language.Forest.Pretty
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set(..))
import Data.Char (isSpace)

import System.IO.Unsafe (unsafePerformIO)
ws = REd "[ \t]*" " "
fileName = REd "[a-zA-Z.0-9]*" "a"
uc = REd "[A-Z]+" "A"

trim :: String -> String
trim = let removeWS = dropWhile $ (`elem` " \r\t\NUL") in
       removeWS . reverse . removeWS . reverse

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
             "subbasin", Line StringLn, ws
           , subFile :: StringME fileName
           }
                    
       data RouteComm = RouteComm {
             "route", Line StringLn, ws
           , rteFile :: [Char] length 13
           , swqFile :: StringME fileName
           }

       data AddComm = AddComm {
             "add", StringLn, EOR
           }

       data SaveConcComm = SaveConcComm {
             "saveconc", StringLn, EOR, ws
           , concFile :: StringME fileName
           }

       data FinishComm = FinishComm {
             "finish", StringLn, EOR
           }

       data FigLine = Sub SubComm | Route RouteComm | Add AddComm | SaveConc SaveConcComm | Finish FinishComm
       type FigLines = [Line FigLine] terminator EOF 
       data FIG = FIG {figLines :: FigLines}

       data HruFiles = HruFiles {
             hruFile :: [Char] length 13
           , mgtFile :: [Char] length 13
           , solFile :: [Char] length 13
           , chmFile :: [Char] length 13
           , gwFile :: [Char] length 13
           , opsFile :: [Char] length 13
           , septFile :: [Char] length 13
           , sdrFile :: [Char] length 13
           }

        type SubLines = [Line SwatLine] terminator "HRU: General"
        type HruLines = [Line HruFiles] terminator EOF
        data SUB = SUB {
              subLines :: SubLines
            , hruLines :: HruLines
            }
 |]

isEntry :: SwatLine -> Bool
isEntry (SwatLine l) = False
isEntry (_) = True

figFile :: SwatLines -> FilePath
figFile s = case s !! 0 of
              SwatLine e -> head $ words e

getSwatStringVal :: Int -> SwatLines -> FilePath
getSwatStringVal n s = case s !! n of
                SwatLine _ -> ""
                SwatString s -> strVal s

cstFile :: SwatLines -> FilePath
cstFile = getSwatStringVal 38

slrFile :: SwatLines -> FilePath
slrFile = getSwatStringVal 35

rhFile :: SwatLines -> FilePath
rhFile = getSwatStringVal 36

wndFile :: SwatLines -> FilePath
wndFile = getSwatStringVal 37

bsnFile :: SwatLines -> FilePath
bsnFile = getSwatStringVal 40

plantFile :: SwatLines -> FilePath
plantFile = getSwatStringVal 42

tillFile :: SwatLines -> FilePath
tillFile = getSwatStringVal 43

pestFile :: SwatLines -> FilePath
pestFile = getSwatStringVal 44

fertFile :: SwatLines -> FilePath
fertFile = getSwatStringVal 45

urbanFile :: SwatLines -> FilePath
urbanFile = getSwatStringVal 46

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

subFiles :: FigLines -> [FilePath]
subFiles f = foldl (\acc l -> case l of
                                Sub s -> ((subFile s):acc)
                                _ -> acc) [] f

rteFiles :: FigLines -> [FilePath]
rteFiles f = foldl (\acc l -> case l of
                                Route r -> ((rteFile r):acc)
                                _ -> acc) [] f

swqFiles :: FigLines -> [FilePath]
swqFiles f = foldl (\acc l -> case l of
                                Route r -> ((swqFile r):acc)
                                _ -> acc) [] f

subValid :: [(FilePath, SUB)] -> FigLines -> Bool
subValid subs figlines =
    Set.fromList (map fst subs) == Set.fromList (subFiles figlines)

rteValid :: [(FilePath, RTE)] -> FigLines -> Bool
rteValid rtes figlines =
    Set.fromList (map fst rtes) == Set.fromList (rteFiles figlines)

swqValid :: [(FilePath, SWQ)] -> FigLines -> Bool
swqValid swqs figlines =
    Set.fromList (map fst swqs) == Set.fromList (swqFiles figlines)

getFigStringVal :: Int -> SubLines -> FilePath
getFigStringVal n s = case s !! n of
                SwatLine _ -> ""
                SwatString s -> strVal s

wgnFile :: SubLines -> FilePath
wgnFile = getFigStringVal 12

pndFile :: SubLines -> FilePath
pndFile = getFigStringVal 31

wusFile :: SubLines -> FilePath
wusFile = getFigStringVal 33

getHruFiles :: (HruFiles -> FilePath) -> HruLines -> [FilePath]
getHruFiles f l = foldl (\acc l -> let next = trim $ f l in
                                case next of
                                  "" -> acc
                                  _ -> next:acc) [] l 

hruFiles :: HruLines -> [FilePath]
hruFiles = getHruFiles hruFile

mgtFiles :: HruLines -> [FilePath]
mgtFiles = getHruFiles mgtFile

solFiles :: HruLines -> [FilePath]
solFiles = getHruFiles solFile

chmFiles :: HruLines -> [FilePath]
chmFiles = getHruFiles chmFile

gwFiles :: HruLines -> [FilePath]
gwFiles = getHruFiles gwFile

opsFiles :: HruLines -> [FilePath]
opsFiles = getHruFiles opsFile

septFiles :: HruLines -> [FilePath]
septFiles = getHruFiles septFile

sdrFiles :: HruLines -> [FilePath]
sdrFiles = getHruFiles sdrFile

hruValid :: [(FilePath, HRU)] -> [HruLines] -> Bool
hruValid hrus hruLines =
    Set.fromList (map fst hrus) == Set.fromList (concat (map hruFiles hruLines))

mgtValid :: [(FilePath, MGT)] -> [HruLines] -> Bool
mgtValid mgts hruLines = 
    Set.fromList (map fst mgts) == Set.fromList (concat (map mgtFiles hruLines))

solValid :: [(FilePath, SOL)] -> [HruLines] -> Bool
solValid sols hruLines =
    Set.fromList (map fst sols) == Set.fromList (concat (map solFiles hruLines))

chmValid :: [(FilePath, CHM)] -> [HruLines] -> Bool
chmValid chms hruLines =
    Set.fromList (map fst chms) == Set.fromList (concat (map chmFiles hruLines))

gwValid :: [(FilePath, GW)] -> [HruLines] -> Bool
gwValid gws hruLines =
    Set.fromList (map fst gws) == Set.fromList (concat (map gwFiles hruLines))

septValid :: [(FilePath, SEP)] -> [HruLines] -> Bool
septValid sols hruLines =
    Set.fromList (map fst sols) == Set.fromList (concat (map solFiles hruLines))

[forest|
         type PCP = TextFile
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
         type RTE = TextFile
         type SWQ = TextFile
         type HRU = TextFile
         type MGT = TextFile
         type SOL = TextFile
         type CHM = TextFile
         type GW = TextFile
         type SEP = TextFile
         type WUS = TextFile
         type PND = TextFile
         type WGN = TextFile

         type Swat_d = Directory 
	     { cio is "file.cio" :: File Preamble
             , output is "output.sub" :: TextFile
	     , pcps is [f :: PCP | f <- matches <|GL "*.pcp" |>]
             , tmps is [f :: TMP | f <- matches <|GL "*.tmp" |>]
             , fig is <| figFile $ swatLines cio |> :: File FIG
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

             , subs is [f :: File SUB | f <- matches <|GL "*.sub" |>, <| f /= "output.sub" |>]
             , rtes is [f :: RTE | f <- matches <|GL "*.rte" |>]
             , swqs is [f :: SWQ | f <- matches <|GL "*.swq" |>]

             , hrus is [f :: HRU | f <- matches <|GL "*.hru" |>]
             , mgts is [f :: MGT | f <- matches <|GL "*.mgt" |>]
             , sols is [f :: SOL | f <- matches <|GL "*.sol" |>]
             , chms is [f :: CHM | f <- matches <|GL "*.chm" |>]
             , gws is [f :: GW | f <- matches <|GL "*.gw" |>]
             , seps is [f :: SEP | f <- matches <|GL "*.sep" |>]
             , wgns is [f :: WGN | f <- matches <|GL "*.wgn" |>]
             , pnds is [f :: PND | f <- matches <|GL "*.pnd" |>]
             , wuss is [f :: WUS | f <- matches <|GL "*.wus" |>]

             , all_files  is [ f :: TextFile     | f <- matches <|GL "*"|> ]
             } where <| pcpValid (pcps this) (swatLines $ cio this)
             && tmpValid (tmps this) (swatLines $ cio this)
             && subValid (subs this) (figLines $ fig this) 
             && rteValid (rtes this) (figLines $ fig this)
             && swqValid (swqs this) (figLines $ fig this)
             && hruValid (hrus this) (map (hruLines . snd) $ subs this)
             && mgtValid (mgts this) (map (hruLines . snd) $ subs this)
             && solValid (sols this) (map (hruLines . snd) $ subs this)
             && chmValid (chms this) (map (hruLines . snd) $ subs this)
             && gwValid (gws this) (map (hruLines . snd) $ subs this)
             && septValid (seps this) (map (hruLines . snd) $ subs this)|>
|]

get :: FilePath -> IO Swat_d
get path = do
  (rep, md) <- swat_d_load path
  return rep
	

testCIO :: IO ()
testCIO = do
  swat <- get "/home/richard/Documents/forest/TxtInOut"
  let swatInfo = swatLines $ cio swat
  print $ pcpNumber swatInfo
  print $ pcpFiles swatInfo
  print $ tmpNumber swatInfo
  print $ tmpFiles swatInfo
  print $ pcps swat
  print $ figFile swatInfo
  print $ tmps swat
  print $ swatInfo !! 38
  print $ slrFile swatInfo
  print $ slr swat
  print $ wnd swat
  print $ cst swat
  print $ urban swat
  print $ bsn swat
  print $ fig swat

testFIG :: IO ()
testFIG = do
  swat <- get "/home/richard/Documents/forest/TxtInOut"
  let figInfo = figLines $ fig swat
  print $ subFiles figInfo
  print $ rteFiles figInfo
  print $ swqFiles figInfo
  print $ map fst $ subs swat
--  print $ swqs swat

testSUB :: IO ()
testSUB = do
  swat <- get "/home/richard/Documents/forest/TxtInOut"
  let subInfo = map (subLines . snd) $ subs swat
  let hruInfo = map (hruLines . snd) $ subs swat
  print $ concat $ map hruFiles hruInfo
  print $ chms swat
  return ()
	
