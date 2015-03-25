{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}
module Swat where
import Language.Pads.Padsc hiding (take, rest, head)
import Language.Pads.BaseTypes
import Language.Forest.Forestc hiding (test, numErrors)
import Language.Pads.GenPretty
import Language.Forest.Graph
import Language.Forest.Infer
import Language.Forest.Pretty
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set(..))
import Data.Char (isSpace)
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)

ws = REd "[ \t]*" " "
fileName = REd "[a-zA-Z.0-9]*" "a"
alphaNum = REd "[a-zA-Z0-9]*" "a"
uc = REd "[A-Z]+" "A"
alpha = REd "[a-zA-Z]*" "a"

trim :: String -> String
trim = let removeWS = dropWhile $ (`elem` " \r\t\NUL") in
       removeWS . reverse . removeWS . reverse

-- Known issue: we assume unix line endings for files on system (pcp, fig).
-- Note: doesn't work on default pcp.pcp or tmp.tmp file because it appears 
--       those files are missing newlines at the end. Not sure if this is an 
--       input error, or something I should take into account?
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
       data SwatFile = SwatFile {swatValues :: SwatLines}
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
             "add", StringLn
           }

       data SaveConcComm = SaveConcComm {
             "saveconc", Line StringLn, ws
           , concFile :: StringME fileName
           }

       data FinishComm = FinishComm {
             "finish", StringLn
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

        type SubLines = [Line SwatLine] terminator ("HRU: General", EOR)
        type HruLines = [Line HruFiles] terminator EOF
        data SUB = SUB {
              subLines :: SubLines
            , hruLines :: HruLines
            }

        type HeaderLine = (alphaNum, ws, Double)
        data PcpLine = PcpLine {
              pcpYear :: [Char] length 4
            , pcpDate :: [Char] length 3
            , precip :: [Char] length 5
            }

        type PcpLines = [(PcpLine, EOR)] terminator EOF

        data PCP = PCP {
              Line StringLn
            , [Line HeaderLine] length 3
            , pcpLines :: PcpLines
            }

        data TmpLine = TmpLine {
              tmpYear :: [Char] length 4
            , tmpDate :: [Char] length 3
            , maxTemp :: [Char] length 5
            , minTemp :: [Char] length 5
            }

        type TmpLines = [(TmpLine, EOR)] terminator EOF
        data TMP = TMP {
              Line StringLn
            , [Line HeaderLine] length 3
            , tmpLines :: TmpLines
            }

        type WSDouble = (ws, Double)
        type LabelDouble = ([Char] length 10, ws, "=", ws, Double)
        type PLine1 = [WSDouble] length 10
        type PLine2 = [WSDouble] length 5
        data PEntry = PEntry {
              ws, Int, ws, plantLabel :: StringME uc, ws, Int, EOR
            , [Line PLine1] length 3
            , PLine2
            }
        type PlantLines = [Line PEntry] terminator EOF
        data PLANT = PLANT {plantLines :: PlantLines}

        data TillLine = TillLine {
              ws, Int, ws
            , tillName :: [Char] length 8, ws
            , tillEff :: Double, ws
            , tillDepth :: Double, ws
            , Double}
        type TillLines = [Line TillLine] terminator EOF
        data TILL = TILL {tillLines :: TillLines}

        type ULine2 = [WSDouble] length 8
        data ULine1 = Uline1 {ws, Int, ws, urbanLabel :: StringME uc 
                           , [Char] length 56, WSDouble, WSDouble, EOR}
        type UrbanLine = (ULine1, ULine2)
        type UrbanLines = [Line UrbanLine] terminator EOF
        data URBAN = URBAN {urbanLines :: UrbanLines}

        type WusLine = [WSDouble] length 6
        type WusLines = [Line WusLine] terminator EOF
        data WUS = WUS {
              [Line StringLn] length 3
            , wusLines :: WusLines
            }

        type WGNLine = [WSDouble] length 12
        type WGNLatLong = [LabelDouble] length 2
        type WGNLines = [Line WGNLine] terminator EOF
        data WGN = WGN {
              Line StringLn
            , wgnLatLong :: Line WGNLatLong
            , wgnElev :: Line LabelDouble
            , wgnRain :: Line LabelDouble
            , wgnLines :: WGNLines
            }

        type ColonLabel = [Char] terminator ":"
        type CLine1 = [WSDouble] length 10
        type CHMLine = (ColonLabel, CLine1)
        type CHMEndLine = [WSDouble] length 3
        type CHMEndLines = [Line CHMEndLine] terminator EOF
        data CHM = CHM {
              [Line StringLn] length 2
            , chmData :: [Line CHMLine] length 6
            , [Line StringLn] length 3
            , chmPestData :: CHMEndLines
            }

        data DateFloatLine = DateFloatLine {
              year :: [Char] length 4
            , date :: [Char] length 3
            , info :: [Char] length 8
            }

        type DateFloatLines = [Line DateFloatLine] terminator EOF
        data DateFloatFile = DateFloatFile {
              Line StringLn
            , dateFloatLines :: DateFloatLines
            }

        data PestLine = PestLine {
              pestNum :: [Char] length 3
            , pestName :: [Char] length 17
            , pestSKOC :: [Char] length 10
            , pestWOF :: [Char] length 5
            , pestHLIFE_F :: [Char] length 8
            , pestHLIFE_S :: [Char] length 8
            , pestAP_EF :: [Char] length 5
            , pestWSOL :: [Char] length 11
            }
        type PestLines = [Line PestLine] terminator EOF
        data PEST = PEST {pestLines :: PestLines}

        data FertLine = FertLine {
              fertNum :: [Char] length 4, " "
            , fertName :: [Char] length 8
            , fertMineralN :: [Char] length 8
            , fertMineralP :: [Char] length 8
            , fertOrgN :: [Char] length 8
            , fertOrgP :: [Char] length 8
            , fertAmmN :: [Char] length 8
            , bactPers :: [Char] length 8
            , bactLP :: [Char] length 10
            , bactPart :: [Char] length 10
            }
        type FertLines = [Line FertLine] terminator EOF
        data FERT = FERT {fertLines :: FertLines}
                 
        type SolLineDesc = [Char] terminator ":"
        type SolLineTopStr = (SolLineDesc,  ws, StringLn)
        type SolLineTopVal = (SolLineDesc, ws, Double)
        type SolLineBot = (SolLineDesc, WSDouble, WSDouble)
        data SOL = SOL {
              Line StringLn
            , solDesc :: [Line SolLineTopStr] length 2
            , solVals :: [Line SolLineTopVal] length 3
            , Line StringLn
            , solBotVals :: [Line SolLineBot] length 14
            }

 |]

dir = "/home/richard/Documents/forest/TxtInOut/"

testSOL :: IO (SOL, SOL_md)
testSOL = parseFile (dir ++ "000050001.sol")

testSOL_md = liftM snd testSOL

testCHM :: IO (CHM, CHM_md)
testCHM = parseFile (dir ++ "000050001.chm")

testCHM_md = liftM snd testCHM

testWGN :: IO (WGN, WGN_md)
testWGN = parseFile (dir ++ "000050000.wgn")

testwgn_md = liftM snd testWGN

testWus :: IO (WUS, WUS_md)
testWus = parseFile (dir ++ "000050000.wus")

testwus_md = liftM snd testWus

testurban :: IO (URBAN, URBAN_md)
testurban = parseFile (dir ++ "urban.dat")

testurban_md = liftM snd testurban

testtill :: IO (TILL, TILL_md)
testtill = parseFile (dir ++ "till.dat")

testtill_md = liftM snd testtill

testpcp :: IO (PCP, PCP_md)
testpcp = parseFile (dir ++ "pcp1.pcp")

testpcp_rep = liftM fst testpcp
testpcp_md = liftM snd testpcp

testtmp :: IO (TMP, TMP_md)
testtmp = parseFile (dir ++ "tmp.tmp")

testtmp_md = liftM snd testtmp

testplant :: IO (PLANT, PLANT_md)
testplant = parseFile (dir ++ "plant.dat")

testplant_md = liftM snd testplant

test :: IO (FIG, FIG_md)
test = parseFile "/home/richard/Documents/forest/TxtInOut/fig.fig"

test_rep = liftM fst test
test_md = liftM snd test

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
         type CST = TextFile
         type SLR = File DateFloatFile
         type WND = File DateFloatFile
         type RH = File DateFloatFile
         type BSN = File SwatFile
         type RTE = File SwatFile
         type SWQ = File SwatFile
         type HRU = File SwatFile
         type MGT = File SwatFile
         type GW = File SwatFile
         type SEP = File SwatFile
         type PND = File SwatFile

         type Swat_d = Directory 
	     { cio is "file.cio" :: File Preamble
	     , pcps is [f :: File PCP | f <- matches <|GL "*.pcp" |>]
             , tmps is [f :: File TMP | f <- matches <|GL "*.tmp" |>]
             , fig is <| figFile $ swatLines cio |> :: File FIG
             , cst matches <| RE (cstFile $ swatLines cio) |> :: Maybe CST
             , wnd matches <| RE (wndFile $ swatLines cio) |> :: Maybe WND
             , rhf matches <| RE (rhFile $ swatLines cio) |> :: Maybe RH
             , slr matches <| RE (slrFile $ swatLines cio) |> :: Maybe SLR
             , bsn is <| bsnFile $ swatLines cio |> :: BSN
             , plant is <| plantFile $ swatLines cio |> :: File PLANT
             , till is <| tillFile $ swatLines cio |> :: File TILL
             , pest is <| pestFile $ swatLines cio |> :: File PEST
             , fert is <| fertFile $ swatLines cio |> :: File FERT
             , urban is <| urbanFile $ swatLines cio |> :: File URBAN

             , subs is [f :: File SUB | f <- matches <|GL "*.sub" |>, <| f /= "output.sub" |>]
             , rtes is [f :: RTE | f <- matches <|GL "*.rte" |>]
             , swqs is [f :: SWQ | f <- matches <|GL "*.swq" |>]

             , hrus is [f :: HRU | f <- matches <|GL "*.hru" |>, <| f /= "output.hru" |>]
             , mgts is [f :: MGT | f <- matches <|GL "*.mgt" |>]
             , sols is [f :: File SOL | f <- matches <|GL "*.sol" |>]
             , chms is [f :: File CHM | f <- matches <|GL "*.chm" |>]
             , gws is [f :: GW | f <- matches <|GL "*.gw" |>]
             , seps is [f :: SEP | f <- matches <|GL "*.sep" |>]
             , wgns is [f :: File WGN | f <- matches <|GL "*.wgn" |>]
             , pnds is [f :: PND | f <- matches <|GL "*.pnd" |>]
             , wuss is [f :: File WUS | f <- matches <|GL "*.wus" |>]

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

md :: FilePath -> IO Swat_d_md
md path = do
  (rep, md) <- swat_d_load path
  return md
	

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
  print figInfo
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
	
