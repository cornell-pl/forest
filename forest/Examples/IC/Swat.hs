{-# LANGUAGE ConstraintKinds, TypeFamilies, DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.IC.Beautiful.Swat where
import Language.Pads.Padsc hiding (take, rest, head, numErrors)
import Language.Forest.IC hiding (writeFile)

import Data.Maybe
import Data.IORef
import Language.Haskell.TH.Syntax

import System.Posix.Files
import Control.Concurrent
import Control.Concurrent.Async
import System.Directory
import System.TimeIt
import Control.Monad.IO.Class
import Data.WithClass.MData
import Data.DeepTypeable

import Data.WithClass.Derive.DeepTypeable
import Data.DeriveTH
import Data.WithClass.Derive.MData
import System.FilePath.Posix

import Control.Monad.Incremental.Display
import Data.List as List hiding (delete)
import Control.Monad.Incremental hiding (read, new)
import Prelude hiding (read)

import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set(..))
import Data.Char (isSpace)
import Control.Monad
import System.Environment

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
[ipads|
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

        type SLR = DateFloatFile
        type WND = DateFloatFile
        type RH = DateFloatFile
        type BSN = SwatFile
        type RTE = SwatFile
        type SWQ = SwatFile
        type HRU = SwatFile
        type MGT = SwatFile
        type GW = SwatFile
        type SEP = SwatFile
        type PND = SwatFile
 |]


dir = "/home/richard/Documents/forest/TxtInOut/"
dir2 = "fig.fig"

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

test :: FilePath -> IO (FIG, FIG_md)
test = parseFile

test_rep = liftM fst . test
test_md = liftM snd . test

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

[txforest|
         data CST = TextFile

         data FPreamble = File Preamble
         data FPCP = File PCP
         data FTMP = File TMP
         data FFIG = File FIG
         data FWND = File WND
         data FRH = File RH
         data FSLR = File SLR
         data FBSN = File BSN
         data FPLANT = File PLANT
         data FTILL = File TILL
         data FPEST = File PEST
         data FFERT = File FERT
         data FURBAN = File URBAN
         data FSUB = File SUB
         data FRTE = File RTE
         data FSWQ = File SWQ
         data FHRU = File HRU
         data FMGT = File MGT
         data FSOL = File SOL
         data FCHM = File CHM
         data FGW = File GW
         data FSEP = File SEP
         data FWGN = File WGN
         data FPND = File PND
         data FWUS = File WUS

         data Swat_d = Directory 
	     { cio is "file.cio" :: FPreamble
         , pcps is [f :: FPCP | f <- matches (GL "*.pcp") ]
         , tmps is [f :: FTMP | f <- matches (GL "*.tmp") ]
         , fig is (figFile $ swatLines cio) :: FFIG
         , cst matches (RE (cstFile $ swatLines cio)) :: Maybe CST
         , wnd matches (RE (wndFile $ swatLines cio)) :: Maybe (FWND)
         , rhf matches (RE (rhFile $ swatLines cio)) :: Maybe (FRH)
         , slr matches (RE (slrFile $ swatLines cio)) :: Maybe (FSLR)
         , bsn is (bsnFile $ swatLines cio) :: FBSN
         , plant is (plantFile $ swatLines cio) :: FPLANT
         , till is (tillFile $ swatLines cio) :: FTILL
         , pest is (pestFile $ swatLines cio) :: FPEST
         , fert is (fertFile $ swatLines cio) :: FFERT
         , urban is (urbanFile $ swatLines cio) :: FURBAN

         , subs is [f :: FSUB | f <- matches (GL "*.sub"), (f /= "output.sub")]
         , rtes is [f :: FRTE | f <- matches (GL "*.rte")]
         , swqs is [f :: FSWQ | f <- matches (GL "*.swq")]

         , hrus is [f :: FHRU | f <- matches (GL "*.hru"), (f /= "output.hru")]
         , mgts is [f :: FMGT | f <- matches (GL "*.mgt")]
         , sols is [f :: FSOL | f <- matches (GL "*.sol")]
         , chms is [f :: FCHM | f <- matches (GL "*.chm")]
         , gws is [f :: FGW | f <- matches (GL "*.gw")]
         , seps is [f :: FSEP | f <- matches (GL "*.sep")]
         , wgns is [f :: FWGN | f <- matches (GL "*.wgn")]
         , pnds is [f :: FPND | f <- matches (GL "*.pnd")]
         , wuss is [f :: FWUS | f <- matches (GL "*.wus")]
             } where (validSwat this)
 |]

validSwat (swat_md, swat) =
    let (cio_md, cio_info) = cio swat in
    let (fig_md, fig_info) = fig swat in
    let (pcp_md, pcp) = pcps swat in
    let (tmp_md, tmp) = tmps swat in
    let (sub_md, sub) = subs swat in
    let (rte_md, rte) = rtes swat in
    let (swq_md, swq) = swqs swat in
    let (hru_md, hru) = hrus swat in
    let (mgt_md, mgt) = mgts swat in
    let (sol_md, sol) = sols swat in
    let (chm_md, chm) = chms swat in
    let (gw_md, gw) = gws swat in
    let (sep_md, sep) = seps swat in
    ((tmpValid tmp (swatLines cio_info))
        && (pcpValid pcp (swatLines cio_info))
        && (subValid sub (figLines fig_info))
        && (rteValid rte (figLines fig_info))
        && (swqValid swq (figLines fig_info))
        && (hruValid hru (map (hruLines . snd) sub))
        && (mgtValid mgt (map (hruLines . snd) sub))
        && (solValid sol (map (hruLines . snd) sub))
        && (chmValid chm (map (hruLines . snd) sub))
        && (gwValid gw (map (hruLines . snd) sub))
        && (septValid sep (map (hruLines . snd) sub)))
