{-# LANGUAGE TupleSections, ConstraintKinds, TypeFamilies, DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.IC.SwatOpt where

import Language.Pads.Padsc hiding (take, rest, head, numErrors)
import Language.Forest.IC hiding (writeFile)
import Examples.IC.Swat
import Examples.IC.Universal

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Incremental hiding (read, new)
import Control.Monad.Incremental.Display
import Control.Monad.IO.Class
import Data.WithClass.Derive.DeepTypeable
import Data.DeepTypeable
import Data.DeriveTH
import Data.IORef
import Data.List as List hiding (delete)
import Data.Maybe
import Data.WithClass.MData
import Data.WithClass.Derive.MData
import Language.Haskell.TH.Syntax
import Numeric
import Prelude hiding (read)
import System.Directory
import System.Random
import System.TimeIt
import System.FilePath.Posix
import System.Posix.Files

import System.Process
import System.IO.Temp
import System.Environment

[ipads|
data FlowEntry = FlowEntry {
    flowAgency :: StringME uc
  , ws, flowLocation :: Int
  , ws, flowYear :: [Char] length 4, "-", flowMonth :: [Char] length 2, "-", flowDay :: [Char] length 2
  , ws, flowAmount :: Double, StringLn
}

type FlowLines = [Line FlowEntry] terminator EOF
data Flow = Flow {
    [Line StringLn] length 27
  , flowInfo :: FlowLines
}

data RCHEntry = RCHEntry {
  [Char] length 37,
  rchInFlow :: [Char] length 10,
  StringLn
}
type RCHLines = [Line RCHEntry] terminator EOF
data RCH = RCH {
    [Line StringLn] length 9
  , rchInfo :: RCHLines
}

data Max = Max Double
|]

[txforest|
data FFlow = File Flow
data FRCH = File RCH
data MaxFile = File Max
|]

escoRange = (0.1, 1.0)
surlagRange = (0.0, 15.0)

rootDir = "/home/vagrant/forest/iforest" -- change this to proper location
optDir = rootDir </> "Examples/IC/bolo_arriba"

maxFile = "/tmp/maxOpt"
flowFile = rootDir </> "Examples/IC/Swat/flow.dat"
executableSWAT = rootDir </> "Examples/IC/Swat/rswat2012.exe"

getNewDirectory :: IO FilePath
getNewDirectory = do
  threadID <- myThreadId
  createTempDirectory "/tmp/Swat/" $ (drop 9 $ show threadID)

runSwat :: FilePath -> IO ()
runSwat path = do
  setCurrentDirectory path
  callProcess executableSWAT []

writeCurrent :: FilePath -> IO ()
writeCurrent maxFile = do
  ((max_rep, max_md) :: (Max, Max_md)) <- parseFile maxFile
  printFile maxFile (Max 500000, max_md)

optSwat :: Int -> IO ()
optSwat n = replicateM_ n optSwatIter -- 6 is number of iterations

newValue :: (Floating a, Ord a, Random a) => (a, a) -> a -> IO a
newValue (min, max) current = do
  (randValue :: a) <- randomRIO (-1, 1)
  let new = current + randValue * (max - min) / 3 -- this is arbitrary
  return (case compare min new of
  	LT | new < max -> new
  	   | new >= max -> max
  	_ -> min)

copyData :: FilePath -> IO ()
copyData tmpDir = do
  err <- atomically $ ((do
    (original :: Universal_d TxVarFS) <- new () optDir
    (copy :: Universal_d TxVarFS) <- new () tmpDir
    copyOrElse original copy "" $ return . show) :: FTM TxVarFS String)
  putStrLn err

optSwatIter :: IO ()
optSwatIter = do
  tmpDir <- getNewDirectory
  copyData tmpDir
  runOpt tmpDir

runOpt :: FilePath -> IO ()
runOpt tmpDir = do
  let copiedBsnFile = tmpDir </> "basins.bsn"
  ((bsnRep, bsnMd) :: (SwatFile, SwatFile_md)) <- parseFile copiedBsnFile
  let bsnValues = swatValues bsnRep
  newEsco <- newValue escoRange $ getEsco bsnValues
  newSurlag <- newValue surlagRange $ getSurlag bsnValues
  let newBsn = setSurlag newSurlag $ setEsco newEsco bsnValues -- figure out nicer syntax here
  printFile copiedBsnFile $ (SwatFile {swatValues=newBsn}, bsnMd)
  
  _ <- runSwat tmpDir

  -- Not worried about transactions here, so I'm using the pads interface
  -- instead of the forest one to avoid transactional overhead
  ((flowRep, flowMd) :: (Flow, Flow_md)) <- parseFile flowFile
  let firstFlowValue = flowAmount . head . flowInfo $ flowRep
  ((rchRep, rchMd) :: (RCH, RCH_md)) <- parseFile $ tmpDir </> "output.rch"
  let calcFlow = trim . rchInFlow . head . rchInfo $ rchRep
  let (calcFlowValue :: Double) = fst . head $ readFloat calcFlow
  -- Not the actual function here, but trying something easy before moving on
  let optVal = abs $ (firstFlowValue / 86400.0) - calcFlowValue
  putStrLn $ show optVal
  msg <- atomically $ ((do
    (maxInfo :: MaxFile TxVarFS) <- new () maxFile
    ((max_fmd, max_md), Max m) <- read maxInfo
    case m > optVal of
      True -> do
        (swatRep :: Swat_d TxVarFS) <- new () $ optDir
        (dir_md, dir) <- read swatRep
        (bsn_md, _) <- read $ bsn dir
        _ <- writeOrElse maxInfo ((max_fmd, max_md), Max optVal) "" $ return . show
        msg <- writeOrElse (bsn dir) (bsn_md, SwatFile newBsn) "Updated" $ return . show
        return msg
      False -> return "Not updated") :: FTM TxVarFS String)
  putStrLn msg  

main :: IO ()
main = do
  _ <- writeCurrent maxFile
  replicateM_ 2 $ forkIO $ optSwat 6 -- 2 is number of threads, starting small

getEsco :: SwatLines -> Double
getEsco lines =
  case lines !! 12 of
    SwatDouble d -> doubleVal d
    _ -> 0

setEsco :: Double -> SwatLines -> SwatLines
setEsco newDouble lines =
  let start = take 12 lines in
  let end = drop 13 lines in
  case lines !! 12 of
    SwatDouble d -> start ++ [SwatDouble (d {doubleVal = newDouble})] ++ end
    x -> lines

getSurlag :: SwatLines -> Double
getSurlag lines =
  case lines !! 19 of
    SwatDouble d -> doubleVal d
    _ -> 0

-- Should rethink this
mapEntry :: IntEntry -> DoubleEntry
mapEntry entry =
  DoubleEntry { doubleJunkWS = intJunkWS entry,
    doubleVal = fromInt $ intVal entry,
    doubleJunkWS2 = intJunkWS2 entry,
    doubleJunkWS3 = intJunkWS3 entry,
    doubleTag = intTag entry,
    doubleJunkWS4 = intJunkWS4 entry,
    doubleJunkWS5 = intJunkWS5 entry,
    doubleDesc = intDesc entry}

setSurlag :: Double -> SwatLines -> SwatLines
setSurlag newDouble lines =
  let start = take 19 lines in
  let end = drop 20 lines in
  case lines !! 19 of
    SwatInt d -> start ++ [SwatDouble ((mapEntry d) {doubleVal = newDouble})] ++ end
    SwatDouble d -> start ++ [SwatDouble (d {doubleVal = newDouble})] ++ end
    x -> lines
