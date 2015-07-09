{-# LANGUAGE TupleSections, ConstraintKinds, TypeFamilies, DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.IC.SwatOpt where

import Language.Pads.Padsc hiding (take, rest, head, numErrors)
import Language.Forest.IC hiding (writeFile)
import Examples.IC.Swat2
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


{- Should be added to Language/Forest/IC/Generic.hs -}
copyOrError :: (Forest fs, FTK fs rep) => rep -> rep -> String -> FTM fs ()
copyOrError orig copy e = copyOrElse orig copy () (Prelude.const $ error e)



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

data Deviation = Deviation Double
|]


[txforest|
  data Preamble_f  = File Preamble
  data BSN_f       = File SwatFile
  data PCP_f       = File PCP
  data TMP_f       = File TMP

  data Flow_f      = File Flow                  -- Contains measured flow data.
  data RCH_f       = File RCH                   -- Contains modeled flow information.
  data Deviation_f = File Deviation     -- Contains deviation between modeled and measured flows.  
                                          -- XXX: Shouldn't these be in the SWAT directory?
  data Swat_d = Directory 
     { cio is "file.cio" :: Preamble_f      -- master watershed file (options, inputs and output specifications)
     , basin is <| getBasinPath cio |> :: BSN_f          -- physical information about basin
     , pcps  is [f :: PCP_f | f <- matches (GL "*.pcp") ]     -- daily precipitation data
     , tmps  is [f :: TMP_f | f <- matches (GL "*.tmp") ]     -- daily temperature data
     }
|]


{- 
 adaptor functions to make the code in the paper smoother.
-}
getBasinPath cio = getCioFile basinFile cio
getCioFile :: (SwatLines -> FilePath) -> Preamble_f TxVarFS -> ReadOnlyFTM TxVarFS FilePath
getCioFile f cio = liftM (f . swatLines) $ readData cio



{-
  XXXX: We should be able to replace 
         FPreamble with File Preamble,
         FPCP with File PCP
         FTMP with File TMP
         FBSN with File SwatLines, etc.
     but at the moment if we do so, we get tons of type errors.
-}

type Path = String
{- Constants -}

rootDir = "/home/vagrant/forest/iforest"

swatExe  = rootDir </> "Examples/IC/Swat/rswat2012.exe"
swatDataDir = rootDir </> "Examples/IC/bolo_arriba"
measuredFlowFile    = rootDir </> "Examples/IC/Swat/flow.dat"

deviationFile = "/tmp/deviationt"
deviationMax = Deviation 5000000

numThreads = 2      -- 2 is number of threads, starting small
numIterations = 6   -- 6 is the number of iterations per thread, starting small



escoRange = (0.1, 1.0)
surlagRange = (0.0, 15.0)


{-  
  Initialize the deviation to be the maximum possible, 
  then start parallel search with numThreads.
-}
main :: IO ()
main = do
  createDirectoryIfMissing True "/tmp/Swat"
  setDeviation deviationMax deviationFile     -- XXX: Why doesn't this have to be done atomically?
  replicateM_ numThreads $ forkIO $ optSWAT 

{-
  Get thread's temporary directory, assumed to be unique and not accessible via other processes,
  whether within forest or not?
  Run SWAT numIteration times.
-}
optSWAT :: IO ()
optSWAT = do
  threadWorkingDir <- getThreadTempDirectory
  replicateM_ numIterations $ iterateSWAT threadWorkingDir

{-
  Each iteration first atomically copies the SWAT data into a thread-local working directory.
  It then randomly perturbs relevant basin parameters in the thread-copy and runs black-box SWAT 
  executable to calculate effects of modified parameters.  
  It calculates the deviation of the flow in the resulting model from the measured flow
  and atomically updates the SWAT data if the newDeviation is the lowest observed so far. 
-}
iterateSWAT :: Path -> IO ()
iterateSWAT workingDir = do
  atomically (copySWATData workingDir)
  newBsn <- modifyBsnParams workingDir
  runSWAT workingDir
  newDeviation <- getNewDeviation workingDir
  atomically (updateDeviation newBsn newDeviation)


{- Copy SWAT data from main location (swatDataDir) to argument directory 
   Report an error if the copy goes awry.  -}
copySWATData :: FilePath -> FTM TxVarFS ()
copySWATData tmpDir = do
    (orig :: Universal_d TxVarFS) <- new () swatDataDir
    (copy :: Universal_d TxVarFS) <- new () tmpDir
    copyOrError orig copy "Failed to copy swat data to temporary directory."  


{- Randomly modify basin parameters within given tolerances. -}
{- All these operations are taking place in the thread-local directory, 
   and so we read the files with PADS rather than transactional forest.
   XXX: something seems wrong here at the design level.
        We should be using the forest description to navigate the file
        system, not manually manipulating hard-coded paths.
-}
modifyBsnParams :: Path -> IO SwatLines
modifyBsnParams workingDir = do 
  let workingBsnFile = workingDir </> "basins.bsn"
  ((bsnRep, bsnMd) :: (SwatFile, SwatFile_md)) <- parseFile workingBsnFile
  let bsnValues = swatValues bsnRep
  newEsco   <- newValue escoRange   $ getEsco   bsnValues
  newSurlag <- newValue surlagRange $ getSurlag bsnValues
  let newBsn = setSurlag newSurlag $ setEsco newEsco bsnValues 
  printFile workingBsnFile $ (SwatFile {swatValues=newBsn}, bsnMd)
  return newBsn

{- Calculuate the deviation of the flows predicated by the working model 
   from the measured flows -}
{-
   XXX: it seems wrong here that we are manipulating the path to the rch
   file by hand rather than using forest to navigate.
-}
getNewDeviation :: Path -> IO Double
getNewDeviation workingDir = do
  -- Read the measured flow.
  ((flowRep, flowMd) :: (Flow, Flow_md)) <- parseFile measuredFlowFile
  let measuredFlow = flowAmount . head . flowInfo $ flowRep
  -- Read the calculated flow
  let workingRchFile = workingDir </> "output.rch"
  ((rchRep, rchMd) :: (RCH, RCH_md)) <- parseFile workingRchFile 
  let estimatedFlows = trim . rchInFlow . head . rchInfo $ rchRep
  let (estimatedFlow :: Double) = fst . head $ readFloat estimatedFlows
  -- Calculate the deviation
  let newDeviation = calcDeviation measuredFlow estimatedFlow
  return newDeviation

{- Compute the deviation between measured and estimated flows -}
calcDeviation :: Double -> Double -> Double
calcDeviation measuredFlow estimatedFlow = 
  abs $ (measuredFlow / 86400.0) - estimatedFlow


{- Invoke SWAT black-box solver on data in path directory -}
runSWAT :: FilePath -> IO ()
runSWAT path = do
  setCurrentDirectory path
  callProcess swatExe []

{-
  Read the current best deviation from the global file.
  If the newDeviation is smaller, then write the basin data with the updated
  parametrs to the SWAT data directory.
-}
{-
  XXX: pads returns (rep, md) but iforest is returning (md, rep)
       Is there a reason we can't be consistent?
-}
updateDeviation :: SwatLines -> Double -> FTM TxVarFS ()
updateDeviation newBsn newDeviation =  do
    (devInfo :: Deviation_f TxVarFS) <- new () deviationFile
    (devMd, Deviation currentDeviation) <- read devInfo
    guard (currentDeviation > newDeviation)
    (swatRep :: Swat_d TxVarFS) <- new () $ swatDataDir
    (dirMd, dir) <- read swatRep
    (bsnMd, _  ) <- read $ basin dir
    writeOrError devInfo     (devMd, Deviation newDeviation) "Failed to write new deviation."
    writeOrError (basin dir) (bsnMd, SwatFile  newBsn)       "Failed to update SWAT Basin data."






{- 
    Helper functions.
-} 


{- Create thread-specific temporary directory. -}
getThreadTempDirectory :: IO FilePath
getThreadTempDirectory = do
  threadID <- myThreadId
  createTempDirectory "/tmp/Swat/" $ (drop 9 $ show threadID)


{-
  Get a new random value offset from current.
  It looks like the random value is supposed to be between min and max
  but I'm not convinced the code maintains this invariant.
-}
newValue :: (Floating a, Ord a, Random a) => (a, a) -> a -> IO a
newValue (min, max) current = do
  (randValue :: a) <- randomRIO (-1, 1)
  let new = current + randValue * (max - min) / 3 -- this is arbitrary
  return (case compare min new of
  	LT | new < max -> new
  	   | new >= max -> max
  	_ -> min)




{-
   Functions for getting and setting values in files with Pads descriptions.
-}

{- setDeviation sets the current deviation between
   the predicted outflow from the basin and the measured outflow.
   We will use this function to set the initial deviation to
   an artificially high value, from which we will start our search
   for the minimum value. -}
{- XXX: 
   Why do we parse the devFile and then ignore the parsed data?
   We can create a default clean meta-data structure if that is the goal
   and avoid parsing the file.
-}
setDeviation ::  Deviation -> FilePath -> IO ()
setDeviation dev devFile = do
  ((devRep, devMd) :: (Deviation, Deviation_md)) <- parseFile devFile
  printFile devFile (dev, devMd)


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


{- OLD -}

updateDeviation' :: SwatLines -> Double -> FTM TxVarFS ()
updateDeviation' newBsn newDeviation = 
  (( do
    (devInfo :: Deviation_f TxVarFS) <- new () deviationFile
    (devMd, Deviation currentDeviation) <- read devInfo
    if currentDeviation > newDeviation then do
        (swatRep :: Swat_d TxVarFS) <- new () $ swatDataDir
        (dirMd, dir) <- read swatRep
        (bsnMd, _  ) <- read $ basin dir
        writeOrError devInfo   (devMd, Deviation newDeviation) "Failed to write new deviation."
        writeOrError (basin dir) (bsnMd, SwatFile  newBsn)       "Failed to update SWAT Basin data."
        return ()
    else do
        return ()) :: FTM TxVarFS ())

copySWATData' :: FilePath -> IO ()
copySWATData' tmpDir = do
  err <- atomically $ (do
    (orig :: Universal_d TxVarFS) <- new () swatDataDir
    (copy :: Universal_d TxVarFS) <- new () tmpDir
    copyOrElse orig copy "" $ return . show :: FTM TxVarFS String)
  putStrLn err
