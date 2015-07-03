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

rootDir = "."
optDir = rootDir </> "Examples/IC/bolo_arriba"

maxFile = "/tmp/maxOpt"
flowFile = rootDir </> "Examples/IC/Swat/flow.dat"
swatLocation = "/home/vagrant/R/i686-pc-linux-gnu-library/3.2/SWATmodel/libs/i686/rswat2012.exe"

getNewDirectory :: IO FilePath
getNewDirectory = do
  threadID <- myThreadId
  createTempDirectory "/tmp/Swat/" $ (drop 9 $ show threadID)

runSwat :: FilePath -> IO ()
runSwat path = do
  setCurrentDirectory path
  callProcess swatLocation []

writeCurrent :: FilePath -> IO ()
writeCurrent maxFile = do
  ((max_rep, max_md) :: (Max, Max_md)) <- parseFile maxFile
  printFile maxFile (Max 500000, max_md)

optSwat :: Int -> IO ()
optSwat n = replicateM_ n optSwatIter -- 6 is number of iterations

optSwatIter :: IO ()
optSwatIter = do
  tmpDir <- getNewDirectory
  err <- atomically $ ((do
    (original :: Universal_d TxVarFS) <- new () optDir
    (copy :: Universal_d TxVarFS) <- new () $ tmpDir
    copyOrElse original copy ("") $ return . show) :: FTM TxVarFS String)
  putStrLn err
  -- edit parameters in random fashion
  _ <- runSwat tmpDir

  -- Not worried about transactions here, so I'm using the pads interface
  -- instead of the forest one to avoid transactional overhead
  ((flowRep, flowMd) :: (Flow, Flow_md)) <- parseFile flowFile
  let firstFlowValue = flowAmount . head . flowInfo $ flowRep
  ((rchRep, rchMd) :: (RCH, RCH_md)) <- parseFile $ tmpDir </> "output.rch"
  let calcFlow = trim . rchInFlow . head . rchInfo $ rchRep
  let (calcFlowValue :: Double) = fst . head $ readFloat calcFlow
  _ <- case firstFlowValue > calcFlowValue of
    True -> atomically $ ((do
      return ()) :: FTM TxVarFS ()) -- write files here
    False -> return ()
  return ()

main :: IO ()
main = do
  _ <- writeCurrent maxFile
  replicateM_ 2 $ forkIO $ optSwat 6 -- 2 is number of threads, starting small

--getVar1 :: SwatLines -> Double
--getVar1 lines =
--  case lines !! 3 of
--    SwatDouble d -> numVal d
--    _ -> 0

--setVar1 :: SwatLines -> Double -> SwatLines
--setVar1 lines newDouble =
--  let start = take 3 lines in
--  let end = drop 4 lines in
--  case lines !! 3 of
--    SwatDouble d -> start ++ [SwatDouble (d {numVal = newDouble})] ++ end
--    x -> lines

--checkOne :: OptFunc -> Double -> Double -> Double -> IO ()
--checkOne f x y z = do
--  status <- atomically $ do
--    (maxInfo :: MaxFile TxVarFS) <- new () maxFile
--    ((max_fmd, max_md), Max m) <- read maxInfo
--    let candidate = f x y z
--    case candidate > m of
--      True -> do
--        (swatInfo :: Swat_d TxVarFS) <- new () optDir
--        (main_fmd, dir) <- read swatInfo
--        ((file1_fmd, file1_md), SwatFile bsnVals) <- read $ bsn dir
--        let updated = setVar3 (setVar2 (setVar1 bsnVals x) y) z
--        _ <- writeOrElse (bsn dir) ((file1_fmd, file1_md), SwatFile updated) ("") (return . show)
--        _ <- writeOrElse (maxInfo) ((max_fmd, max_md), Max candidate) ("") (return . show)
--        return "updated"
--      False -> return "not updated"
--  putStrLn status

--writeCurrent :: OptFunc -> IO ()
--writeCurrent f = do
--  _ <- atomically $ do
--    (rep :: MaxFile TxVarFS) <- new () maxFile
--    (swatRep :: Swat_d TxVarFS) <- new () optDir
--    (main_fmd, dir) <- read swatRep
--    ((file1_fmd, file1_md), SwatFile bsnVals) <- read $ bsn dir
--    let v1 = getVar1 bsnVals
--    let v2 = getVar2 bsnVals
--    let v3 = getVar3 bsnVals
--    ((max_fmd, max_md), Max m) <- read rep
--    _ <- writeOrElse (rep) ((max_fmd, max_md), Max (f v1 v2 v3)) ("") (return . show)
--    return ()
--  return ()

--getBest :: OptFunc -> Interval -> Interval -> Interval -> IO ()
--getBest f i1 i2 i3 =
--  let ((l1, h1), (l2, h2), (l3, h3)) = (i1, i2, i3) in
--  let trips = [(i, j, k) | i <- [l1..h1], j <- [l2..h2], k <- [l3..h3]] in
--  do
--    _ <- writeCurrent f
--    _ <- mapM_ (\(x,y,z) -> forkIO $ checkOne f x y z) trips
--    return ()
