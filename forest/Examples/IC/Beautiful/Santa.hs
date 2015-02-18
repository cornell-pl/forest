{-# LANGUAGE ConstraintKinds, TypeFamilies, DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.IC.Beautiful.Santa where

import System.Random

import Language.Pads.Padsc hiding (numErrors)
import Data.Maybe
import Data.IORef
import Language.Haskell.TH.Syntax

import System.IO.Unsafe (unsafePerformIO)
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
import Control.Monad.Incremental hiding (read,new)
import Prelude hiding (read)
import Language.Forest.IC hiding (writeFile)

[pads|
       data Max = Max Int
|]

$( derive makeMData ''Max )
$( derive makeMData ''Max_imd )
$( derive makeDeepTypeable ''Max )
$( derive makeDeepTypeable ''Max_imd )

[iforest|
          type MaxFile = File Max
          type Inactive = Directory {
              reindeerI  is [ r :: TextFile | r <- matches (GL "Rein*") ]
              , elvesI  is [ e :: TextFile | e <- matches (GL "Elf*") ]} 
          type Gate = Directory {
              reindeerGa  is [ r :: TextFile | r <- matches (GL "Rein*") ]
              , elvesGa  is [ e :: TextFile | e <- matches (GL "Elf*") ]
              , maxGa is "max" :: MaxFile
              } 
          type Group = Directory {
              gateIn  is "GateIn" :: Gate
              , gateOut  is "GateOut" :: Gate
              , memGr  is [ r :: TextFile | r <- matches (GL "[Rein|Elf]*") ]
              , maxGr is "max" :: MaxFile
              } 
          type Main = Directory {
                groups  is [ g :: Group    | g <- matches (GL "Group*"), (kind  g_att == DirectoryK) ]
              , inactive is "inactive" :: Inactive
              } 
|]
{-
Main
- GroupElves (group)
-- 2 gates?
- GroupReindeer (group)
-- 2 gates?
- !inactive (this is where inactive reindeers and elves go)
-}

type GroupID = FilePath
type GateID = FilePath

data GroupStore = GroupStore (GroupID,GateID,GateID)

--Change this to the forest directory to make the example work for you!
relDir = "."

santaDir = relDir </> "Examples/IC/Beautiful/Santa"

inactiveDir = santaDir </> "inactive"

reindeerGroupId = santaDir </> "GroupReindeer"
reindeerInGate = reindeerGroupId </> "GateIn"
reindeerOutGate = reindeerGroupId </> "GateOut"

reindeerGroup = GroupStore (reindeerGroupId,reindeerInGate,reindeerOutGate)

joinGroup :: GroupStore -> String -> IO (Maybe (GateID,GateID))
joinGroup store name = do
  let GroupStore (grId,gIn,gOut) = store
  (status, win) <- atomically $ do
    (group :: Group TxVarFS) <- new () grId
    (meta, contents) <- read group
    let max =  maxGr contents
    test <- read max
    status <- showInc max
    return (status, False)
    -- let lst = memGr contents
    -- check (length lst < max)
    -- (rep :: TextFile TxVarFS) <- new () (inactiveDir </> name)
    -- err <- validate rep
    -- case errorMsg err of
    --   Just (MissingFile _) -> return ((name ++ " is not currently inactive."), False)
    --   _ -> do
    --     let contents = contents {memGr = lst ++ [(name,rep)]}
    --     -- Does not currently work
    --     status <- writeOrElse group (meta, contents) (name ++ " joined a group!") (return . show)
    --     delete rep
    --     return (status, True)
  putStrLn status
  if win then return $ Just (gIn,gOut) else return Nothing

joinGroupTest = joinGroup reindeerGroup "Rein1"

check :: Bool -> FTM TxVarFS ()
check True = return ()
check False = retry

forever :: IO () -> IO ()
forever act = do
 act
 forever act

randomDelay :: IO ()
randomDelay = do
 waitTime <- getStdRandom (randomR (1000000, 10000000))
 threadDelay waitTime
  
-- clear :: FSRep fs => ForestM fs ()
-- clear = error "Not Implemented" -- Delete everything in Santa somehow?

-- newGate :: GateID -> Int -> IO ()
-- newGate id n = do
--   status <- atomically $ do
--     (rep :: Gate TxVarFS) <- new () id

-- passGate :: FSRep fs => Int -> ForestM fs ()
-- passGate n = error "Not Implemented"

-- operateGate :: FSRep fs => Int -> ForestM fs ()
-- operateGate n = error "Not Implemented"

-- newGroup :: FSRep fs => String -> String -> Int -> ForestM fs ()
-- newGroup dest name n = error "Not Implemented"

-- initialize :: FSRep fs => ForestM fs ()
-- initialize = do
--   newGroup (santaDir ++ "/GroupReindeer")
--   newGroup (santaDir ++ "/GroupElves")

-- Helper Functions
                                 
-- spotsLeft :: FSRep fs => String -> ForestM fs Bool
-- spotsLeft dir = do
--   (rep,md) :: (Gate, Gate_md) <- load () dir
--   let (Gate_inner r e (Max m)) = rep
--   forestIO $ print ((read m) - ((length r) + (length e)))
--   return ((read m) > ((length r) + (length e)))

-- runSpots dir = runForest PureFSForestCfg $ spotsLeft dir
