{-# LANGUAGE TemplateHaskell,  ConstraintKinds, UndecidableInstances, TupleSections, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, DataKinds, TypeFamilies, Rank2Types, GADTs, ViewPatterns, DeriveDataTypeable, ScopedTypeVariables #-}

-- Regular filesystem with optimistic concurrency support for transactions, with purely functional data structures

module Language.Forest.Pure.FS.TxFS where

import Control.Monad.Catch
import Control.Concurrent
import System.Cmd
import Data.Global.TH as TH

import Language.Forest.FS.FSRep
import Control.Applicative
import System.IO.Unsafe
import Control.Monad

import Data.Maybe
import Data.List
import Data.WithClass.MData
import Data.Typeable
import System.Directory
import Data.List as List
import Data.IORef
import System.FilePath.Posix
import Language.Forest.IO.Utils
import Language.Forest.Pure.MetaData
import Control.Concurrent.Lock (Lock(..))
import qualified Control.Concurrent.Lock as Lock
import Control.Concurrent.MVar
import Language.Forest.IO.Shell
import Language.Forest.IO.Utils
import Language.Forest.Pure.Generic
import Data.IORef
import Control.Monad.Ref
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import Control.Monad.State (State(..),StateT(..),MonadState(..))
import Language.Forest.FS.FSDelta
import Control.Monad.Trans
import qualified Control.Monad.State as State
import Language.Forest.Manifest
import Data.Time.Clock
import Control.Monad.Catch as Catch
import Safe
import Data.Global.TH

{-
TODO:
- Figure out how to get writes to work with relative paths
- Currently overly pessimistic. Any files with the same names will be assumed to be the same.
- Make it look prettier
- Make it watch files?
-- Easy without Mvars
-- NOW WITH MVARS?!
-}

type TxFSReads = Set FilePath
type TxFSWrites = Set FilePath

-- a list of the starting times of running transactions sorted from newest to oldest
TH.declareMVar "runningTransactions"  [t| [UTCTime] |] [e| [] |]

-- a map with commit times of committed transactions and their performed changes
TH.declareMVar "doneTransactions"  [t| (Map UTCTime TxFSWrites) |] [e| Map.empty |]

startTxFSTransaction :: ForestM TxFS UTCTime
startTxFSTransaction = do
  TxFSForestM $ State.modify  $ \_ -> ((Set.empty,Map.empty),Set.empty)
  forestIO $ modifyMVar runningTransactions (\xs -> getCurrentTime >>= \t -> return (t:xs,t))

-- validates and commits a transaction as a single atomic operation
validateAndCommitTxFSTransaction :: UTCTime -> TxFSChangesFlat -> FSTreeDelta -> IO Bool
validateAndCommitTxFSTransaction starttime chgs@(reads,writes) td = Lock.with noFSLock $ do
	-- gets the transactions that commited after the current transaction's start time
	finished <- liftM (map snd . Map.toAscList . Map.filterWithKey (\k v -> k > starttime)) $ readMVar doneTransactions
	let check = checkTxFSTransaction chgs finished
	if check
		then finishTransaction starttime writes td
		else modifyMVar_ runningTransactions (return . List.delete starttime)
	return check

checkTxFSTransaction :: TxFSChangesFlat -> [TxFSWrites] -> Bool
checkTxFSTransaction chg [] = True
checkTxFSTransaction chg (d:ds) = checkTxFSTransaction' d chg && checkTxFSTransaction chg ds

-- checks if two logs of modifications are consistent
checkTxFSTransaction' :: TxFSWrites -> TxFSChangesFlat -> Bool
checkTxFSTransaction' (writesBefore) (readsAfter,writesAfter) = do
	readsAfter `Set.intersection` writesBefore == Set.empty -- no write-read conflicts
--	&& writesAfter `Set.intersection` writesBefore == Set.empty -- no write-write conflicts

-- adds a new commited transaction, and deletes done transactions that finished before the start of the earliest running transaction
finishTransaction :: UTCTime -> TxFSWrites -> FSTreeDelta -> IO ()
finishTransaction starttime writes td = do
	mb <- modifyMVar runningTransactions (\xs -> return (List.delete starttime xs,lastMay xs))
        commitPhase td ""
	case mb of
		Just oldt -> modifyMVar_ doneTransactions (\m -> getCurrentTime >>= \now -> return $ Map.filterWithKey (\t _ -> t > oldt) $ Map.insert now writes m)
		Nothing -> modifyMVar_ doneTransactions (\m -> getCurrentTime >>= \now -> return $ Map.insert now writes m)

commitPhase :: FSTreeDelta -> FilePath -> IO ()
commitPhase td cpath = Map.foldrWithKey
    (\ npath node m -> case node of
		FSTreeNew dmap mf od _ -> do --What is mf for?
 --         print dmap
   --       print cpath
			if ((cpath ++ npath) == od) || ((cpath ++ npath) == "/" ++ od)
				then commitPhase dmap (cpath ++ npath ++ "/")
				else do
					code <- runShellCommand_ ("cp -r " ++ od ++ " " ++ (cpath ++ npath))
					print code
					commitPhase dmap (cpath ++ npath ++ "/")
					m
		FSTreeChg dmap od    -> error "Not Implemented"
		FSTreeNop dmap       -> commitPhase dmap (cpath ++ npath ++ "/")
		FSTreeRem            -> do
			code <- removePath (cpath ++ npath)
			print code
    ) (return ()) td


recCheck :: TxFSReads -> [TxFSWrites] -> Bool
recCheck reads [] = True
recCheck reads (d:ds) = (reads `Set.intersection` d == Set.empty) && recCheck reads ds

checkForRestart :: UTCTime -> TxFSReads -> IO (Bool, UTCTime) 
checkForRestart time reads =
  do
  {
    if reads == Set.empty
    -- If reads are empty, we just wait a bit and then try again. It is unclear what good behavior is.
    -- Looping forever seems like a poor choice in general, on the other hand, I can't imagine decent ways in which they
    -- would have this result.
    then do
      threadDelay 9000000
      return (False, time)
    else do
        newtime <- getCurrentTime
        writeList <- liftM (map snd . Map.toAscList . Map.filterWithKey (\k v -> k > time)) $ readMVar doneTransactions
{-        print ("Reads: " ++ (show reads))
        print ("Writes: " ++ (show writeList))
        let b = recCheck reads writeList
        print b
-}
        return(recCheck reads writeList, newtime)
  }

-- Helper function for splitting on strings
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

                             
{-# NOINLINE noFSLock #-}
noFSLock :: Lock
noFSLock = unsafePerformIO $ Lock.new

instance TransactionalPureForest TxFS where
	atomically = atomicallyTxFS
	retry = retryTxFS
	orElse = orElseTxFS
	throw = throwTxFS
	catch = catchTxFS

-- we remember a set of read files
-- we use an absolute tree delta (starting at the filesystem's root) to keep track of the filesystem modifications performed by the transaction, to be committed at the end
-- we keep a set of temporary files/directories used  by the transaction, that are purged after commit
type TxFSLog = (TxFSChanges,Set FilePath)


-- changes just as a set of reads and a set of writes
type TxFSChangesFlat = (TxFSReads,TxFSWrites)
-- changes as a set of reads and a modification tree
type TxFSChanges = (TxFSReads,FSTreeDelta)
data TxExcep = TxExcep TxFSLog deriving (Show, Typeable)

instance Exception TxExcep

getTxFSChanges ::  ForestM TxFS TxFSChanges
getTxFSChanges = liftM fst $ TxFSForestM State.get

putTxFSRead :: FilePath -> ForestM TxFS ()
putTxFSRead path = TxFSForestM $ State.modify $ \((paths,y),z) -> ((Set.insert path paths,y),z)

getTxFSTmp ::  ForestM TxFS (Set FilePath)
getTxFSTmp = liftM snd $ TxFSForestM State.get

putTxFSTmp :: FilePath -> ForestM TxFS ()
putTxFSTmp tmp = TxFSForestM $ State.modify $ \(x,tmps) -> (x,Set.insert tmp tmps)

getTxFSTreeDeltas ::  ForestM TxFS FSTreeDelta
getTxFSTreeDeltas = TxFSForestM $ State.get >>= \((_,td),_) -> return td

modifyTxFSTreeDeltas :: (FSTreeDelta -> FSTreeDelta) -> ForestM TxFS ()
modifyTxFSTreeDeltas f = TxFSForestM $ State.modify $ \((x,td),z) -> ((x,f td),z)

atomicallyTxFS :: ForestM TxFS a -> IO a
atomicallyTxFS t =
  let keepTrying fail time reads = do
        {
          if fail
          then do
            {
              threadDelay 1000000;
              (failure,newtime) <- checkForRestart time reads;
              keepTrying failure newtime reads
            }
          else return ()
        }
  in
  let tryIt = do
        {
          time <- startTxFSTransaction;
          result <- TxFSForestM $ Catch.try (runTxFSForestM t);
          case result of
            Right x -> do
              {
                (reads,td) <- getTxFSChanges;
--                forestIO $ print td;
                let writes = fsTreeDeltaWrites "/" td in do
                  {
                    success <- forestIO $ validateAndCommitTxFSTransaction time (reads,writes) td;
                    getTxFSTmp >>= return . Set.foldr (\path m -> removePath path >> m) (return ()); -- remove all temporary data used by this run
                    if success then return x else tryIt
                  }
              }
            Left (TxExcep((r,_),_)) -> do
              -- Retry has been called!
              -- Check every 1s if global transaction log has a new entry written to something
              -- we tried to read.
              -- Eventually change this to watch files both for interoperability with outside transactions and for less arbitrary time measurement
              let newr = Set.map (last . (wordsWhen (\ c -> c=='\\' || c == '/'))) r
              forestIO $ modifyMVar_ runningTransactions (return . List.delete time)
              getTxFSTmp >>= return . Set.foldr (\path m -> removePath path >> m) (return ())
              forestIO $ keepTrying True time newr
              tryIt
        }
   in
    runForest TxFSForestCfg tryIt
    
-- Assume always called in atomically
-- Throw exception, catch in atomically or orElse.        
retryTxFS ::  ForestM TxFS a
retryTxFS = do
  x <- TxFSForestM State.get
  TxFSForestM $ throwM (TxExcep x)

-- Assume always called in atomically
-- Needs to be able to 'catch' retries somehow
-- Do a1
-- If a1 retries (catch)
--    Do a2
orElseTxFS :: ForestM TxFS a -> ForestM TxFS a -> ForestM TxFS a 
orElseTxFS a1 a2 = TxFSForestM $ Catch.catch (runTxFSForestM a1) (\ (ex :: TxExcep) -> runTxFSForestM a2)


catchTxFS :: Exception e => FTM fs a -> (e -> FTM fs a) -> FTM fs a
catchTxFS = error "Not Implemented"

throwTxFS :: Exception e => e -> FTM fs a
throwTxFS = error "Not Implemented"

-- filesystems are always different
instance Eq (FSTree TxFS) where
	t1 == t2 = False

instance FSRep TxFS where
	
	unsafeInterleaveForestM (TxFSForestM m) = TxFSForestM $ do
		st <- State.get
		(a,st') <- lift $ unsafeInterleaveIO $ State.runStateT m st
		State.put st'
		return a
	
	newtype ForestM TxFS a = TxFSForestM { runTxFSForestM :: StateT TxFSLog IO a } deriving (Functor,Applicative,Monad)
	
	data ForestCfg TxFS = TxFSForestCfg
	
	runForest _ (TxFSForestM m) = do
		mountAVFS
		x <- State.evalStateT m ((Set.empty,Map.empty),Set.empty)
		unmountAVFS
		return x
	
	forestIO = TxFSForestM . lift
	
	-- | returns the forest directory used to store auxiliary data
	getForestDirectory = liftM (</> "Forest") $ forestIO getTemporaryDirectory
	
	-- there is no notion of filesystem versions, but we still need to distinguish AVFS mode from normal mode
	data FSTree TxFS = TxFSTree | VirtualTxFSTree deriving Show
	
	-- log the modifications
	deletePath path = modifyTxFSTreeDeltas $ appendToFSTreeDelta (Rem path)
	writeFile path ondisk = modifyTxFSTreeDeltas $ appendToFSTreeDelta (Add path ondisk)
	writeDir path = modifyTxFSTreeDeltas $ appendToFSTreeDelta (Add path path) --XXX:revise this...
	writeLink path ondisk = modifyTxFSTreeDeltas $ appendToFSTreeDelta (Add path ondisk)
	writePathMD path ondisk = modifyTxFSTreeDeltas $ appendToFSTreeDelta (ChgAttrs path ondisk)
	
	-- registers a new temporary path
	tempPath = forestIO getTempPath >>= \path -> putTxFSTmp path >> return path
	-- change into AVFS mode
	virtualTree _ = return VirtualTxFSTree
	latestTree = return TxFSTree
	
	-- reads from the FS or from the modification log
	pathInTree path TxFSTree = do
		(reads,td) <- getTxFSChanges
                currDir <- forestIO $ getCurrentDirectory
                let newpath =
                      if head path == '.'
                      then currDir ++ tail path
                      else path
		let td' = focusFSTreeDeltaByRelativePathMay td newpath
		case onDiskWriteMay td' of
			Nothing -> do
                          putTxFSRead newpath
                          return newpath
			Just ondisk -> do
                          return ondisk
	pathInTree path VirtualTxFSTree = do
		ondisk <- pathInTree path TxFSTree
		home <- forestIO $ getHomeDirectory
		let result = home </> ".avfs" </> makeRelative "/" ondisk
		return result
		
	stepPathInTree _ path rel = return $ path </> rel
	canonalizePathInTree path _ = forestIO $ canonalizePath path









