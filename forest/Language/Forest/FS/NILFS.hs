{-# LANGUAGE ViewPatterns #-}

module Language.Forest.FS.NILFS where

import Language.Forest.FS.FSDelta
import Data.List
import Language.Forest.IO.Shell
import qualified Data.Map as Map
import System.FilePath
import Data.List.Split
import qualified Control.Concurrent.Map as CMap
import qualified Control.Concurrent.WeakMap as CWeakMap
import Language.Forest.FS.FSRep
import Data.IORef
import System.Mem.WeakKey
import System.Directory
import Control.Monad
import Data.Time.Clock
import Data.Time.LocalTime

--XXX: see what NILFS does if we add a new symlink!!
-- nilfs-diff between two given checkpoints, for a NILFS partition on @device@ mounted at @mountpoint@.
diffNILFS :: FilePath -> FilePath -> Int -> Int -> IO FSTreeDelta
diffNILFS mountpoint device cp1 cp2 = do
	
	diffLog <- sudoShellCommand $ "nilfs-diff " ++ device ++ " " ++ show cp1 ++ ".." ++ show cp2
	let diffLines = lines diffLog
	let td = foldl' (parseLine mountpoint) Map.empty diffLines
	return $ unfocusFSTreeDelta (relativeFSTreeDelta td) mountpoint

parseLine :: FilePath -> FSTreeDelta -> String -> FSTreeDelta
parseLine mountpoint td (modified -> Just path) = appendToFSTreeDelta (ChgAttrs path $ mountpoint </> makeRelative "/" path) td
parseLine mountpoint td (added -> Just to) = appendToFSTreeDelta (Add to $ mountpoint </> makeRelative "/" to) td
parseLine mountpoint td (removed -> Just from) = appendToFSTreeDelta (Rem from) td
parseLine mountpoint td (moved -> Just (from,to)) = appendToFSTreeDelta (Move from to $ mountpoint </> makeRelative "/" to) td
parseLine mountpoint td line = error $ "parseLine: failed to parse nilfs-diff log entry " ++ show line

modified ('M':' ':path) = Just path
modified _ = Nothing
added ('+':' ':to) = Just to
added _ = Nothing
removed ('-':' ':from) = Just from
removed _ = Nothing
moved ('R':' ':e) = case splitOn "->" e of
	[from,to] -> Just (from,to)
	otherwise -> Nothing
moved _ = Nothing

type Snapshot = Int

-- map from snapshot ids to the filepath where its root directory is mounted on the disk and a record of unevaluated thunks depending on the snapshot
-- a weak table to avoid keeping thunks alive
type LiveSnapshots = CWeakMap.WeakMap Snapshot (IORef MountPoint)

-- live snapshots,the root path being monitored, and its corresponding device
type NILFSData = ((FilePath,FilePath),FilePath)

type MountPoint = FilePath

-- mounts a NILFS snapshot, making sure that it is actually a NILFS snapshot (not a checkpoint)
mountSnapshotIO :: FilePath -> NILFSData -> Snapshot -> IO (IORef MountPoint)
mountSnapshotIO forestDir ((rootPath,rootFolder),device) snapshot = do
	let mountpoint = forestDir </> "Snapshots" </> show snapshot
	sudoShellCommand_ $ "chcp ss "++show snapshot
	createDirectoryIfMissing False mountpoint
	sudoShellCommand_ $ "mount.nilfs2 -r "++device++" "++mountpoint++" -o cp="++show snapshot
	ref <- newIORef (mountpoint)
	return ref

mountSnapshot :: FSRep fs => LiveSnapshots -> NILFSData -> Snapshot -> ForestM fs MountPoint
mountSnapshot liveSnapshots nilfs@((rootPath,rootFolder),device) snapshot = do
	forestDir <- getForestDirectory
	ref <- forestIO $ CWeakMap.lookupOrInsert liveSnapshots snapshot (mountSnapshotIO forestDir nilfs snapshot) (\v -> MkWeak $ mkWeakKey v)
	mountpoint <- forestIO $ readIORef ref
	return mountpoint

findRootDevice :: FilePath -> IO ((FilePath,FilePath),FilePath)
findRootDevice rootPathFolder = do
	rootDevice <- pathDevice rootPathFolder
	rootPath <- devicePath rootDevice
	let rootFolder = makeRelative rootPath rootPathFolder
	return ((rootPath,rootFolder),rootDevice)

pathInSnapshot :: FSRep fs => LiveSnapshots -> NILFSData -> FilePath -> Snapshot -> ForestM fs FilePath
pathInSnapshot liveSnapshots nilfs@((rootPath,rootFolder),device) path snapshot = do
	forestDir <- getForestDirectory
	ref <- forestIO $ CWeakMap.lookupOrInsert liveSnapshots snapshot (mountSnapshotIO forestDir nilfs snapshot) (\v -> MkWeak $ mkWeakKey v)
	mountpoint <- forestIO $ readIORef ref
	let result = mountpoint </> makeRelative rootPath path
	return result

newCheckpoint :: NILFSData -> IO Snapshot
newCheckpoint ((rootPath,rootFolder),device) = do
	str <- sudoShellCommand $ "mkcp " ++ device ++  " -p"
	return $ read str

readNILFSTime :: String -> IO UTCTime
readNILFSTime str = do
	timezone <- getCurrentTimeZone
	let localTime = Prelude.read str
	return $ localTimeToUTC timezone localTime

latestCheckpoint :: IO (Snapshot,UTCTime,Bool)
latestCheckpoint = do
	(snapStr:dateStr:timeStr:isSnapStr:_) <- liftM words $ runShellCommand "lscp -r | sed -n '2 p'"
	snapTime <- readNILFSTime (dateStr++' ':timeStr)
	let isSnap = case isSnapStr of { "ss" -> True; "cp" -> False }
	
	return (Prelude.read snapStr,snapTime,isSnap)

