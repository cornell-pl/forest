{-# LANGUAGE Rank2Types, GADTs, ViewPatterns, TupleSections #-}

module Language.Forest.IC.FS.FSDelta where

import Control.Monad
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import System.FilePath.Posix
import Language.Forest.IO.Utils
--import Language.Forest.FS.FSRep
import Language.Forest.IO.Shell
import System.Directory
import Safe

type OnDisk = FilePath

type FSTreeDeltaNodeMay = Maybe FSTreeDeltaNode

-- filesystem deltas store the modified file content
data FSTreeDeltaNode = FSTreeNew FSTreeDelta MoveFrom OnDisk -- a new directory or file
					 | FSTreeChg FSTreeDelta OnDisk -- a directory or file whose metadata has changed
					 | FSTreeNop FSTreeDelta -- a directory or file that has not changed
					 | FSTreeRem
	deriving (Show,Eq,Ord)

type MoveFrom = Maybe FilePath -- move origin

type FSTreeDelta = Map FileName FSTreeDeltaNode

data FSDelta = Add FilePath OnDisk
			 | Rem FilePath
			 | Move FilePath FilePath OnDisk
			 | ChgAttrs FilePath OnDisk
	deriving (Show,Eq,Ord)

-- * Operations on filesystem deltas

commitFSTreeDelta :: FilePath -> FSTreeDelta -> IO (Set FilePath)
commitFSTreeDelta root = Map.foldrWithKey aux (return Set.empty) where
	aux file td m = do
		m1 <- commitFSTreeDeltaNode (root </> file) td
		m2 <- m
		return $ m1 `Set.union` m2

commitFSTreeDeltaNode :: FilePath -> FSTreeDeltaNode -> IO (Set FilePath)
commitFSTreeDeltaNode root (FSTreeNew td _ ondisk) = do
	runShellCommand_ $ "cp -r " ++ show ondisk ++ " " ++ show root
	xs <- commitFSTreeDelta root td
	return $ Set.insert root xs
commitFSTreeDeltaNode root (FSTreeChg td ondisk) = do
	copyPermissions ondisk root
	xs <- commitFSTreeDelta root td
	return $ Set.insert root xs
commitFSTreeDeltaNode root (FSTreeNop td) = do
	commitFSTreeDelta root td
commitFSTreeDeltaNode root FSTreeRem = do
	runShellCommand_ $ "rm -r " ++ show root
	return $ Set.singleton root

onDiskWriteMay :: FSTreeDeltaNodeMay -> Maybe OnDisk
onDiskWriteMay = maybe Nothing onDiskWrite

onDiskWrite :: FSTreeDeltaNode -> Maybe OnDisk
onDiskWrite (FSTreeNew _ _ ondisk) = Just ondisk
onDiskWrite (FSTreeChg _ ondisk) = Just ondisk
onDiskWrite (FSTreeNop _) = Nothing
onDiskWrite FSTreeRem = Nothing

fsTreeDeltaWrites :: FilePath -> FSTreeDelta -> Set FilePath
fsTreeDeltaWrites root td = Map.foldrWithKey aux Set.empty td where
	aux file td xs = fsTreeDeltaNodeWrites (root </> file) td `Set.union` xs

fsTreeDeltaNodeWrites :: FilePath -> FSTreeDeltaNode -> Set FilePath
fsTreeDeltaNodeWrites root (FSTreeNew td _ _) = Set.insert root (fsTreeDeltaWrites root td)
fsTreeDeltaNodeWrites root (FSTreeChg td _) = Set.insert root (fsTreeDeltaWrites root td)
fsTreeDeltaNodeWrites root (FSTreeNop td) = fsTreeDeltaWrites root td
fsTreeDeltaNodeWrites root FSTreeRem = Set.singleton root

childrenFSTreeDeltaNode = fromJustNote "no FSTreeDelta children" . childrenFSTreeDeltaNodeMay

childrenFSTreeDeltaNodeMay :: FSTreeDeltaNode -> Maybe FSTreeDelta
childrenFSTreeDeltaNodeMay (FSTreeNew tds _ _) = Just tds
childrenFSTreeDeltaNodeMay (FSTreeChg tds _) = Just tds
childrenFSTreeDeltaNodeMay (FSTreeNop tds) = Just tds
childrenFSTreeDeltaNodeMay _ = Nothing

emptyFSTreeDelta :: FSTreeDelta
emptyFSTreeDelta = Map.empty

isEmptyFSTreeDelta :: FSTreeDelta -> Bool
isEmptyFSTreeDelta = Map.null

isEmptyFSTreeDeltaNodeMay :: FSTreeDeltaNodeMay -> Bool
isEmptyFSTreeDeltaNodeMay = isNothing

isEmptyTopFSTreeDeltaNodeMay :: FSTreeDeltaNodeMay -> Bool
isEmptyTopFSTreeDeltaNodeMay Nothing = True
isEmptyTopFSTreeDeltaNodeMay _ = False

-- ** Compression

compressFSDeltas :: [FSDelta] -> FSTreeDelta
compressFSDeltas xs = appendListToFSTreeDelta xs Map.empty

appendListToFSTreeDelta :: [FSDelta] -> FSTreeDelta -> FSTreeDelta
appendListToFSTreeDelta xs t = foldl' (flip appendToFSTreeDelta) t xs

changeFSTreeDeltaNode :: FSTreeDeltaNode -> OnDisk -> FSTreeDeltaNode
changeFSTreeDeltaNode td@(FSTreeNew tds mv _) dsk = FSTreeNew tds mv dsk
changeFSTreeDeltaNode td@(FSTreeChg tds _) dsk = FSTreeChg tds dsk
changeFSTreeDeltaNode td@(FSTreeNop tds) dsk = FSTreeChg tds dsk
changeFSTreeDeltaNode td@(FSTreeRem) dsk = FSTreeChg Map.empty dsk

insertFSTreeDelta :: FileName -> FSTreeDeltaNode -> FSTreeDelta -> FSTreeDelta
insertFSTreeDelta = Map.insert

deleteFSTreeDelta :: FileName -> FSTreeDelta -> FSTreeDelta
deleteFSTreeDelta = Map.delete

lookupFSTreeDelta :: FileName -> FSTreeDelta -> Maybe FSTreeDeltaNode
lookupFSTreeDelta name td = Map.lookup name td

removeFSTreeDeltaNode :: FileName -> FSTreeDelta -> FSTreeDelta
removeFSTreeDeltaNode name tds = case lookupFSTreeDelta name tds of
	Just td' -> deleteFSTreeDelta name tds
	Nothing -> insertFSTreeDelta name FSTreeRem tds

appendToFSTreeDelta :: FSDelta -> FSTreeDelta -> FSTreeDelta
appendToFSTreeDelta (Add path dsk) td = focusFSTreeDelta path (insertFSTreeDelta (takeFileName path) $ FSTreeNew Map.empty Nothing dsk) td
appendToFSTreeDelta (Rem path) td = focusFSTreeDelta path (removeFSTreeDeltaNode (takeFileName path)) td
appendToFSTreeDelta (Move from to dsk) td = case findFSTreeDeltaNode from (removeFSTreeDeltaNode (takeFileName from)) td of
	(td',Just (_,FSTreeNew tds src dsk)) -> focusFSTreeDelta to (insertFSTreeDelta (takeFileName to) $ FSTreeNew tds src dsk) td'
	otherwise -> focusFSTreeDelta to (insertFSTreeDelta (takeFileName to) $ FSTreeNew Map.empty (Just from) dsk) td
appendToFSTreeDelta (ChgAttrs path dsk) td = focusFSTreeDelta path (\tds -> case lookupFSTreeDelta (takeFileName path) tds of
	Just td -> insertFSTreeDelta (takeFileName path) (changeFSTreeDeltaNode td dsk) tds
	Nothing -> insertFSTreeDelta (takeFileName path) (FSTreeChg Map.empty dsk) tds) td
	
updateFSTreeDeltaDir :: (FSTreeDelta -> FSTreeDelta) -> (FSTreeDeltaNode -> FSTreeDeltaNode)
updateFSTreeDeltaDir upd (FSTreeNew tds moved dsk) = FSTreeNew (upd tds) moved dsk
updateFSTreeDeltaDir upd (FSTreeChg tds dsk) = FSTreeChg (upd tds) dsk
updateFSTreeDeltaDir upd (FSTreeNop tds) = FSTreeNop (upd tds)

-- finds and updates a delta for a specific filepath when found, returning the new global tree and the local tree before being updated
findFSTreeDeltaNode :: FilePath -> (FSTreeDelta -> FSTreeDelta) -> FSTreeDelta -> (FSTreeDelta,Maybe (FileName,FSTreeDeltaNode))
findFSTreeDeltaNode path ifFound tds = findFSTreeDeltaNode' (splitDirectories path) ifFound tds
	
findFSTreeDeltaNode' :: [FileName] -> (FSTreeDelta -> FSTreeDelta) -> FSTreeDelta -> (FSTreeDelta,Maybe (FileName,FSTreeDeltaNode))
findFSTreeDeltaNode' [] ifFound tds = (tds,Nothing)
findFSTreeDeltaNode' [name] ifFound tds = (ifFound tds,fmap (name,) $ lookupFSTreeDelta name tds)
findFSTreeDeltaNode' (name:path) ifFound tds = case lookupFSTreeDelta name tds of
	Just td -> case findFSTreeDeltaNode' path ifFound (childrenFSTreeDeltaNode td) of
		(children',Just (name,td')) -> (insertFSTreeDelta name (updateFSTreeDeltaNodeChildren td children') tds,Just (name,td'))
		(children',Nothing) -> (tds,Nothing)
	Nothing -> (tds,Nothing)

updateFSTreeDeltaNodeChildren :: FSTreeDeltaNode -> FSTreeDelta -> FSTreeDeltaNode
updateFSTreeDeltaNodeChildren (FSTreeNew tds mv dsk) tds' = FSTreeNew tds' mv dsk
updateFSTreeDeltaNodeChildren (FSTreeChg tds dsk) tds' = FSTreeChg tds' dsk
updateFSTreeDeltaNodeChildren (FSTreeNop tds) tds' = FSTreeNop tds'
updateFSTreeDeltaNodeChildren td tds = td

-- applies a tree delta transformation under the parent directory of a given filepath
focusFSTreeDelta :: FilePath -> (FSTreeDelta -> FSTreeDelta) -> (FSTreeDelta -> FSTreeDelta)
focusFSTreeDelta path ifFound tds = case splitDirectories (takeDirectory path) of
	[] -> ifFound tds
	parent -> focusFSTreeDelta' parent ifFound tds

focusFSTreeDelta' :: [FileName] -> (FSTreeDelta -> FSTreeDelta) -> FSTreeDelta -> FSTreeDelta
focusFSTreeDelta' [] upd tds = tds
focusFSTreeDelta' [name] upd tds = case lookupFSTreeDelta name tds of
	Just td -> insertFSTreeDelta name (updateFSTreeDeltaDir upd td) tds
	Nothing -> insertFSTreeDelta name (updateFSTreeDeltaDir upd $ FSTreeNop Map.empty) tds
focusFSTreeDelta' (name:path) upd tds = case lookupFSTreeDelta name tds of
	Just td -> let children' = focusFSTreeDelta' path upd (childrenFSTreeDeltaNode td)
	           in insertFSTreeDelta name (updateFSTreeDeltaNodeChildren td children') tds
	Nothing -> insertFSTreeDelta name (FSTreeNop (focusFSTreeDelta' path upd Map.empty)) tds

-- ** Intersection

-- | given a path and a relative @FSTreeDelta@ under that path, returns a @FSTreeDelta@ over exactly that path
unfocusFSTreeDelta :: FSTreeDelta -> FilePath -> FSTreeDelta
unfocusFSTreeDelta td = unfocusFSTreeDelta' td . splitDirectories
	where
	unfocusFSTreeDelta' td [] = td
	unfocusFSTreeDelta' td (dir:dirs) = Map.singleton dir $ FSTreeNop $ unfocusFSTreeDelta' td dirs

-- | removes a top-level slash from an absolute @FSTreeDelta@
relativeFSTreeDelta :: FSTreeDelta -> FSTreeDelta
relativeFSTreeDelta td = case Map.keys td of
	[] -> Map.empty
	["/"] -> case head (Map.elems td) of
		FSTreeNop td' -> td'
		otherwise -> error $ "relativeFSTreeDelta: not absolute " ++ show td
	otherwise -> error $ "relativeFSTreeDelta: not absolute " ++ show td

focusFSTreeDeltaByRelativePath :: FSTreeDelta -> FilePath -> FSTreeDelta
focusFSTreeDeltaByRelativePath td relpath = Map.fromList $ maybeToList $ snd $ findFSTreeDeltaNode relpath id td

focusFSTreeDeltaByRelativePathMay :: FSTreeDelta -> FilePath -> FSTreeDeltaNodeMay
focusFSTreeDeltaByRelativePathMay td relpath = fmap snd $ snd $ findFSTreeDeltaNode relpath id td

focusFSTreeDeltaNodeMayByRelativePath :: FSTreeDeltaNodeMay -> FilePath -> FSTreeDeltaNodeMay
focusFSTreeDeltaNodeMayByRelativePath Nothing relpath = Nothing
focusFSTreeDeltaNodeMayByRelativePath (Just td) relpath = fmap snd $ snd (findFSTreeDeltaNode relpath id $ childrenFSTreeDeltaNode td)

