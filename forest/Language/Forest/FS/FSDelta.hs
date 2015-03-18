{-# LANGUAGE DeriveDataTypeable, Rank2Types, GADTs, ViewPatterns, TupleSections #-}

module Language.Forest.FS.FSDelta where

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
import System.Posix.Files
import Safe
import Data.List as List
import Data.DList as DList
import Data.IORef
import Data.Typeable

import Debug.Trace

type OnDisk = FilePath

type FSTreeDeltaNodeMay = Maybe FSTreeDeltaNode

-- filesystem deltas store the modified file content
data FSTreeDeltaNode = FSTreeNew FSTreeDelta MoveFrom OnDisk OnDisk -- a new directory or file (the first OnDisk is for content and the second for permisions)
					 | FSTreeChg FSTreeDelta OnDisk -- a directory or file whose metadata has changed
					 | FSTreeNop FSTreeDelta -- a directory or file that has not changed
					 | FSTreeRem
					 | FSTreeNewLink FilePath (Maybe OnDisk) -- a new symbolic link to another path from the top-level tree; the on-disk path holds the permissions
	deriving (Show,Eq,Ord,Typeable)

type MoveFrom = Maybe FilePath -- move origin

type FSTreeDelta = Map FileName FSTreeDeltaNode

data FSDelta = Add FilePath OnDisk
             | AddLink FilePath FilePath
			 | Rem FilePath
			 | Move FilePath FilePath OnDisk
			 | ChgAttrs FilePath OnDisk
	deriving (Eq,Show,Ord,Typeable)

-- * Operations on filesystem deltas

getContentsFSTreeDeltaNodeMay :: FilePath -> FSTreeDeltaNodeMay -> IO [FileName]
getContentsFSTreeDeltaNodeMay root Nothing = getDirectoryContentsTry root
getContentsFSTreeDeltaNodeMay root (Just td) = getContentsFSTreeDeltaNode root td

getContentsFSTreeDeltaNode ::FilePath -> FSTreeDeltaNode -> IO [FileName]
getContentsFSTreeDeltaNode root (FSTreeNew tds _ dsk permissions) = do
	files <- getDirectoryContentsTry dsk
	let files' = Map.foldrWithKey (\file td xs -> if isFSTreeDeltaNodeRem td then List.delete file xs else file:xs) files tds
	return $ List.sort $ List.nub files'
getContentsFSTreeDeltaNode root (FSTreeChg tds _) = do
	files <- getDirectoryContentsTry root
	let files' = Map.foldrWithKey (\file td xs -> if isFSTreeDeltaNodeRem td then List.delete file xs else file:xs) files tds
	return $ List.sort $ List.nub files'
getContentsFSTreeDeltaNode root (FSTreeNop tds) = do
	files <- getDirectoryContentsTry root
	let files' = Map.foldrWithKey (\file td xs -> if isFSTreeDeltaNodeRem td then List.delete file xs else file:xs) files tds
	return $ List.sort $ List.nub files'
getContentsFSTreeDeltaNode root FSTreeRem = return []
getContentsFSTreeDeltaNode root (FSTreeNewLink tgt _) = error "getContentsFSTreeDeltaNode link"

isFSTreeDeltaNodeRem :: FSTreeDeltaNode -> Bool
isFSTreeDeltaNodeRem FSTreeRem = True
isFSTreeDeltaNodeRem _ = False

commitFSTreeDelta :: FilePath -> FSTreeDelta -> IO (Set FilePath)
commitFSTreeDelta root = Map.foldrWithKey aux (return Set.empty) where
	aux file td m = do
		m1 <- commitFSTreeDeltaNode (root </> file) td
		m2 <- m
		return $ m1 `Set.union` m2

commitFSTreeDeltaNode :: FilePath -> FSTreeDeltaNode -> IO (Set FilePath)
commitFSTreeDeltaNode root (FSTreeNew td _ ondisk permissions) = do
	-- remove any data at this path before creating the link
	runShellCommand_ $ "rm -rf " ++ show root
	-- copy the new data
	createDirectoryIfMissing True (takeDirectory root)
	runShellCommand_ $ "cp -rf " ++ show ondisk ++ " " ++ show root
	copyPermissions permissions root
	xs <- commitFSTreeDelta root td
	return $ Set.insert root xs
commitFSTreeDeltaNode root (FSTreeChg td ondisk) = do
	copyPermissions ondisk root
	xs <- commitFSTreeDelta root td
	return $ Set.insert root xs
commitFSTreeDeltaNode root (FSTreeNop td) = do
	commitFSTreeDelta root td
commitFSTreeDeltaNode root FSTreeRem = do
	runShellCommand_ $ "rm -rf " ++ show root
	return $ Set.singleton root
commitFSTreeDeltaNode root (FSTreeNewLink link mbondisk) = do
	-- remove any data at this path before creating the link
	runShellCommand_ $ "rm -rf " ++ show root
	-- create a symbolic link
--	putStrLn $ "creating link " ++ show link ++ " " ++ show root
	--createSymbolicLink link root
	createDirectoryIfMissing True (takeDirectory root)
	runShellCommand_ $ "ln -s " ++ show link ++ " " ++ show root
--	case mbondisk of
--		Nothing -> return ()
--		Just ondisk -> copyPermissions ondisk root
	return $ Set.singleton root

onDiskWriteMay :: FSTreeDeltaNodeMay -> Maybe OnDisk
onDiskWriteMay = maybe Nothing onDiskWrite

onDiskWrite :: FSTreeDeltaNode -> Maybe OnDisk
onDiskWrite (FSTreeNew _ _ ondisk permissions) = Just ondisk
onDiskWrite _ = Nothing

fsTreeDeltaWrites :: FilePath -> FSTreeDelta -> Set FilePath
fsTreeDeltaWrites root td = Map.foldrWithKey aux Set.empty td where
	aux file td xs = fsTreeDeltaNodeWrites (root </> file) td `Set.union` xs

fsTreeDeltaNodeWrites :: FilePath -> FSTreeDeltaNode -> Set FilePath
fsTreeDeltaNodeWrites root (FSTreeNew td _ _ _) = Set.insert root (fsTreeDeltaWrites root td)
fsTreeDeltaNodeWrites root (FSTreeChg td _) = Set.insert root (fsTreeDeltaWrites root td)
fsTreeDeltaNodeWrites root (FSTreeNop td) = fsTreeDeltaWrites root td
fsTreeDeltaNodeWrites root FSTreeRem = Set.singleton root
fsTreeDeltaNodeWrites root (FSTreeNewLink _ _) = Set.singleton root

childrenFSTreeDeltaNode = maybe Map.empty id . childrenFSTreeDeltaNodeMay

childrenFSTreeDeltaNodeMay :: FSTreeDeltaNode -> Maybe FSTreeDelta
childrenFSTreeDeltaNodeMay (FSTreeNew tds _ _ _) = Just tds
childrenFSTreeDeltaNodeMay (FSTreeChg tds _) = Just tds
childrenFSTreeDeltaNodeMay (FSTreeNop tds) = Just tds
childrenFSTreeDeltaNodeMay _ = Nothing

emptyFSTreeDelta :: FSTreeDelta
emptyFSTreeDelta = Map.empty

isEmptyFSTreeDelta :: FSTreeDelta -> Bool
isEmptyFSTreeDelta = Map.null

isEmptyFSTreeDeltaNodeMay :: FSTreeDeltaNodeMay -> Bool
isEmptyFSTreeDeltaNodeMay = isNothing

isEmptyTopFSTreeDelta :: FSTreeDelta -> Bool
isEmptyTopFSTreeDelta tds = case (Map.toList tds) of
	[] -> True
	[(_,FSTreeNop td)] -> True
	otherwise -> False

isEmptyTopFSTreeDeltaNodeMay :: FSTreeDeltaNodeMay -> Bool
isEmptyTopFSTreeDeltaNodeMay tds = case tds of
	Nothing -> True
	Just (FSTreeNop td) -> True
	otherwise -> False

isChgFSTreeDelta :: FSTreeDelta -> Bool
isChgFSTreeDelta tds = case (Map.toList tds) of
	[(_,FSTreeChg td _)] -> True
	otherwise -> False
	
isChgFSTreeDeltaNodeMay :: FSTreeDeltaNodeMay -> Bool
isChgFSTreeDeltaNodeMay tds = case tds of
	Just (FSTreeChg td _) -> True
	otherwise -> False

isMoveFSTreeDelta :: FSTreeDelta -> Maybe FilePath
isMoveFSTreeDelta tds = case (Map.toList tds) of
	[(_,FSTreeNew td (Just from) _ _)] -> Just from
	otherwise -> Nothing
	
isMoveFSTreeDeltaNodeMay :: FSTreeDeltaNodeMay -> Maybe FilePath
isMoveFSTreeDeltaNodeMay tds = case tds of
	Just (FSTreeNew td (Just from) _ _) -> Just from
	otherwise -> Nothing

-- ** Compression

type FSDeltas = DList FSDelta

compressFSDeltas :: FSDeltas -> FSTreeDelta
compressFSDeltas xs = appendListToFSTreeDelta xs Map.empty

appendListToFSTreeDelta :: FSDeltas -> FSTreeDelta -> FSTreeDelta
appendListToFSTreeDelta xs t = foldl' (flip appendToFSTreeDelta) t (DList.toList xs)

appendListToFSTreeSym :: FSDeltas -> FSTreeSym -> FSTreeSym
appendListToFSTreeSym xs t = foldl' (flip appendToFSTreeSym) t (DList.toList xs)

changeAttrsFSTreeDeltaNode :: FSTreeDeltaNode -> OnDisk -> FSTreeDeltaNode
changeAttrsFSTreeDeltaNode td@(FSTreeNew tds mv file _) dsk = FSTreeNew tds mv file dsk
changeAttrsFSTreeDeltaNode td@(FSTreeChg tds _) dsk = FSTreeChg tds dsk
changeAttrsFSTreeDeltaNode td@(FSTreeNop tds) dsk = FSTreeChg tds dsk
changeAttrsFSTreeDeltaNode td@(FSTreeRem) dsk = FSTreeChg Map.empty dsk
changeAttrsFSTreeDeltaNode td@(FSTreeNewLink tgt _) dsk = FSTreeNewLink tgt (Just dsk)

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
appendToFSTreeDelta (Add path dsk) td = transformFSTreeDelta (takeDirectory path) (insertFSTreeDelta (takeFileName path) $ FSTreeNew Map.empty Nothing dsk dsk) td
appendToFSTreeDelta (AddLink path tgt) td = transformFSTreeDelta (takeDirectory path) (insertFSTreeDelta (takeFileName path) $ FSTreeNewLink tgt Nothing) td
appendToFSTreeDelta (Rem path) td = transformFSTreeDelta (takeDirectory path) (removeFSTreeDeltaNode (takeFileName path)) td
appendToFSTreeDelta (Move from to dsk) td = case findFSTreeDeltaNode from (removeFSTreeDeltaNode (takeFileName from)) td of
	(td',Just (_,FSTreeNew tds src dsk dsk2)) -> transformFSTreeDelta (takeDirectory to) (insertFSTreeDelta (takeFileName to) $ FSTreeNew tds src dsk dsk2) td'
	(td',Just (_,childrenFSTreeDeltaNodeMay -> Just tds)) -> transformFSTreeDelta (takeDirectory to) (insertFSTreeDelta (takeFileName to) $ FSTreeNew tds (Just from) dsk dsk) td
	otherwise -> transformFSTreeDelta (takeDirectory to) (insertFSTreeDelta (takeFileName to) $ FSTreeNew Map.empty (Just from) dsk dsk) td
appendToFSTreeDelta (ChgAttrs path dsk) td = transformFSTreeDelta (takeDirectory path) (\tds -> case lookupFSTreeDelta (takeFileName path) tds of
	Just td -> insertFSTreeDelta (takeFileName path) (changeAttrsFSTreeDeltaNode td dsk) tds
	Nothing -> insertFSTreeDelta (takeFileName path) (FSTreeChg Map.empty dsk) tds) td
	
appendToFSTreeSym :: FSDelta -> FSTreeSym -> FSTreeSym
appendToFSTreeSym (AddLink path tgt) = transformFSTreeSym path (Map.insert (takeFileName path) (FSTreeSymLink tgt))
appendToFSTreeSym (Rem path) = transformFSTreeSym path (Map.delete (takeFileName path))
appendToFSTreeSym d = id
	
updateFSTreeDeltaDir :: (FSTreeDelta -> FSTreeDelta) -> (FSTreeDeltaNode -> FSTreeDeltaNode)
updateFSTreeDeltaDir upd (FSTreeNew tds moved dsk dsk2) = FSTreeNew (upd tds) moved dsk dsk2
updateFSTreeDeltaDir upd (FSTreeChg tds dsk) = FSTreeChg (upd tds) dsk
updateFSTreeDeltaDir upd (FSTreeNop tds) = FSTreeNop (upd tds)
updateFSTreeDeltaDir upd (FSTreeNewLink _ _) = error "updateFSTreeDeltaDir link"
updateFSTreeDeltaDir upd td = td

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
updateFSTreeDeltaNodeChildren (FSTreeNew tds mv dsk dsk2) tds' = FSTreeNew tds' mv dsk dsk2
updateFSTreeDeltaNodeChildren (FSTreeChg tds dsk) tds' = FSTreeChg tds' dsk
updateFSTreeDeltaNodeChildren (FSTreeNop tds) tds' = FSTreeNop tds'
updateFSTreeDeltaNodeChildren (FSTreeNewLink _ _) tds = error "updateFSTreeDeltaNodeChildren link"
updateFSTreeDeltaNodeChildren td tds = td

-- applies a tree delta transformation under the parent directory of a given filepath
transformFSTreeDelta :: FilePath -> (FSTreeDelta -> FSTreeDelta) -> (FSTreeDelta -> FSTreeDelta)
transformFSTreeDelta path ifFound tds = case splitDirectories path of
	[] -> ifFound tds
	parent -> transformFSTreeDelta' parent ifFound tds
  where
	transformFSTreeDelta' :: [FileName] -> (FSTreeDelta -> FSTreeDelta) -> FSTreeDelta -> FSTreeDelta
	transformFSTreeDelta' [] upd tds = tds
	transformFSTreeDelta' [name] upd tds = case lookupFSTreeDelta name tds of
		Just td -> insertFSTreeDelta name (updateFSTreeDeltaDir upd td) tds
		Nothing -> insertFSTreeDelta name (updateFSTreeDeltaDir upd $ FSTreeNop Map.empty) tds
	transformFSTreeDelta' (name:path) upd tds = case lookupFSTreeDelta name tds of
		Just td -> let children' = transformFSTreeDelta' path upd (childrenFSTreeDeltaNode td)
		           in insertFSTreeDelta name (updateFSTreeDeltaNodeChildren td children') tds
		Nothing -> insertFSTreeDelta name (FSTreeNop (transformFSTreeDelta' path upd Map.empty)) tds

-- applies a tree delta transformation under the parent directory of a given filepath
transformFSTreeSym :: FilePath -> (FSTreeSym -> FSTreeSym) -> (FSTreeSym -> FSTreeSym)
transformFSTreeSym path ifFound root_ts = case splitDirectories path of
	[] -> ifFound root_ts
	parent -> transformFSTreeSym' "" parent ifFound root_ts
  where
	transformFSTreeSym' :: FilePath -> [FileName] -> (FSTreeSym -> FSTreeSym) -> FSTreeSym -> FSTreeSym
	transformFSTreeSym' root [] upd ts = ts
	transformFSTreeSym' root [name] upd ts = case Map.lookup name ts of
		Just tsn -> Map.insert name (updateFSTreeSymDir (root </> name) root_ts upd tsn) ts
		Nothing -> Map.insert name (updateFSTreeSymDir (root </> name) root_ts upd $ FSTreeSymFileDir Map.empty) ts
	transformFSTreeSym' root (name:names) upd ts = case Map.lookup name ts of
		Just tsn -> let children' = transformFSTreeSym' (root </> name) names upd (childrenFSTreeSymNode (root </> name) root_ts tsn)
		            in Map.insert name (updateFSTreeSymNodeChildren tsn children') ts
		Nothing -> Map.insert name (FSTreeSymFileDir (transformFSTreeSym' (root </> name) names upd Map.empty)) ts

childrenFSTreeSymNode :: FilePath -> FSTreeSym -> FSTreeSymNode -> FSTreeSym
childrenFSTreeSymNode root root_ts (FSTreeSymFileDir tds) = tds
childrenFSTreeSymNode root root_ts (FSTreeSymLink tgt) = focusFSTreeSym (root </> tgt) root_ts

updateFSTreeSymDir :: FilePath -> FSTreeSym -> (FSTreeSym -> FSTreeSym) -> (FSTreeSymNode -> FSTreeSymNode)
updateFSTreeSymDir root root_ts upd (FSTreeSymLink tgt) = FSTreeSymFileDir $ upd $ focusFSTreeSym (root </> tgt) root_ts
updateFSTreeSymDir root root_ts upd (FSTreeSymFileDir tds) = FSTreeSymFileDir (upd tds)

updateFSTreeSymNodeChildren :: FSTreeSymNode -> FSTreeSym -> FSTreeSymNode
updateFSTreeSymNodeChildren (FSTreeSymLink tgt) tds' = FSTreeSymFileDir tds'
updateFSTreeSymNodeChildren (FSTreeSymFileDir tds) tds' = FSTreeSymFileDir tds'

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
	["/"] -> case List.head (Map.elems td) of
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

-- focuses a FSTreeDelta with symlink information by a relative path
--focusFSTreeDeltaSymByRelativePathMay :: (FSTreeDelta,FSTreeSym) -> FilePath -> (FSTreeDelta,FSTreeSym)

-- paths are naturally canonical, because we don't resolve symlinks
type FSTreeSym = Map FileName FSTreeSymNode

data FSTreeSymNode =
	  FSTreeSymLink FilePath -- a symbolic link
	| FSTreeSymFileDir FSTreeSym -- a file or directory
  deriving (Eq,Show,Ord,Typeable)

-- focuses a symtree on some path; resolves symlinks
focusFSTreeSym :: FilePath -> FSTreeSym -> FSTreeSym
focusFSTreeSym path root_ts = focusFSTreeSym' "" (splitDirectories path) root_ts
	where
	focusFSTreeSym' :: FilePath -> [FileName] -> FSTreeSym -> FSTreeSym
	focusFSTreeSym' root [] ts = ts
	focusFSTreeSym' root (name:names) ts = case Map.lookup name ts of
		Nothing -> Map.empty
		Just tsn -> focusFSTreeSymNode' root name names tsn
	focusFSTreeSymNode' :: FilePath -> FileName -> [FileName] -> FSTreeSymNode -> FSTreeSym
	focusFSTreeSymNode' root name names (FSTreeSymLink target) = focusFSTreeSym (root </> target </> joinPath names) root_ts
	focusFSTreeSymNode' root name names (FSTreeSymFileDir ts) = focusFSTreeSym' (root </> name) names ts

findSymLinks :: FilePath -> IO FSTreeSym
findSymLinks path = do
	xs <- allSymLinksUnder path
	return $ foldl (\ts (src,tgt) -> appendToFSTreeSym (AddLink src tgt) ts) Map.empty xs

-- expand a FSTreeDelta according to the supplied symlinks
fixFSTreeDelta :: FSTreeDelta -> FSTreeSym -> FSTreeDelta
fixFSTreeDelta root_td root_ts = fixFSTreeDelta' "" root_td root_ts
	where
	fixFSTreeDelta' :: FilePath -> FSTreeDelta -> FSTreeSym -> FSTreeDelta
	fixFSTreeDelta' root = Map.mergeWithKey
		(fixFSTreeDeltaSymNode' root)
		(Map.mapMaybeWithKey (fixFSTreeDeltaNode' root))
		(Map.mapMaybeWithKey (fixFSTreeSymNode' root))
	fixFSTreeDeltaSymNode' :: FilePath -> FileName -> FSTreeDeltaNode -> FSTreeSymNode -> Maybe FSTreeDeltaNode
	fixFSTreeDeltaSymNode' root rel tdn@(FSTreeNew td from c a) (FSTreeSymLink tgt) = Just $ (\td -> FSTreeNew td from c a) $ fixFSTreeDelta' (root </> rel) td (focusFSTreeSym (root </> tgt) root_ts)
	fixFSTreeDeltaSymNode' root rel tdn@(FSTreeNew td from c a) (FSTreeSymFileDir tsn) = Just $ (\td -> FSTreeNew td from c a) $ fixFSTreeDelta' (root </> rel) td tsn
	fixFSTreeDeltaSymNode' root rel tdn@(FSTreeChg td a) (FSTreeSymLink tgt) = Just $ flip FSTreeChg a $ fixFSTreeDelta' (root </> rel) td (focusFSTreeSym (root </> tgt) root_ts)
	fixFSTreeDeltaSymNode' root rel tdn@(FSTreeChg td a) (FSTreeSymFileDir tsn) = Just $ flip FSTreeChg a $ fixFSTreeDelta' (root </> rel) td tsn
	fixFSTreeDeltaSymNode' root rel tdn@(FSTreeNop td) (FSTreeSymLink tgt) = maybeFSTreeNop $ fixFSTreeDelta' (root </> rel) td (focusFSTreeSym (root </> tgt) root_ts)
	fixFSTreeDeltaSymNode' root rel tdn@(FSTreeNop td) (FSTreeSymFileDir tsn) = maybeFSTreeNop $ fixFSTreeDelta' (root </> rel) td tsn
	fixFSTreeDeltaSymNode' root rel tdn@(FSTreeRem) ts = Just tdn
	fixFSTreeDeltaSymNode' root rel tdn@(FSTreeNewLink tgt a) ts = Just $ (\td -> FSTreeNew td Nothing (error "no content") (fromJustNote "fixFSTreeDelta" a)) $ fixFSTreeDelta
		(focusFSTreeDeltaByRelativePath root_td (root </> tgt))
		(focusFSTreeSym (root </> tgt) root_ts)
	
	fixFSTreeDeltaNode' :: FilePath -> FileName -> FSTreeDeltaNode -> Maybe FSTreeDeltaNode
	fixFSTreeDeltaNode' root rel tdn = Just tdn
	
	fixFSTreeSymNode' :: FilePath -> FileName -> FSTreeSymNode -> Maybe FSTreeDeltaNode
	fixFSTreeSymNode' root rel (FSTreeSymLink tgt) = maybeFSTreeNop $ fixFSTreeDelta' (root </> rel) Map.empty (focusFSTreeSym (root </> tgt) root_ts)
	fixFSTreeSymNode' root rel (FSTreeSymFileDir tsn) = maybeFSTreeNop $ fixFSTreeDelta' (root </> rel) Map.empty tsn

maybeFSTreeNop :: FSTreeDelta -> Maybe FSTreeDeltaNode
maybeFSTreeNop tds = if (Map.null tds) then Nothing else Just (FSTreeNop tds)
