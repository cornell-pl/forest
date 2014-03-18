{-# LANGUAGE Rank2Types, GADTs, ViewPatterns #-}

module Language.Forest.Delta where

import {-# SOURCE #-} Language.Forest.MetaData

import Control.Monad
import Control.Lens
import Data.Maybe
import Data.List
import Data.Functor.Identity
import Data.List.Split

import Debug.Trace

type FileSystemDeltas = [FileSystemDelta]

-- | Filesystem edit operations
data FileSystemDelta = AddFile FilePath | RemFile FilePath | MoveFile FilePath FilePath			-- files
                     | AddDir FilePath | RemDir FilePath | MoveDir FilePath FilePath			-- directories (we assume that only empty directories can be removed)
					 | AddLink FilePath FilePath | RemLink FilePath 							-- symbolic links (add a link from path r' to path r)
					 | ChangeAttrs FilePath FileInfo											-- file/directory/link attributes
	deriving (Eq,Ord,Show)

isEmptyFSD :: FileSystemDeltas -> Bool
isEmptyFSD = (==[])

domainFSDelta :: FileSystemDelta -> Maybe FilePath
domainFSDelta (AddFile r) = Nothing
domainFSDelta (RemFile r) = Just r
domainFSDelta (MoveFile r r1) = Just r
domainFSDelta (AddDir r) = Nothing
domainFSDelta (RemDir r) = Just r
domainFSDelta (MoveDir r r1) = Just r
domainFSDelta (AddLink r r') = Nothing
domainFSDelta (RemLink r) = Just r
domainFSDelta (ChangeAttrs r a) = Just r

rangeFSDelta :: FileSystemDelta -> Maybe FilePath
rangeFSDelta (AddFile r) = Just r
rangeFSDelta (RemFile r) = Nothing
rangeFSDelta (MoveFile r r1) = Just r1
rangeFSDelta (AddDir r) = Just r
rangeFSDelta (RemDir r) = Nothing
rangeFSDelta (MoveDir r r1) = Just r1
rangeFSDelta (AddLink r r') = Just r'
rangeFSDelta (RemLink r) = Nothing
rangeFSDelta (ChangeAttrs r a) = Just r

-- cuts transitive moves like (add a ; move a b) or (move a b ; move b c) or (move /a /b/c ; move /b /d)
normalizeFSDeltas :: FileSystemDeltas -> FileSystemDeltas
normalizeFSDeltas = traces2deltas . traceFSDeltas []

type Trace = (FileSystemDelta,Maybe FileInfo) -- single FS operation, plus optional change of attributes

traces2deltas :: [Trace] -> FileSystemDeltas
traces2deltas [] = []
traces2deltas ((u,Nothing):ts) = u : traces2deltas ts
traces2deltas ((u,Just a):ts) = u : ChangeAttrs (fromJust $ rangeFSDelta u) a : traces2deltas ts

-- a trace is a single filesystem operation. sequences of traces depend on order.
-- each trace describes what happens to a particular object in the filesystem during an update (no recursive moves)
traceFSDeltas :: [Trace] -> FileSystemDeltas -> [Trace]
traceFSDeltas traces [] = traces
traceFSDeltas traces (u:us) = traceFSDeltas (traceFSDelta traces u) us

traceFSDelta :: [Trace] -> FileSystemDelta -> [Trace]
traceFSDelta [] u = [(u,Nothing)]

traceFSDelta ((AddFile r,a):ts) (RemFile ((==r) -> True)) = ts -- delete added file
traceFSDelta ((AddFile r,a):ts) (MoveFile ((==r) -> True) r1) = (AddFile r1,a) : ts -- move added file
traceFSDelta ((AddFile r,a):ts) u@(MoveDir (isSubPathOf r -> Just p) d1) = (AddFile (concatPath d1 r),a) : traceFSDelta ts u -- add file and move parent directory
traceFSDelta ((AddFile r,a):ts) (ChangeAttrs ((==r) -> True) a') = (AddFile r,Just a') : ts -- add file and change its attributes

traceFSDelta (((RemFile r),a):ts) u = traceFSDelta ts u -- anything is independent from a removed file

traceFSDelta ((MoveFile r r1,a):ts) u@(RemFile ((==r1) -> True)) = (u,Nothing) : ts -- remove moved file
traceFSDelta (m@(MoveFile r r1,a):ts) u@(MoveFile ((==r1) -> True) r2) = (MoveFile r r2,a) : ts -- file moved twice
traceFSDelta (m@(MoveFile r r1,a):ts) u@(MoveDir (isSubPathOf r1 -> Just p) d1) = (MoveFile r (concatPath d1 p),a) : traceFSDelta ts u -- move parent directory
traceFSDelta ((MoveFile r r1,a):ts) (ChangeAttrs ((==r1) -> True) a') = (MoveFile r r1,Just a') : ts -- move file and change its attributes

traceFSDelta ((AddDir d,a):ts) (RemDir ((==d) -> True)) = ts -- deleted added directory
traceFSDelta ((AddDir d,a):ts) (MoveDir ((==d) -> True) d2) = (AddDir d2,a) : ts -- move added directory
traceFSDelta ((AddDir d,a):ts) u@(MoveDir (isSubPathOf d -> Just p) d2) = (AddDir (concatPath d2 p),a) : traceFSDelta ts u -- move parent directory
traceFSDelta ((AddDir d,a):ts) (ChangeAttrs ((==d) -> True) a') = (AddDir d,Just a') : ts -- add directory and change its attributes

traceFSDelta (((RemDir r),a):ts) u = traceFSDelta ts u -- anything is independent from a removed directory

traceFSDelta (m@(MoveDir d d1,a):ts) u@(MoveFile (isSuperPathOf d1 -> Just p) r1) = (MoveFile (concatPath d p) r1,Nothing) : m : ts -- move file within directory
traceFSDelta (m@(MoveDir d d1,a):ts) u@(MoveDir ((==d1) -> True) d2) = (MoveDir d d2,a) : ts -- move directory twice
traceFSDelta (m@(MoveDir d d1,a):ts) u@(MoveDir (isSubPathOf d1 -> Just p) d2) = (MoveDir d (concatPath d2 p),a) : traceFSDelta ts u -- move parent directory
traceFSDelta (m@(MoveDir d d1,a):ts) u@(MoveDir (isSuperPathOf d1 -> Just p) d2) = (MoveDir (concatPath d p) d2,Nothing) : m : ts -- move directory within directory

traceFSDelta (((AddLink r r'),a):ts) (RemLink ((==r') -> True)) = ts -- delete added link
traceFSDelta ((AddLink r r',a):ts) u@(MoveDir (isSubPathOf r' -> Just p) d1) = (AddLink r (concatPath d1 r'),a) : traceFSDelta ts u -- add file and move parent directory
traceFSDelta ((AddLink r r',a):ts) (ChangeAttrs ((==r') -> True) a') = (AddLink r r',Just a') : ts -- add file and change its attributes

traceFSDelta (((ChangeAttrs r a),a'):ts) u@(RemFile ((==r) -> True)) = (u,Nothing) : ts -- change attributes and remove file
traceFSDelta (((ChangeAttrs r a),a'):ts) u@(MoveFile ((==r) -> True) r1) = (u,Just a `mplus` a') : ts -- change attributes and move file
traceFSDelta (((ChangeAttrs r a),a'):ts) u@(RemDir ((==r) -> True)) = (u,Nothing) : ts -- change attributes and remove directory
traceFSDelta (((ChangeAttrs r a),a'):ts) u@(MoveDir ((==r) -> True) r1) = (u,Just a `mplus` a') : ts -- change attributes and move directory
traceFSDelta (((ChangeAttrs r a),a'):ts) u@(RemLink ((==r) -> True)) = (u,Nothing) : ts -- change attributes and remove link
traceFSDelta ((ChangeAttrs r _,_):ts) (ChangeAttrs ((==r) -> True) a') = (ChangeAttrs r a',Nothing) : ts -- change attributes twice

traceFSDelta (t:ts) u = t : traceFSDelta ts u -- independent operations

-- checks how a filesystem delta affects an exact path
-- assume that the fsdeltas are normalized
-- boolean determines whether the filepath is a directory or a file
intersectFSDeltasWithPath :: Bool -> FileSystemDeltas -> FilePath -> FileSystemDeltas
intersectFSDeltasWithPath isDir df r = df'
	where df' = nub $ concatMap (maybeToList . intersectFSDeltaWithPath isDir r) df

-- note that this may generate invalid updates like (RemFile x ; RemFile x), e.g., when a file and its parent directory are both removed.
intersectFSDeltaWithPath :: Bool -> FilePath -> FileSystemDelta -> Maybe FileSystemDelta
intersectFSDeltaWithPath isDir r u@(AddFile ((==r) -> True)) = Just u
intersectFSDeltaWithPath isDir r u@(RemFile ((==r) -> True)) = Just u
intersectFSDeltaWithPath isDir r (MoveFile ((==r) -> True) r1) = Just $ RemFile r -- move file, so it ceases to exist under this path
intersectFSDeltaWithPath isDir r (MoveFile r1 ((==r) -> True)) = Just $ AddFile r -- move file, so it now exists under this path
intersectFSDeltaWithPath isDir r u@(AddDir ((==r) -> True)) = Just u
intersectFSDeltaWithPath isDir r u@(RemDir ((==r) -> True)) = Just u
intersectFSDeltaWithPath isDir r u@(MoveDir ((==r) -> True) d1) = Just $ RemDir r -- move directory from path
intersectFSDeltaWithPath isDir r u@(MoveDir d1 ((==r) -> True)) = Just $ AddDir r -- move directory to path
intersectFSDeltaWithPath isDir r u@(MoveDir (isSubPathOf r -> Just p) d1) = Just $ if isDir then RemDir r else RemFile r -- move parent directory, so the file ceases to exist under this path
intersectFSDeltaWithPath isDir r u@(MoveDir d1 (isSubPathOf r -> Just p)) = Just $ if isDir then AddDir r else AddFile r -- move parent directory, so the file now exists under this path
intersectFSDeltaWithPath isDir r u@(AddLink _ ((==r) -> True)) = Just u
intersectFSDeltaWithPath isDir r u@(RemLink ((==r) -> True)) = Just u
intersectFSDeltaWithPath isDir r u@(ChangeAttrs ((==r) -> True) a) = Just u
intersectFSDeltaWithPath isDir r u = Nothing

--intersectFSDeltasWithPathAndUnder :: FileSystemDeltas -> FilePath -> FileSystemDeltas
--intersectFSDeltasWithPathAndUnder df r = intersectFSDeltasWithPred (\r1 -> r1 == r || isJust (isSuperPathOf r r1)) df

intersectFSDeltasWithPathsUnder :: FileSystemDeltas -> FilePath -> FileSystemDeltas
intersectFSDeltasWithPathsUnder df r = df'
	where df' = intersectFSDeltasWithPred (isJust . isSuperPathOf r) df

intersectFSDeltasWithPred :: (FilePath -> Bool) -> FileSystemDeltas -> FileSystemDeltas
intersectFSDeltasWithPred p df = concatMap (maybeToList . intersectFSDeltaWithPred p) df

-- the FSdeltas need to be normalized, to avoid the case when a file/directory is transitively moved out and in of the focus
intersectFSDeltaWithPred :: (FilePath -> Bool) -> FileSystemDelta -> Maybe FileSystemDelta
intersectFSDeltaWithPred p u@(AddFile (p -> True)) = Just u
intersectFSDeltaWithPred p u@(RemFile (p -> True)) = Just u
intersectFSDeltaWithPred p u@(MoveFile (p -> True) (p -> True)) = Just u
intersectFSDeltaWithPred p (MoveFile d@(p -> True) d1) = Just $ RemFile d
intersectFSDeltaWithPred p (MoveFile d d1@(p -> True)) = Just $ AddFile d1
intersectFSDeltaWithPred p u@(AddDir (p -> True)) = Just u
intersectFSDeltaWithPred p u@(RemDir (p -> True)) = Just u
intersectFSDeltaWithPred p u@(MoveDir (p -> True) (p -> True)) = Just u
intersectFSDeltaWithPred p (MoveDir d@(p -> True) d1) = Just $ RemDir d
intersectFSDeltaWithPred p (MoveDir d d1@(p -> True)) = Just $ AddDir d1
intersectFSDeltaWithPred p u@(AddLink r (p -> True)) = Just u
intersectFSDeltaWithPred p u@(RemLink (p -> True)) = Just u
intersectFSDeltaWithPred p u@(ChangeAttrs (p -> True) a) = Just u
intersectFSDeltaWithPred p u = Nothing

-- may move the path to itself (when it is unnafected by the FS deltas)
-- the argument path is the "root" under which the modification may occur, so that old and new are both subpaths
movePathToInFSDelta :: FilePath -> FilePath -> FileSystemDeltas -> Maybe FilePath
movePathToInFSDelta path old df = case movePathTo (concatPath path old) df of
	Just (isSuperPathOf path -> Just new) -> Just new
	otherwise -> Nothing

-- may move the path to itself (when it is unnafected by the FS deltas)
movePathToPathInFSDelta :: FilePath -> FilePath -> FilePath -> FileSystemDeltas -> Bool
movePathToPathInFSDelta path old new df = case movePathToInFSDelta path old df of
	Just ((==new) -> True) -> True
	otherwise -> False

-- | Finds out the updated path of a path in the source
movePathTo :: FilePath -> FileSystemDeltas -> Maybe FilePath
movePathTo r [] = Just r
movePathTo r (AddFile ((==r) -> True):us) = Nothing
movePathTo r (RemFile ((==r) -> True):us) = Nothing
movePathTo r (MoveFile ((==r) -> True) r1:us) = movePathTo r1 us
movePathTo r (AddDir ((==r) -> True):us) = Nothing
movePathTo r (RemDir ((==r) -> True):us) = Nothing
movePathTo r (MoveDir ((==r) -> True) d1:us) = movePathTo d1 us
movePathTo r (MoveDir d ((==r) -> True):us) = Nothing
movePathTo r (MoveDir (isSubPathOf r -> Just p) d1:us) = movePathTo (concatPath d1 p) us
movePathTo r (MoveDir d (isSubPathOf r -> Just p):us) = Nothing
movePathTo r (AddLink l ((==r) -> True):us) = Nothing
movePathTo r (RemLink ((==r) -> True):us) = Nothing
movePathTo r (ChangeAttrs ((==r) -> True) a:us) = movePathTo r us
movePathTo r (u:us) = movePathTo r us -- when the first update does not affect the path

type ValueDeltas v = [ValueDelta v]

-- | Algebraic data type operations		
data ValueDelta v where 
	Apply       :: (v -> v) -> ValueDelta v
	Replace		:: v -> ValueDelta v
	ModFst		:: ValueDeltas v1 -> ValueDelta (v1,v2)
	ModSnd		:: ValueDeltas v2 -> ValueDelta (v1,v2)
	ArrowFst	:: (v2 -> v1) -> ValueDelta (v1,v2)
	ModMb		:: ValueDeltas v -> ValueDelta (Maybe v)
	DelPos		:: Int -> ValueDelta [v] -- lists
	InsPos		:: Int -> v -> ValueDelta [v] -- lists
	ReorderPos	:: [(Int,Int)] -> ValueDelta [v] -- lists
	ModPos		:: Int -> ValueDeltas v -> ValueDelta [v] -- lists
	ModLens     :: Lens' s v -> ValueDeltas v -> ValueDelta s

instance Show (ValueDelta v) where
	show (Apply f) = "(Apply f)"
	show (Replace v) = "(Replace v)"
	show (ModFst d) = "(ModFst " ++ show d ++ ")"
	show (ModSnd d) = "(ModSnd " ++ show d ++ ")"
	show (ArrowFst f) = "(ArrowFst f)"
	show (ModMb d) = "(ModMb " ++ show d ++ ")"
	show (DelPos i) = "(DelPos " ++ show i ++ ")"
	show (InsPos i v) = "(InsPos " ++ show i ++ " v)"
	show (ReorderPos f) = "(ReorderPos " ++ show f ++ ")"
	show (ModPos i d) = "(ModPos " ++ show i ++ " " ++ show d ++ ")"
	show (ModLens l d) = "(ModLens l " ++ show d ++ ")"

isEmptyVD :: ValueDeltas v -> Bool
isEmptyVD = and . map isEmptyVD'

isEmptyVD' :: ValueDelta v -> Bool
isEmptyVD' (Apply f) = False
isEmptyVD' (Replace v) = False
isEmptyVD' (ModFst ds) = isEmptyVD ds
isEmptyVD' (ModSnd ds) = isEmptyVD ds
isEmptyVD' (ArrowFst f) = False
isEmptyVD' (ModMb ds) = isEmptyVD ds
isEmptyVD' (DelPos i) = False
isEmptyVD' (InsPos i v) = False
isEmptyVD' (ReorderPos []) = True
isEmptyVD' (ReorderPos l) = False
isEmptyVD' (ModPos i ds) = isEmptyVD ds
isEmptyVD' (ModLens l ds) = isEmptyVD ds

modFst :: ValueDeltas a -> ValueDeltas (a,b)
modFst d = simplifyDelta [ModFst d]

modSnd :: ValueDeltas b -> ValueDeltas (a,b)
modSnd d = simplifyDelta [ModSnd d]

modLens :: Lens' s v -> ValueDeltas v -> ValueDeltas s
modLens lns d = simplifyDelta [ModLens lns d]

applyValueDeltas :: Monad m => [ValueDelta v] -> v -> m v
applyValueDeltas [] v = return v
applyValueDeltas (d:ds) v = applyValueDelta d v >>= applyValueDeltas ds

applyValueDelta :: Monad m => ValueDelta v -> v -> m v
applyValueDelta (Apply f) x = return $ f x
applyValueDelta (Replace x') x = return x'
applyValueDelta (ModFst dx) ~(x,y) = applyValueDeltas dx x >>= \x' -> return (x',y)
applyValueDelta (ModSnd dy) ~(x,y) = applyValueDeltas dy y >>= \y' -> return (x,y')
applyValueDelta (ArrowFst f) ~(x,y) = return (f y,y)
applyValueDelta (ModMb dx) Nothing = return Nothing
applyValueDelta (ModMb dx) (Just x) = liftM Just (applyValueDeltas dx x)
applyValueDelta (DelPos i) xs = deletePos i xs
applyValueDelta (InsPos i x) xs = insertPos i x xs
applyValueDelta (ReorderPos f) xs = reorderPos f xs
applyValueDelta (ModPos i dx) xs = modifyPos i (applyValueDeltas dx) xs
applyValueDelta (ModLens lns dv) s = liftM (\v' -> set lns v' s) $ applyValueDeltas dv (view lns s)
	
-- | Deletes the element at position @i@ from a list
deletePos :: Monad m => Int -> [a] -> m [a]
deletePos i [] = fail $ "deletePos: no element at position "++show i
deletePos i xs = return $ take i xs ++ drop (i+1) xs

-- | Inserts an element at position @i@ in a list
insertPos :: Monad m => Int -> a -> [a] -> m [a]
insertPos i x xs | length xs < i = fail $ "insertPos: no element at position "++show (i-1)
                 | otherwise = return $ take i xs ++ x : drop i xs

-- | Gets an element at position @i@
getPos :: Monad m => Int -> [a] -> m a
getPos i xs = if length xs <= i then fail ("getPos: no element at position "++show i) else return (xs!!i)

-- | Modifies an element at position @i@ in a list
modifyPos :: Monad m => Int -> (a -> m a) -> [a] -> m [a]
modifyPos i f xs = do
	x <- getPos i xs >>= f
	return $ take i xs ++ x : drop (i+1) xs

replacePos :: Int -> a -> [a] -> [a]
replacePos i a = runIdentity . modifyPos i (return . const a)

-- | Reorders two elements in a list
reorderPos :: Monad m => [(Int,Int)] -> [a] -> m [a]
reorderPos perms xs = reorderPos' perms xs xs

reorderPos' :: Monad m => [(Int,Int)] -> [a] -> ([a] -> m [a])
reorderPos' [] ori = return
reorderPos' ((i,j):ijs) ori = modifyPos j (const $ return $ ori!!i) >=> reorderPos' ijs ori

isEmptyValueDelta :: ValueDelta v -> Bool
isEmptyValueDelta = undefined

replaceDelta :: a -> ValueDeltas a
replaceDelta x = [Replace x]

replaceBoth :: (a,b) -> (ValueDeltas a,ValueDeltas b)
replaceBoth (x,y) = ([Replace x],[Replace y])

prodDelta :: ValueDeltas a -> ValueDeltas b -> ValueDeltas (a,b)
prodDelta dx dy = [ModFst dx,ModSnd dy]

simplifyDelta :: ValueDeltas v -> ValueDeltas v
simplifyDelta d = if isEmptyVD d then [] else d

type ListDeltas a = ValueDeltas [a] -- we assume that only operations on lists are supported

concatPath stem new = 
  if isGlobal new then new else
    case new of
     [] -> stem
     ('/':rest) -> new
     otherwise -> stem ++ "/" ++ new

isGlobal s = case s of 
  '/' : rest -> True
  otherwise -> False

isSubPathOf :: FilePath -> FilePath -> Maybe FilePath
isSubPathOf subpath path = case stripPrefix path subpath of
	Just ('/':str) -> Just str
	otherwise -> Nothing
	
isSuperPathOf = flip isSubPathOf

localname :: FilePath -> FilePath
localname fullpath = last (splitOn "/" fullpath)

fileExtension :: FilePath -> (String,String)
fileExtension file = splitOnTwo "." file
		
splitOnTwo tok str = case toks of
	[x] -> (x,"")
	otherwise -> (foldr1 (\x y -> x++"."++y) $ init toks,last toks)
	where toks = splitOn tok str
		
