{-# LANGUAGE TupleSections, ViewPatterns #-}

-- | A simple linear-time list alignment algorithm presented in http://dl.acm.org/citation.cfm?id=359467
-- It performs bad when elements are duplicated, but in forest we align (old and new) filenames listed in a directory, that are supposed to be unique.

module Data.List.Diff where
	
import Data.List
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Array.MArray
import Data.Array.IO
import Data.Either
import Safe

-- * Data structures (IOArrays for constant array update operations)

type SymbolTable a = Map a SEntry
type SEntry = (OC,NC,Maybe Int,Maybe Int)

data Entry = Zero | One | Many deriving (Eq,Ord,Show)

type OC = Entry
type NC = Entry

type OA = IOArray Int (Either Int SEntry)
type NA = IOArray Int (Either Int SEntry)

-- * Steps of the algorithm

freshNewEntry i = (Zero,One,Nothing,Just i)

mergeNewEntries :: SEntry -> SEntry -> SEntry
mergeNewEntries (Zero,One,Nothing,Just i) (Zero,One,Nothing,Just j) = (Zero,Many,Nothing,Nothing) -- we never create zero new entries...

mkNewTable :: Ord a => [a] -> SymbolTable a
mkNewTable xs = foldr (\(i,x) m -> Map.insertWith mergeNewEntries x (freshNewEntry i) m) Map.empty (zip [0..] xs)

freshOldEntry j = (One,Zero,Just j,Nothing)

mergeOldEntries :: SEntry -> SEntry -> SEntry
mergeOldEntries (One,Zero,Just i,Nothing) (Zero,n,Nothing,mj) = (One,n,Just i,mj)
mergeOldEntries (One,Zero,Just i,Nothing) (_,n,_,mj) = (Many,n,Nothing,mj)

mkOldTable :: Ord a => [a] -> SymbolTable a -> SymbolTable a
mkOldTable xs m = foldl (\m (i,x) -> Map.insertWith mergeOldEntries x (freshOldEntry i) m) m (zip [0..] xs)

oneEntries (One,One,Just i,Just j) = True
oneEntries (_,_,_,_) = False

pass3 :: OA -> NA -> [SEntry] -> IO ()
pass3 oa na [] = return ()
pass3 oa na ((One,One,Just j,Just i):xs) = do
	writeArray na i (Left j)
	writeArray oa j (Left i)
	pass3 oa na xs

pass4 :: OA -> NA -> Int -> Int -> Int -> IO ()
pass4 oa na i maxi maxj = if i == maxi then return () else do
	nai <- readArray na i
	case nai of
		Left j@((< maxj) -> True) -> do
			nasucci <- readArray na (i+1)
			oasuccj <- readArray oa (j+1)
			case (nasucci,oasuccj) of
				(Right e,Right ((==e) -> True)) -> do
					writeArray na (i+1) (Left (j+1))
					writeArray oa (j+1) (Left (i+1))
				otherwise -> pass4 oa na (i+1) maxi maxj
		otherwise -> pass4 oa na (i+1) maxi maxj

pass5 :: OA -> NA -> Int -> IO ()
pass5 oa na i = if i == 0 then return () else do
	nai <- readArray na i
	case nai of
		Left j@((> 0) -> True) -> do
			napredi <- readArray na (i-1)
			oapredj <- readArray oa (j-1)
			case (napredi,oapredj) of
				(Right e,Right ((==e) -> True)) -> do
					writeArray na (i-1) (Left (j-1))
					writeArray oa (j-1) (Left (i-1))
				otherwise -> pass5 oa na (i-1)
		otherwise -> pass5 oa na (i-1)

-- * Creation of the delta

--deletes :: OA -> NA -> Int -> Int -> Int -> IO (ListDeltas a)
--deletes oa na j maxj delcount = if j > maxj then return [] else do
--	oaj <- readArray oa j
--	case oaj of
--		Left i -> do
--			Left nj <- readArray na i
--			writeArray na i $ Left (j-delcount)
--			deletes oa na (j+1) maxj delcount
--		Right _ -> do
--			ds <- deletes oa na (j+1) maxj (delcount+1)
--			return $ DelPos (j-delcount) : ds
--
--insertsAndReorders :: [a] -> OA -> NA -> Int -> Int -> Int -> IO (ListDeltas a,[(Int,Int)])
--insertsAndReorders n oa na i maxi inscount = if i > maxi then return ([],[]) else do
--	nai <- readArray na i
--	case nai of
--		Left j -> do
--			(inss,reos) <- insertsAndReorders n oa na (i+1) maxi inscount
--			return $ (inss,addReorder j (i-inscount) reos)
--		Right _ -> do
--			(inss,reos) <- insertsAndReorders n oa na (i+1) maxi (inscount+1)
--			return $ (InsPos i (n!!i) : inss,reos)
--
--addReorder :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
--addReorder j i reos = if j == i then reos else (j,i):reos

-- * Main procedure

-- alignment between positions in the first list and positions in the second list
-- positions start at 0
align :: Ord a => [a] -> [a] -> IO [(Int,Int)]
align o [] = return []
align [] n = return []
align o n = do
	let newTable = mkNewTable n
	let table = mkOldTable o newTable
	let maxi = max (length n - 1) 0
	let maxj = max (length o - 1) 0
	na <- newListArray (0,maxi) $ map (Right . fromJustNote "align" . (flip Map.lookup) table) n
	oa <- newListArray (0,maxj) $ map (Right . fromJustNote "align" . (flip Map.lookup) table) o
	pass3 oa na (Map.elems $ Map.filter oneEntries table)
	pass4 oa na 0 maxi maxj
	pass5 oa na maxi
	oalist <- getAssocs oa
	let deltas = foldr (\(i,e) l -> either (\x -> (i,x):l) (const l) e) [] oalist
	return deltas

-- returns a list of associations for all the elements in the first list, and a boolean stating whether all elements in the second list have a match
alignMaybe :: Ord a => [a] -> [a] -> IO ([(Int,Maybe Int)],Bool)
alignMaybe o [] = return (map (,Nothing) (take (length o - 1) [0..]),False)
alignMaybe [] n = return ([],False)
alignMaybe o n = do
	let newTable = mkNewTable n
	let table = mkOldTable o newTable
	let maxi = length n - 1
	let maxj = length o - 1
	na <- newListArray (0,maxi) $ map (Right . fromJustNote "alignMaybe" . (flip Map.lookup) table) n
	oa <- newListArray (0,maxj) $ map (Right . fromJustNote "alignMaybe" . (flip Map.lookup) table) o
	pass3 oa na (Map.elems $ Map.filter oneEntries table)
	pass4 oa na 0 maxi maxj
	pass5 oa na maxi
	oalist <- getAssocs oa
	let deltas = foldr (\(i,e) l -> either (\x -> (i,Just x):l) (\x -> (i,Nothing):l) e) [] oalist
	nalist <- getAssocs na
	let allHaveAssocs = and $ map (isLeft . snd) nalist
	return (deltas,allHaveAssocs)

-- returns an association list sorted according to the positions of the first list and with optional correspondents in a third list by looking up elements according to the associated positions in the second list
alignWithLookup :: Ord a => [a] -> [a] -> [b] -> IO ([(a,Maybe b)],Bool)
alignWithLookup xs ys oracle = alignMaybe xs ys >>= \(assocs,b) -> return (makeMaybeAssocs assocs xs oracle,b)
	where
	makeMaybeAssocs :: [(Int,Maybe Int)] -> [a] -> [b] -> [(a,Maybe b)]
	makeMaybeAssocs [] [] zs = []
	makeMaybeAssocs ((i,Nothing):ijs) (x:xs) zs = (x,Nothing) : makeMaybeAssocs ijs xs zs
	makeMaybeAssocs ((i,Just j):ijs) (x:xs) zs = (x,Just $ zs!!j) : makeMaybeAssocs ijs xs zs

--align :: Ord a => [a] -> [a] -> IO ([(Int,Int)],ListDeltas a)
--align o n = do
--	let newTable = mkNewTable n
--	let table = mkOldTable o newTable
--	let maxi = length n - 1
--	let maxj = length o - 1
--	na <- newListArray (0,maxi) $ map (Right . fromJust . (flip Map.lookup) table) n
--	oa <- newListArray (0,maxj) $ map (Right . fromJust . (flip Map.lookup) table) o
--	pass3 oa na (Map.elems $ Map.filter oneEntries table)
--	pass4 oa na 0 maxi maxj
--	pass5 oa na maxi
--	dels <- deletes oa na 0 maxj 0
--	(inss,reos) <- insertsAndReorders n oa na 0 maxi 0
--	oalist <- getAssocs oa
--	let deltas = foldr (\(i,e) l -> either (\x -> (i,x):l) (const l) e) [] oalist
--	return (deltas,dels ++ simplifyDelta [ReorderPos reos] ++ inss)




