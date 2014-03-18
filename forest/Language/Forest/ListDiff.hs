{-# LANGUAGE ViewPatterns #-}

-- | A simple linear-time list alignment algorithm presented in http://dl.acm.org/citation.cfm?id=359467
-- It performs bad when elements are duplicated, but in forest we align (old and new) filenames listed in a directory, that are supposed to be unique.

module Language.Forest.ListDiff where
	
import Language.Forest.Delta
import Data.List
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Maybe
import Data.Array.MArray
import Data.Array.IO

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

deletes :: OA -> NA -> Int -> Int -> Int -> IO (ListDeltas a)
deletes oa na j maxj delcount = if j > maxj then return [] else do
	oaj <- readArray oa j
	case oaj of
		Left i -> do
			Left nj <- readArray na i
			writeArray na i $ Left (j-delcount)
			deletes oa na (j+1) maxj delcount
		Right _ -> do
			ds <- deletes oa na (j+1) maxj (delcount+1)
			return $ DelPos (j-delcount) : ds

insertsAndReorders :: [a] -> OA -> NA -> Int -> Int -> Int -> IO (ListDeltas a,[(Int,Int)])
insertsAndReorders n oa na i maxi inscount = if i > maxi then return ([],[]) else do
	nai <- readArray na i
	case nai of
		Left j -> do
			(inss,reos) <- insertsAndReorders n oa na (i+1) maxi inscount
			return $ (inss,addReorder j (i-inscount) reos)
		Right _ -> do
			(inss,reos) <- insertsAndReorders n oa na (i+1) maxi (inscount+1)
			return $ (InsPos i (n!!i) : inss,reos)

addReorder :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
addReorder j i reos = if j == i then reos else (j,i):reos

-- * Main procedure

align :: Ord a => [a] -> [a] -> IO ([(Int,Int)],ListDeltas a)
align o n = do
	let newTable = mkNewTable n
	let table = mkOldTable o newTable
	let maxi = length n - 1
	let maxj = length o - 1
	na <- newListArray (0,maxi) $ map (Right . fromJust . (flip Map.lookup) table) n
	oa <- newListArray (0,maxj) $ map (Right . fromJust . (flip Map.lookup) table) o
	pass3 oa na (Map.elems $ Map.filter oneEntries table)
	pass4 oa na 0 maxi maxj
	pass5 oa na maxi
	dels <- deletes oa na 0 maxj 0
	(inss,reos) <- insertsAndReorders n oa na 0 maxi 0
	oalist <- getAssocs oa
	let deltas = foldr (\(i,e) l -> either (\x -> (i,x):l) (const l) e) [] oalist
	return (deltas,dels ++ simplifyDelta [ReorderPos reos] ++ inss)




