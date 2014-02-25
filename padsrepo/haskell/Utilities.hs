-- Author: Michel Blockelet (supervised by David Walker)
-- Institution: Princeton University
-- Date: July 08

module Utilities where

import Data.Typeable

import Types



{--- Datatypes handling Functions ---}
{- mkPDlist :: computes the final PDErrorCode from a list of PD -}
getPDErrorCode :: PD -> PDErrorCode
getPDErrorCode (PDbase pdec) = pdec
getPDErrorCode (PDlist pdec l) = pdec
getPDErrorCode (PDdata pdec pd) = pdec

mkPDlist :: [PD] -> PD
mkPDlist [] = PDlist Good []
mkPDlist (x:xs) = let npd = (getPDErrorCode x) in
                  case mkPDlist xs of
                    (PDlist pd _) -> case pd of
                                       Good -> case npd of
                                                 Good -> PDlist Good (x:xs)
                                                 Bad _ nr -> PDlist (Nested nr) (x:xs)
                                                 Nested nr -> PDlist (Nested nr) (x:xs)
                                       Nested r -> case npd of
                                                     Good -> PDlist (Nested r) (x:xs)
                                                     Bad _ nr -> PDlist (Nested (and [r,nr])) (x:xs)
                                                     Nested nr -> PDlist (Nested (and [r,nr])) (x:xs)


{- int_to_float :: converts from Int to Float (and force the type) -}
int_to_float :: Int -> Float
int_to_float = fromIntegral



{--- Helper Functions ---}
{- Brancher :: Takes a list of Branches and gives the element (for Pdata) -}
brancher :: [Branch a] -> a -> BranchedValue
brancher [] = \x -> error "brancher :: no branch available"
brancher ((Branch ((_, _, f), p)):r) =
  \x -> case (f x) of Nothing -> brancher r x
                      Just pelem -> BranchedValue (p, pelem)


{- map_and_take :: ~= map (takeWhile()), if it complies it maps, if it doesn't it keeps in the second list -}
map_and_take :: (a -> b) -> (b -> Bool) -> [a] -> ([b], [a])
map_and_take f t   []   = ([], [])
map_and_take f t (x:xs) = let y = (f x) in
                          if (t y)
                            then let (l1, l2) = (map_and_take f t xs) in (y:l1, l2)
                            else ([], x:xs)


{- takeOut :: if r is a prefix of s, take it out -}
takeOut :: Eq a => [a] -> [a] -> (PDErrorCode, [a])
takeOut [] s = (Good, s)
takeOut r [] = (Bad "takeOut :: no more elements" False, [])
takeOut (x:r) (y:s) = if x == y
                        then takeOut r s
                        else (Bad "takeOut :: x != y" False, s)


{- splitAtList :: if s is somewhere inside xs, return the list of elements before s and of those after s -}
splitAtList :: Eq a => [a] -> [a] -> Int -> (PDErrorCode, [a], [a])
splitAtList s [] n = (Bad "splitAtList :: no more elements !" False, [], [])
splitAtList s (x:xs) n = if n > 0
                           then case (takeOut s (x:xs)) of
                                  (Good, ns) -> (Good, [], ns)
                                  (Bad _ _, _) -> let (pd, la, lb) = (splitAtList s xs (n-1)) in (pd, x:la, lb)
                           else (Bad "splitAtList :: not found within scan limit" False, [], (x:xs))


{- unfold :: some kind of fold with elements kept etc. -}
unfold :: (a -> (a,b)) -> a -> [(a,b)]
unfold f x = let (nx, y) = (f x) in
             let l = (unfold f nx) in (nx,y):l


{- takeLast :: take the nth element and keep a list of the elements between -}
takeLast :: Int -> [(a,b)] -> (a,[b])
takeLast 1 ((x,y):xs) = (x,[y])
takeLast n ((x,y):xs) = let (fx, ly) = (takeLast (n-1) xs) in (fx, y:ly)


{- splitoae :: splits odds and evens -}
splitoae :: [a] -> ([a], [a])
splitoae (x:y:xs) = let (l1, l2) = splitoae xs in (x:l1, y:l2)


{- takeEvery :: make the list of every nth element (with offset r) -}
takeEvery :: Int -> [a] -> Int -> [a]
takeEvery n (x:xs) 0 = x:(takeEvery n xs (n-1))
takeEvery n (x:xs) r = takeEvery n xs (r-1)


{- spanAll :: like span but tests the whole list -}
spanAll :: (a -> Bool) -> [a] -> ([a], [a])
spanAll f [] = ([], [])
spanAll f (x:xs) = let (lok, lnok) = spanAll f xs in if (f x) then (x:lok, lnok) else (lok, x:lnok)
