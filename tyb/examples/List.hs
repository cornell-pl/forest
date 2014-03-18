{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-type-defaults #-} -- See Note [Type defaults] in Syntax.hs

module List where

import Data.Generics.TH
import Control.Monad.State

----------------------------------------
-- List
----------------------------------------

-- This is slower than "Prelude.sum" because that uses accumulator
-- style due to the foldl in it.
sum :: [Int] -> Int
sum = $(everything [|(+)|] (mkQ [|0|] 'f) [t|[Int]|]) where
  f :: Int -> Int
  f = id

-- As fast as sum
sum_acc :: [Int] -> Int
sum_acc xs = $(everythingAccR (mkQ [|id|] 'f) [t|[Int]|]) xs 0 where
  f :: Int -> (Int -> Int)
  f = (+)

-- Or by reifying the types from the function name (and using a strict accumulator):
sum_for :: [Int] -> Int
sum_for xs = $(everythingForL' 'f [t|[Int]|]) xs 0 where
  f :: Int -> Int -> Int
  f = (+)

map :: (Int -> Int) -> [Int] -> [Int]
map f = $(everywhere (mkT 'f) [t|[Int]|])

shift :: [Int] -> [Int]
shift xs = evalState ($(everywhereM (mkM 'go) [t|[Int]|]) xs) 0
 where
 go :: Int -> State Int Int
 go i = get >>= \v -> put i >> return v
