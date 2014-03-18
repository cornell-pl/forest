{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-type-defaults #-} -- See Note [Type defaults] in Syntax.hs

module WTree where

import Data.Generics.TH
import Control.Monad.State

data WTree a w = Leaf a
               | Fork (WTree a w) (WTree a w)
               | WithWeight (WTree a w) w
       deriving (Show)

----------------

rmWeight :: WTree Int Int -> WTree Int Int
rmWeight (WithWeight t _) = t
rmWeight t                = t

nrInt :: Int -> State Int Int
nrInt _ = do
     i <- get
     put (i+1)
     return i

----------------

id_WTree :: WTree a w -> WTree a w
id_WTree = $(everywhere (const [|id|]) [t|WTree Int Int|])

const_WTree :: Int -> WTree Int Int -> WTree Int Int
const_WTree c = $(everywhere (const [|id|] `extE` (eqType [t|Int|], [|const c|])) [t|WTree Int Int|])

rmWeights :: WTree Int Int -> WTree Int Int
rmWeights = $(everywhere (const [|id|] `extN` 'rmWeight) [t|WTree Int Int|])

count_WTree_All :: WTree Int Int -> Int
count_WTree_All = $(everything [|(+)|] (const [|const 1|]) [t|WTree Int Int|])

selectInt :: WTree Int Int -> [Int]
selectInt = $(everything [|(++)|] (mkQ [|[]|] 'f) [t|WTree Int Int|])
 where f :: Int -> [Int]
       f i = [i]

selectInt_Acc :: WTree Int Int -> [Int]
selectInt_Acc x = $(everything [|(.)|] (mkQ [|id|] 'f) [t|WTree Int Int|]) x []
 where f :: Int -> [Int] -> [Int]
       f i = (i:)

selectInt_Acc2 :: WTree Int Int -> [Int]
selectInt_Acc2 x = $(everythingAccL (mkQ [|id|] 'f) [t|WTree Int Int|]) x []
 where f :: Int -> [Int] -> [Int]
       f i = (i:)

selectInt_For :: WTree Int Int -> [Int]
selectInt_For x = $(everythingForR 'f [t|WTree Int Int|]) x []
 where
 f :: Int -> [Int] -> [Int]
 f = (:)

number_WTree :: WTree Int Int -> WTree Int Int
number_WTree b = evalState ($(everywhereM (mkM 'nrInt) [t|(WTree Int Int)|]) b) 0

number_WTree_For :: WTree Int Int -> WTree Int Int
number_WTree_For b = evalState ($(everywhereForM 'nrInt [t|(WTree Int Int)|]) b) 0
