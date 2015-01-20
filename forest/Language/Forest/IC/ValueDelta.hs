{-# LANGUAGE TypeOperators, ConstraintKinds, FunctionalDependencies, FlexibleInstances, TupleSections, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, Rank2Types, GADTs, ViewPatterns #-}

module Language.Forest.IC.ValueDelta where

import Language.Forest.IC.ICRep
import Language.Forest.Pure.MetaData hiding (Forest_md(..))

import Control.Monad.Incremental hiding (get)
import Control.Monad
import Data.Maybe
import Data.List
import Data.List.Split
import Language.Forest.FS.FSRep
import Data.Map (Map(..))
import qualified Data.Map as Map
import Language.Forest.FS.FSRep
import Data.WithClass.MData
import Language.Forest.IC.BX

class DeltaClass d where
	isEmptyDelta :: d v -> Bool

instance DeltaClass SValueDelta where
	isEmptyDelta = isEmptySValueDelta
instance DeltaClass NSValueDelta where
	isEmptyDelta = isEmptyNSValueDelta

-- stable deltas
data SValueDelta v where
	Id :: SValueDelta v
	Delta :: SValueDelta v

-- non-stable deltas
data NSValueDelta v where
	StableVD :: SValueDelta v -> NSValueDelta v
	Modify :: (v -> v) -> NSValueDelta v

instance Show (SValueDelta v) where
	show Id = "Id"
	show Delta = "Delta"

instance Show (NSValueDelta v) where
	show (StableVD d) = "(StableVD " ++ show d ++ ")"
	show (Modify f) = "(Modify _)"

maybeNSValueDelta :: NSValueDelta a -> NSValueDelta (Maybe a)
maybeNSValueDelta (StableVD d) = StableVD $ mapSValueDelta d
maybeNSValueDelta (Modify f) = Modify $ \mb -> case mb of
	Nothing -> Nothing
	Just a -> Just $ f a

mapSValueDelta :: SValueDelta a -> SValueDelta b
mapSValueDelta Id = Id
mapSValueDelta Delta = Delta

isEmptySValueDelta :: SValueDelta v -> Bool
isEmptySValueDelta Id = True
isEmptySValueDelta _ = False

applySValueDelta :: SValueDelta v -> v -> v
applySValueDelta Id v = v
applySValueDelta Delta v = v

applyNSValueDelta :: NSValueDelta v -> v -> v
applyNSValueDelta (StableVD d) v = applySValueDelta d v
applyNSValueDelta (Modify f) v = f v

nonstableValueDelta :: SValueDelta v -> NSValueDelta v
nonstableValueDelta = StableVD

-- evidence that stable value deltas do not depend on their type
liftSValueDelta :: SValueDelta a -> SValueDelta b
liftSValueDelta Id = Id
liftSValueDelta Delta = Delta

data SValueDeltaKind = NoOp | Stable | NonStable

andSValueDeltaKinds :: SValueDeltaKind -> SValueDeltaKind -> SValueDeltaKind
andSValueDeltaKinds NonStable _ = NonStable
andSValueDeltaKinds _ NonStable = NonStable
andSValueDeltaKinds Stable _ = Stable
andSValueDeltaKinds _ Stable = Stable
andSValueDeltaKinds NoOp NoOp = NoOp

valueDeltaKind :: SValueDelta v -> SValueDeltaKind
valueDeltaKind Id = NoOp
valueDeltaKind Delta = Stable

mergeCompoundSValueDeltas :: [(Maybe (k,v),SValueDeltaKind)] -> ([(k,v)],SValueDeltaKind)
mergeCompoundSValueDeltas [] = ([],NoOp)
mergeCompoundSValueDeltas ((Nothing,NonStable):xs) = let (ds,ks) = mergeCompoundSValueDeltas xs in (ds,NonStable)
mergeCompoundSValueDeltas ((Just d,k):xs) = let (ds,ks) = mergeCompoundSValueDeltas xs in (d:ds,k `andSValueDeltaKinds` ks)

makeSValueDelta :: Bool -> SValueDelta v
makeSValueDelta True = Id
makeSValueDelta False = Delta

prodSValueDelta :: SValueDelta a -> SValueDelta b -> SValueDelta (a,b)
prodSValueDelta Id Id = Id
prodSValueDelta Id Delta = Delta
prodSValueDelta Delta Id = Delta
prodSValueDelta Delta Delta = Delta

timesSValueDelta :: SValueDelta a -> SValueDelta b -> SValueDelta (a :*: b)
timesSValueDelta Id Id = Id
timesSValueDelta Id Delta = Delta
timesSValueDelta Delta Id = Delta
timesSValueDelta Delta Delta = Delta


(>::>) :: NSValueDelta a -> NSValueDelta a -> NSValueDelta a
StableVD Id >::> d2 = d2
d1 >::> StableVD Id = d1
StableVD Delta >::> StableVD Delta = StableVD Delta
StableVD Delta >::> Modify g = Modify g
Modify f >::> StableVD Delta = Modify f
Modify f >::> Modify g = Modify (g . f)

(>:>) :: SValueDelta a -> SValueDelta a -> SValueDelta a
Id >:> d2 = d2
d1 >:> Id = d1
Delta >:> Delta = Delta

isEmptyNSValueDelta :: NSValueDelta a -> Bool
isEmptyNSValueDelta (StableVD Id) = True
isEmptyNSValueDelta _ = False




