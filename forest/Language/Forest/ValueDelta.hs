{-# LANGUAGE TypeOperators, ConstraintKinds, FunctionalDependencies, FlexibleInstances, TupleSections, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, Rank2Types, GADTs, ViewPatterns #-}

module Language.Forest.ValueDelta where

import Language.Forest.MetaData

import Control.Monad.Incremental hiding (get)
import Control.Monad
import Data.Maybe
import Data.List
--import Data.Functor.Identity
import Data.List.Split
import Language.Forest.FS.FSRep
import Data.Map (Map(..))
import qualified Data.Map as Map
import Language.Forest.FS.FSRep
import Data.WithClass.MData
import Language.Forest.BX

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

--type SValueDelta = SValueDeltaM Identity
--type SValueDeltaO fs = SValueDeltaM (ForestO fs)
--type SValueDeltaI fs = SValueDeltaM (ForestI fs)
--type SValueDeltaL fs l = SValueDeltaM (ForestL fs l)
--
---- a simple representation of value deltas that simply keeps track of empty updates
--data SValueDeltaM m v where
--	Id			:: SValueDeltaM m v
--	-- side-effecting stable operations
--	ModifyS		:: (v -> m ()) -> SValueDeltaM m v
--	OverwriteS	:: m () -> SValueDeltaM m v
--	--change the current value
--	Set			:: v -> SValueDeltaM m v
--	Modify		:: (v -> m v) -> SValueDeltaM m v
--	Overwrite	:: m v -> SValueDeltaM m v

data SValueDeltaKind = NoOp | Stable | NonStable

--valueDeltaKind :: SValueDeltaM m v -> SValueDeltaKind
--valueDeltaKind Id = NoOp
--valueDeltaKind (ModifyS _) = Stable
--valueDeltaKind (OverwriteS _) = Stable
--valueDeltaKind (Set _) = NonStable
--valueDeltaKind (Modify _) = NonStable
--valueDeltaKind (Overwrite _) = NonStable

andSValueDeltaKinds :: SValueDeltaKind -> SValueDeltaKind -> SValueDeltaKind
andSValueDeltaKinds NonStable _ = NonStable
andSValueDeltaKinds _ NonStable = NonStable
andSValueDeltaKinds Stable _ = Stable
andSValueDeltaKinds _ Stable = Stable
andSValueDeltaKinds NoOp NoOp = NoOp
--
--instance Show (SValueDeltaM m v) where
--	show Id = "Id"
--	show (Set v) = "Set"
--	show (ModifyS _) = "ModifyS"
--	show (OverwriteS _) = "OverwriteS"
--	show (Modify _) = "Modify"
--	show (Overwrite _) = "Overwrite"
--
---- we can only have a very permissive notion of equality
--instance Eq (SValueDeltaM m v) where
--	Id == Id = True
--	v1 == v2 = False
--
--isEmptySValueDelta :: Monad m => SValueDeltaM m a -> Bool
--isEmptySValueDelta Id = True
--isEmptySValueDelta _ = False
----
------ | done lazily
--applySValueDelta :: Monad m => SValueDeltaM m v -> v -> m v
--applySValueDelta Id v = return v
--applySValueDelta (Set v') v = return v'
--applySValueDelta (ModifyS f) v = f v >> return v
--applySValueDelta (OverwriteS m) v = m >> return v
--applySValueDelta (Modify f) v = f v
--applySValueDelta (Overwrite m) v = m
--
--applyVD :: SValueDelta v -> v -> v
--applyVD d v = runIdentity $ applySValueDelta d v
--
--applySValueDeltaM :: Monad m => SValueDeltaM m v -> v -> m (v,SValueDeltaM m v)
--applySValueDeltaM d v = do
--	(v',kind) <- applySValueDeltaKind d v
--	case kind of
--		NoOp -> return (v',Id)
--		Stable -> return (v',OverwriteS $ return ())
--		NonStable -> return (v',Set v')
--
----applySValueDeltaMay :: Monad m => String -> SValueDeltaM m v -> Maybe v -> m v
----applySValueDeltaMay note d mv = liftM fst $ applySValueDeltaMayM note d mv
--
----valueDeltaM :: Monad m => SValueDeltaM m v -> m v
----valueDeltaM d = applySValueDeltaMay "valueDeltaM" d Nothing
--
---- applies the delta and returns a new "dummy" delta that denotes the kind of update: either identity @Id@, stable @OverwriteS (return ())@ or change @Set v'@
----applySValueDeltaMayM :: Monad m => String -> SValueDeltaM m v -> Maybe v -> m (v,SValueDeltaM m v)
----applySValueDeltaMayM note d mv = do
----	(v',kind) <- applySValueDeltaMayKind note d mv
----	case kind of
----		NoOp -> return (v',Id)
----		Stable -> return (v',OverwriteS $ return ())
----		NonStable -> return (v',Set v')
----
------ applies the delta and returns a new "dummy" delta that denotes the kind of update: either identity @Id@, stable @OverwriteS (return ())@ or change @Set v'@
----applySValueDeltaMayKind :: Monad m => String -> SValueDeltaM m v -> Maybe v -> m (v,SValueDeltaKind)
----applySValueDeltaMayKind note Id (Just v) = return (v,NoOp)
----applySValueDeltaMayKind note Id Nothing = error $note++" applySValueDeltaMayM: Id"
----applySValueDeltaMayKind note (Set v') mv = return (v',NonStable)
----applySValueDeltaMayKind note (ModifyS f) mv = let v = maybe (error $note++" applySValueDeltaMayM: ModifyS") id mv in f v >> return (v,Stable)
----applySValueDeltaMayKind note (OverwriteS m) mv = let v = maybe (error $note++" applySValueDeltaMayM: OverwriteS") id mv in m >> return (v,Stable)
----applySValueDeltaMayKind note (Modify f) mv = let v = maybe (error $note++" applySValueDeltaMayM: Modify") id mv in f v >>= \v' -> return (v',NonStable)
----applySValueDeltaMayKind note (Overwrite m) mv = m >>= \v' -> return (v',NonStable)
--
---- applies the delta and returns a new "dummy" delta that denotes the kind of update: either identity @Id@, stable @OverwriteS (return ())@ or change @Set v'@
--applySValueDeltaKind :: Monad m => SValueDeltaM m v -> v -> m (v,SValueDeltaKind)
--applySValueDeltaKind Id v = return (v,NoOp)
--applySValueDeltaKind (Set v') v = return (v',NonStable)
--applySValueDeltaKind (ModifyS f) v = f v >> return (v,Stable)
--applySValueDeltaKind (OverwriteS m) v = m >> return (v,Stable)
--applySValueDeltaKind (Modify f) v = f v >>= \v' -> return (v',NonStable)
--applySValueDeltaKind (Overwrite m) v = m >>= \v' -> return (v',NonStable)

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

--isoSValueDelta :: Monad m => Iso a b -> SValueDeltaM m a -> SValueDeltaM m b
--isoSValueDelta (Iso to from) Id = Id
--isoSValueDelta (Iso to from) (Set v) = Set $ to v
--isoSValueDelta (Iso to from) (ModifyS f) = ModifyS $ f . from
--isoSValueDelta (Iso to from) (OverwriteS m) = OverwriteS m
--isoSValueDelta (Iso to from) (Modify f) = Modify $ \b -> liftM to $ f (from b)
--isoSValueDelta (Iso to from) (Overwrite m) = Overwrite $ liftM to m
--
--inferSValueDelta :: Monad m => Eq a => a -> a -> SValueDeltaM m a
--inferSValueDelta old new = if old==new then Id else Set new
--

(>:>) :: NSValueDelta a -> NSValueDelta a -> NSValueDelta a
StableVD Id >:> d2 = d2
d1 >:> StableVD Id = d1
StableVD Delta >:> StableVD Delta = StableVD Delta
StableVD Delta >:> Modify g = Modify g
Modify f >:> StableVD Delta = Modify f
Modify f >:> Modify g = Modify (g . f)

isEmptyNSValueDelta :: NSValueDelta a -> Bool
isEmptyNSValueDelta (StableVD Id) = True
isEmptyNSValueDelta _ = False

---- composition is monadic because in some cases it is better to perform the changes eagerly, returning a constant delta @Set v'@, rather than lazily
--(>:>) :: Monad m => SValueDeltaM m a -> SValueDeltaM m a -> m (SValueDeltaM m a)
--Id >:> d = return d
--d >:> Id = return d
--(Set v) >:> (Set v') = return $ Set v'
--(Set v) >:> (ModifyS g) = g v >> return (Set v)
--(Set v) >:> (OverwriteS n) = n >> return (Set v)
--(Set v) >:> (Modify g) = return $ Overwrite $ g v
--(Set v) >:> (Overwrite n) = return $ Overwrite n
--(ModifyS f) >:> (Set v') = return $ Modify $ \v -> f v >> return v'
--(ModifyS f) >:> (ModifyS g) = return $ ModifyS $ \v -> f v >> g v
--(ModifyS f) >:> (OverwriteS n) = return $ ModifyS $ \v -> f v >> n
--(ModifyS f) >:> (Modify g) = return $ Modify $ \v -> f v >> g v
--(ModifyS f) >:> (Overwrite n) = return $ Modify $ \v -> f v >> n
--(OverwriteS m) >:> (Set v') = m >> return (Set v')
--(OverwriteS m) >:> (ModifyS g) = return $ ModifyS $ \v -> m >> g v
--(OverwriteS m) >:> (OverwriteS n) = return $ OverwriteS $ m >> n
--(OverwriteS m) >:> (Modify g) = return $ Modify $ \v -> m >> g v
--(OverwriteS m) >:> (Overwrite n) = return $ Overwrite $ m >> n
--(Modify f) >:> (Set v') = return $ Modify $ \v -> f v >> return v'
--(Modify f) >:> (ModifyS g) = return $ Modify $ \v -> f v >>= \v' -> g v' >> return v'
--(Modify f) >:> (OverwriteS n) = return $ Modify $ \v -> f v >>= \v' -> n >> return v'
--(Modify f) >:> (Modify g) = return $ Modify $ \v -> f v >>= g
--(Modify f) >:> (Overwrite n) = return $ Modify $ \v -> f v >> n
--(Overwrite m) >:> (Set v') = m >> return (Set v')
--(Overwrite m) >:> (ModifyS g) = m >>= \v' -> g v' >> return (Set v')
--(Overwrite m) >:> (OverwriteS n) = m >>= \v' -> n >> return (Set v')
--(Overwrite m) >:> (Modify g) = return $ Overwrite $ m >>= g
--(Overwrite m) >:> (Overwrite n) = return $ Overwrite $ m >> n
--
--
--lensSValueDelta :: Monad m => Lens s v -> SValueDeltaM m v -> SValueDeltaM m s
--lensSValueDelta l Id = Id
--lensSValueDelta l (Set v) = Modify $ \s -> return $ put l s v
--lensSValueDelta l (ModifyS f) = ModifyS $ \s -> f $ get l s
--lensSValueDelta l (OverwriteS m) = OverwriteS m
--lensSValueDelta l (Modify f) = Modify $ \s -> liftM (put l s) $ f $ get l s
--lensSValueDelta l (Overwrite m) = Modify $ \s -> liftM (put l s) m
--
--maybeSValueDeltaO :: (MData NoCtx (ForestO fs) a,FSRep fs) => SValueDeltaO fs a -> SValueDeltaO fs (Maybe a)
--maybeSValueDeltaO Id = Id
--maybeSValueDeltaO (Set v) = Set $ Just v
--maybeSValueDeltaO (ModifyS f) = ModifyS $ \mb -> case mb of
----	Nothing -> mdefault >>= f
--	Just v -> f v
--maybeSValueDeltaO (OverwriteS m) = OverwriteS m
--maybeSValueDeltaO (Modify f) = Modify $ \mb -> case mb of
----	Nothing -> liftM Just $ f =<< mdefault
--	Just v -> liftM Just $ f v
--maybeSValueDeltaO (Overwrite m) = Overwrite $ liftM Just m
--
--fsthunkSValueDelta :: (Eq a,ForestInput fs FSThunk l) => ForestFSThunk fs l a -> [FSTree fs] -> SValueDeltaL fs l a -> SValueDeltaO fs (ForestFSThunk fs l a)
--fsthunkSValueDelta t ts Id = Id
--fsthunkSValueDelta t ts (Set v) = OverwriteS $ fsset t v 
--fsthunkSValueDelta t ts (ModifyS f) = OverwriteS $ outside $ fsforce t >>= f
--fsthunkSValueDelta t ts (OverwriteS m) = OverwriteS $ outside m
--fsthunkSValueDelta t ts (Modify f) = debug "fsthunkmodify" $ OverwriteS $ fsmodify ts t f
--fsthunkSValueDelta t ts (Overwrite m) = debug "fsthunkoverwrite" $ OverwriteS $ fsoverwrite ts t m
--
--fsthunkSValueDeltaM :: (Eq a,ForestInput fs FSThunk l) => ForestFSThunk fs l a -> [FSTree fs] -> SValueDeltaL fs l a -> ForestO fs (SValueDeltaL fs l (ForestFSThunk fs l a))
--fsthunkSValueDeltaM t ts Id = Id
--fsthunkSValueDeltaM t ts (Set v) = fsset t v >> return OverwriteS $ return ()
--fsthunkSValueDeltaM t ts (ModifyS f) = return $ OverwriteS $ fsforce t >>= f
--fsthunkSValueDeltaM t ts (OverwriteS m) = OverwriteS $ m
--fsthunkSValueDeltaM t ts (Modify f) = debug "fsthunkmodify" $ fsmodify ts t f >> return $ OverwriteS $ return ()
--fsthunkSValueDeltaM t ts (Overwrite m) = debug "fsthunkoverwrite" $ fsoverwrite ts t m >> return $ OverwriteS $ return ()
--
--rethunkSValueDeltaMay :: (Eq a,ForestInput fs FSThunk l) => [FSTree fs] -> Maybe (ForestFSThunk fs l a) -> ForestL fs l a -> SValueDeltaO fs (ForestFSThunk fs l a)
--rethunkSValueDeltaMay trees Nothing m = Overwrite $ outside $ fsthunk trees m
--rethunkSValueDeltaMay trees (Just t) m = OverwriteS $ fsoverwrite trees t m
--
--rethunkSValueDelta :: (Eq a,ForestInput fs FSThunk l) => [FSTree fs] -> ForestFSThunk fs l a -> ForestFSThunk fs l a -> SValueDeltaO fs (ForestFSThunk fs l a)
--rethunkSValueDelta trees t t' = if eqFSThunk t t'
--	then OverwriteS $ return ()
--	else OverwriteS $ fsoverwrite trees t $ fsforce t'
--
----rethunkSValueDeltaEager :: (Eq a,FSRep fs) => Maybe (FSThunk fs a) -> a -> SValueDeltaO fs (FSThunk fs a)
----rethunkSValueDeltaEager Nothing v = Overwrite $ fsref v
----rethunkSValueDeltaEager (Just t) v = OverwriteS $ fsset t v
--
--modifyFSThunkWithSValueDelta :: (Eq a,ForestInput fs FSThunk l) => [FSTree fs] -> ForestFSThunk fs l a -> SValueDeltaL fs l a -> ForestO fs ()
--modifyFSThunkWithSValueDelta tree t Id = return ()
--modifyFSThunkWithSValueDelta tree t (Set v) = fsset t v
----modifyFSThunkWithSValueDelta tree t (ModifyS f) = 
----modifyFSThunkWithSValueDelta tree t (OverwriteS m) = 
----modifyFSThunkWithSValueDelta tree t (Modify f) = fsmodify
--modifyFSThunkWithSValueDelta tree t (Overwrite m) = fsoverwrite tree t m
--
---- change the first element of a pair
--fstSValueDelta :: Monad m => SValueDeltaM m a -> SValueDeltaM m (a,b)
--fstSValueDelta Id = Id
--fstSValueDelta (Set a') = Modify $ \(a,b) -> return (a',b)
--fstSValueDelta (ModifyS f) = ModifyS $ f . fst
--fstSValueDelta (OverwriteS m) = OverwriteS m
--fstSValueDelta (Modify f) = Modify $ \(a,b) -> f a >>= \a' -> return (a',b)
--fstSValueDelta (Overwrite m) = Modify $ \(a,b) -> m >>= \a' -> return (a',b)
--
---- change the second element of a pair
--sndSValueDelta :: Monad m => SValueDeltaM m b -> SValueDeltaM m (a,b)
--sndSValueDelta Id = Id
--sndSValueDelta (Set b') = Modify $ \(a,b) -> return (a,b')
--sndSValueDelta (ModifyS f) = ModifyS $ f . snd
--sndSValueDelta (OverwriteS m) = OverwriteS m
--sndSValueDelta (Modify f) = Modify $ \(a,b) -> f b >>= \b' -> return (a,b')
--sndSValueDelta (Overwrite m) = Modify $ \(a,b) -> m >>= \b' -> return (a,b')
--
--ldepSValueDeltaS :: Monad m => (b -> m ()) -> SValueDeltaM m b -> m (SValueDeltaM m (a,b))
--ldepSValueDeltaS f Id = return Id
--ldepSValueDeltaS f (Set b') = return $ Modify $ \(a,b) -> f b' >> return (a,b')
--ldepSValueDeltaS f (ModifyS g) = return $ ModifyS $ \(a,b) -> g b >> f b
--ldepSValueDeltaS f (OverwriteS m) = return $ ModifyS $ \(a,b) -> m >> f b
--ldepSValueDeltaS f (Modify g) = return $ Modify $ \(a,b) -> g b >>= \b' -> f b' >> return (a,b')
--ldepSValueDeltaS f (Overwrite m) = return $ Modify $ \(a,b) -> m >>= \b' -> f b' >> return (a,b')
--
--ldepSValueDeltaSOutside :: (a,b) -> (b -> Outside inc r m ()) -> SValueDeltaM (Inside inc r m) b -> Outside inc r m (SValueDeltaM (Inside inc r m) (a,b))
--ldepSValueDeltaSOutside (a,b) f Id = return Id
----ldepSValueDeltaSOutside (a,b) f (Set b') = f b >> return $ Modify $ \(a,b) -> f b' >> return (a,b')
--ldepSValueDeltaSOutside (a,b) f (ModifyS g) = g b >> f b >> return $ OverwriteS $ return ()
--ldepSValueDeltaSOutside (a,b) f (OverwriteS m) = m >> f b >> return $ OverwriteS $ return ()
----ldepSValueDeltaSOutside (a,b) f (Modify g) = g b return $ Modify $ \(a,b) -> g b >>= \b' -> f b' >> return (a,b')
----ldepSValueDeltaSOutside (a,b) f (Overwrite m) = return $ Modify $ \(a,b) -> m >>= \b' -> f b' >> return (a,b')
--
---- as long as something changes (even if stable changes underneath), we recompute the dependency
---- this is necessary because FSThunks do not have dependencies
---- assumes that the constraint was already valid, so it returns Id if nothing changed
--ldepSValueDelta :: Monad m => (b -> m a) -> SValueDeltaM m b -> m (SValueDeltaM m (a,b))
--ldepSValueDelta f Id = return Id
--ldepSValueDelta f d = enforceldepSValueDelta f d
--
---- always enforce the dependency
--enforceldepSValueDelta :: Monad m => (b -> m a) -> SValueDeltaM m b -> m (SValueDeltaM m (a,b))
--enforceldepSValueDelta f Id = return $ Modify $ \(a,b) -> f b >>= \a' -> return (a',b)
--enforceldepSValueDelta f (Set b') = f b' >>= \a' -> return (Set (a',b'))
--enforceldepSValueDelta f (ModifyS g) = return $ Modify $ \(a,b) -> g b >> f b >>= \a' -> return (a',b)
--enforceldepSValueDelta f (OverwriteS m) = return $ Modify $ \(a,b) -> m >> f b >>= \a' -> return (a',b)
--enforceldepSValueDelta f (Modify g) = return $ Modify $ \(a,b) -> g b >>= \b' -> f b' >>= \a' -> return (a',b')
--enforceldepSValueDelta f (Overwrite m) = return $ Overwrite $ m >>= \b' -> f b' >>= \a' -> return (a',b')
--
--enforcerdepSValueDelta :: Monad m => (a -> m b) -> SValueDeltaM m a -> m (SValueDeltaM m (a,b))
--enforcerdepSValueDelta f Id = return $ Modify $ \(a,b) -> f a >>= \b' -> return (a,b')
--enforcerdepSValueDelta f (Set a') = f a' >>= \b' -> return (Set (a',b'))
--enforcerdepSValueDelta f (ModifyS g) = return $ Modify $ \(a,b) -> g a >> f a >>= \b' -> return (a,b')
--enforcerdepSValueDelta f (OverwriteS m) = return $ Modify $ \(a,b) -> m >> f a >>= \b' -> return (a,b')
--enforcerdepSValueDelta f (Modify g) = return $ Modify $ \(a,b) -> g a >>= \a' -> f a' >>= \b' -> return (a',b')
--enforcerdepSValueDelta f (Overwrite m) = return $ Overwrite $ m >>= \a' -> f a' >>= \b' -> return (a',b')

-- class to traverse to the top-level thunk of a Forest representation, by dropping newtype tags
class ForestRep rep thunk | rep -> thunk where
	iso_rep_thunk :: Iso rep thunk

instance ForestRep (ForestFSThunk fs l rep) (ForestFSThunk fs l rep) where
	iso_rep_thunk = Iso id id


