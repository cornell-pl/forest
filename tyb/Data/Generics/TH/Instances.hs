{-# LANGUAGE CPP, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -W -Wall -fno-warn-orphans #-}
module Data.Generics.TH.Instances({- only class instances are exported -}) where

import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Language.Haskell.TH.Syntax hiding (lift)

--------------------------------------------------------------------------------
-- Instance of Ord that are useful but missing from Language.Haskell.TH
--------------------------------------------------------------------------------

deriving instance Ord Exp
deriving instance Ord Dec
deriving instance Ord Type
deriving instance Ord Pat

deriving instance Ord Body
deriving instance Ord Callconv
deriving instance Ord Clause
deriving instance Ord Con
deriving instance Ord FamFlavour
deriving instance Ord Foreign
deriving instance Ord FunDep
deriving instance Ord Guard
deriving instance Ord Inline
-- Removed in GHC 7.6.x
-- deriving instance Ord InlineSpec
-- Typed to 'Type'.
-- deriving instance Ord Kind
deriving instance Ord Lit
deriving instance Ord Match
deriving instance Ord Pragma
deriving instance Ord Pred
deriving instance Ord Range
deriving instance Ord Safety
deriving instance Ord Stmt
deriving instance Ord Strict
deriving instance Ord TyVarBndr
deriving instance Ord RuleBndr
deriving instance Ord TyLit
deriving instance Ord Phases
deriving instance Ord RuleMatch
deriving instance Ord FixityDirection
deriving instance Ord Fixity

--------------------------------------------------------------------------------
-- Quasi instances for monad transformers
--------------------------------------------------------------------------------
instance (Quasi m, Error e) => Quasi (ErrorT e m) where
  qNewName s = lift $ qNewName s
  qReport b s = lift $ qReport b s
  qRecover m1 m2 = ErrorT $ runErrorT m1 `qRecover` runErrorT m2
  qReify n = lift $ qReify n
#if __GLASGOW_HASKELL__ >= 700 && __GLASGOW_HASKELL__ < 704
  qClassInstances n ts = lift $ qClassInstances n ts
#endif
  qLocation = lift $ qLocation
  qRunIO m = lift $ qRunIO m

instance (Quasi m) => Quasi (ListT m) where
  qNewName s = lift $ qNewName s
  qReport b s = lift $ qReport b s
  qRecover m1 m2 = Control.Monad.List.ListT $ runListT m1 `qRecover` runListT m2
  qReify n = lift $ qReify n
#if __GLASGOW_HASKELL__ >= 700 && __GLASGOW_HASKELL__ < 704
  qClassInstances n ts = lift $ qClassInstances n ts
#endif
  qLocation = lift $ qLocation
  qRunIO m = lift $ qRunIO m

instance (Quasi m) => Quasi (ReaderT r m) where
  qNewName s = lift $ qNewName s
  qReport b s = lift $ qReport b s
  qRecover m1 m2 = ReaderT $ \r -> runReaderT m1 r `qRecover` runReaderT m2 r
  qReify n = lift $ qReify n
#if __GLASGOW_HASKELL__ >= 700 && __GLASGOW_HASKELL__ < 704
  qClassInstances n ts = lift $ qClassInstances n ts
#endif
  qLocation = lift $ qLocation
  qRunIO m = lift $ qRunIO m

instance (Quasi m) => Quasi (StateT s m) where
  qNewName s = lift $ qNewName s
  qReport b s = lift $ qReport b s
  qRecover m1 m2 = StateT $ \s -> runStateT m1 s `qRecover` runStateT m2 s
  qReify n = lift $ qReify n
#if __GLASGOW_HASKELL__ >= 700 && __GLASGOW_HASKELL__ < 704
  qClassInstances n ts = lift $ qClassInstances n ts
#endif
  qLocation = lift $ qLocation
  qRunIO m = lift $ qRunIO m

instance (Quasi m, Monoid w) => Quasi (WriterT w m) where
  qNewName s = lift $ qNewName s
  qReport b s = lift $ qReport b s
  qRecover m1 m2 = WriterT $ runWriterT m1 `qRecover` runWriterT m2
  qReify n = lift $ qReify n
#if __GLASGOW_HASKELL__ >= 700 && __GLASGOW_HASKELL__ < 704
  qClassInstances n ts = lift $ qClassInstances n ts
#endif
  qLocation = lift $ qLocation
  qRunIO m = lift $ qRunIO m

-- TODO: other possible monad instances
-- ((->) r)
-- Maybe
-- IO

instance (Quasi m) => Quasi (MaybeT m) where
  qNewName s = lift $ qNewName s
  qReport b s = lift $ qReport b s
  qRecover m1 m2 = MaybeT $ runMaybeT m1 `qRecover` runMaybeT m2
  qReify n = lift $ qReify n
#if __GLASGOW_HASKELL__ >= 700 && __GLASGOW_HASKELL__ < 704
  qClassInstances n ts = lift $ qClassInstances n ts
#endif
  qLocation = lift $ qLocation
  qRunIO m = lift $ qRunIO m
