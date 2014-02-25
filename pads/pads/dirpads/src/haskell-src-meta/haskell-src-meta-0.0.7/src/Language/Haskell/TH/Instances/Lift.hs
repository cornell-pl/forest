{-# LANGUAGE TemplateHaskell, CPP, MagicHash, TypeSynonymInstances, StandaloneDeriving #-}

{- |
  Module      :  Language.Haskell.TH.Instances.Lift
  Copyright   :  (c) Matt Morrow 2008
  License     :  BSD3
  Maintainer  :  Matt Morrow <mjm2002@gmail.com>
  Stability   :  experimental
  Portability :  portable (template-haskell)
-}

module Language.Haskell.TH.Instances.Lift () where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr
import Data.List(intercalate)
import GHC.Base

deriving instance Ord Exp
deriving instance Ord Dec
deriving instance Ord Stmt
deriving instance Ord Type
deriving instance Ord Foreign
deriving instance Ord FunDep
deriving instance Ord Con
deriving instance Ord Body
deriving instance Ord Clause
deriving instance Ord Strict
deriving instance Ord Safety
deriving instance Ord Callconv
deriving instance Ord Guard
deriving instance Ord Range
deriving instance Ord Match
deriving instance Ord Pat
deriving instance Ord Lit
deriving instance Ord Kind
deriving instance Ord FamFlavour
deriving instance Ord Pragma
deriving instance Ord TyVarBndr
deriving instance Ord Pred
deriving instance Ord InlineSpec

instance Lift () where
  lift () = [|()|]

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 609
instance Show Loc where
  show (Loc f p m s e) =
    intercalate " " $
      ("Loc":fmap show [f,p,m]++[show s,show e])

instance Eq Loc where
  (Loc a b c d e) == (Loc v w x y z)
    = and $ [d==y,e==z] ++
        (zipWith (==) [a,b,c] [v,w,x])

-- TODO: make this better
instance Ppr Loc where
  ppr = showtextl . show
#endif

instance Ppr Lit where
  ppr l = ppr (LitE l)

-- comic relief from HERA
instance Lift Rational where lift _ = error "Rational.. what are you doing!"

instance Lift Name where
  lift (Name occName nameFlavour) = [| Name occName nameFlavour |]

--instance Lift PackedString where
--  lift ps = [| packString $(lift $ unpackPS ps) |]

instance Lift NameFlavour where
  lift NameS = [| NameS |]
  lift (NameQ modName) = [| NameQ modName |]
  lift (NameU i) = [| case $( lift (I# i) ) of
                          I# i' -> NameU i' |]
  lift (NameL i) = [| case $( lift (I# i) ) of
                          I# i' -> NameL i' |]
  lift (NameG nameSpace pkgName modName)
    = [| NameG nameSpace pkgName modName |]

instance Lift OccName where
   lift occName 
     = conE (mkNameG_v (occString occName) "template-haskell" "Language.Haskell.TH.Syntax")

instance Lift ModName where
   lift modName 
     = conE (mkNameG_v (modString modName) "template-haskell" "Language.Haskell.TH.Syntax")

instance Lift PkgName where
   lift pkgName 
     = conE (mkNameG_v (pkgString pkgName) "template-haskell" "Language.Haskell.TH.Syntax")

instance Lift NameSpace where
  lift VarName = [| VarName |]
  lift DataName = [| DataName |]
  lift TcClsName = [| TcClsName |]


instance Lift Dec where
        lift (FunD x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "FunD" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)
        lift (ValD x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (mkNameG_v "ValD" "template-haskell" "Language.Haskell.TH.Syntax"))
                    (lift x0))
                 (lift x1))
              (lift x2)
        lift (DataD x0 x1 x2 x3 x4)
          = appE
              (appE
                 (appE
                    (appE
                       (appE
                          (conE
                             (mkNameG_v "DataD" "template-haskell" "Language.Haskell.TH.Syntax"))
                          (lift x0))
                       (lift x1))
                    (lift x2))
                 (lift x3))
              (lift x4)
        lift (NewtypeD x0 x1 x2 x3 x4)
          = appE
              (appE
                 (appE
                    (appE
                       (appE
                          (conE
                             (mkNameG_v "NewtypeD" "template-haskell" "Language.Haskell.TH.Syntax"))
                          (lift x0))
                       (lift x1))
                    (lift x2))
                 (lift x3))
              (lift x4)
        lift (TySynD x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (mkNameG_v "TySynD" "template-haskell" "Language.Haskell.TH.Syntax"))
                    (lift x0))
                 (lift x1))
              (lift x2)
        lift (ClassD x0 x1 x2 x3 x4)
          = appE
              (appE
                 (appE
                    (appE
                       (appE
                          (conE
                             (mkNameG_v "ClassD" "template-haskell" "Language.Haskell.TH.Syntax"))
                          (lift x0))
                       (lift x1))
                    (lift x2))
                 (lift x3))
              (lift x4)
        lift (InstanceD x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (mkNameG_v "InstanceD" "template-haskell" "Language.Haskell.TH.Syntax"))
                    (lift x0))
                 (lift x1))
              (lift x2)
        lift (SigD x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "SigD" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)
        lift (ForeignD x0)
          = appE
              (conE
                 (mkNameG_v "ForeignD" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)

instance Lift Exp where
        lift (VarE x0)
          = appE
              (conE
                 (mkNameG_v "VarE" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (ConE x0)
          = appE
              (conE
                 (mkNameG_v "ConE" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (LitE x0)
          = appE
              (conE
                 (mkNameG_v "LitE" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (AppE x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "AppE" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)
        lift (InfixE x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (mkNameG_v "InfixE" "template-haskell" "Language.Haskell.TH.Syntax"))
                    (lift x0))
                 (lift x1))
              (lift x2)
        lift (LamE x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "LamE" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)
        lift (TupE x0)
          = appE
              (conE
                 (mkNameG_v "TupE" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (CondE x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (mkNameG_v "CondE" "template-haskell" "Language.Haskell.TH.Syntax"))
                    (lift x0))
                 (lift x1))
              (lift x2)
        lift (LetE x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "LetE" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)
        lift (CaseE x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "CaseE" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)
        lift (DoE x0)
          = appE
              (conE
                 (mkNameG_v "DoE" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (CompE x0)
          = appE
              (conE
                 (mkNameG_v "CompE" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (ArithSeqE x0)
          = appE
              (conE
                 (mkNameG_v "ArithSeqE" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (ListE x0)
          = appE
              (conE
                 (mkNameG_v "ListE" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (SigE x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "SigE" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)
        lift (RecConE x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "RecConE" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)
        lift (RecUpdE x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "RecUpdE" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)

instance Lift Lit where
        lift (CharL x0)
          = appE
              (conE
                 (mkNameG_v "CharL" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (StringL x0)
          = appE
              (conE
                 (mkNameG_v "StringL" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (IntegerL x0)
          = appE
              (conE
                 (mkNameG_v "IntegerL" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (RationalL x0)
          = appE
              (conE
                 (mkNameG_v "RationalL" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (IntPrimL x0)
          = appE
              (conE
                 (mkNameG_v "IntPrimL" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 609
        lift (WordPrimL x0)
          = appE
              (conE
                 (mkNameG_v "WordPrimL" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
#endif
        lift (FloatPrimL x0)
          = appE
              (conE
                 (mkNameG_v "FloatPrimL" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (DoublePrimL x0)
          = appE
              (conE
                 (mkNameG_v "DoublePrimL" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)

instance Lift Pat where
        lift (LitP x0)
          = appE
              (conE
                 (mkNameG_v "LitP" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (VarP x0)
          = appE
              (conE
                 (mkNameG_v "VarP" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (TupP x0)
          = appE
              (conE
                 (mkNameG_v "TupP" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (ConP x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "ConP" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)
        lift (InfixP x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (mkNameG_v "InfixP" "template-haskell" "Language.Haskell.TH.Syntax"))
                    (lift x0))
                 (lift x1))
              (lift x2)
        lift (TildeP x0)
          = appE
              (conE
                 (mkNameG_v "TildeP" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (AsP x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "AsP" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)
        lift (WildP)
          = conE
              (mkNameG_v "WildP" "template-haskell" "Language.Haskell.TH.Syntax")
        lift (RecP x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "RecP" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)
        lift (ListP x0)
          = appE
              (conE
                 (mkNameG_v "ListP" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (SigP x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "SigP" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)

instance Lift Body where
        lift (GuardedB x0)
          = appE
              (conE
                 (mkNameG_v "GuardedB" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (NormalB x0)
          = appE
              (conE
                 (mkNameG_v "NormalB" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)

instance Lift Con where
        lift (NormalC x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "NormalC" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)
        lift (RecC x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "RecC" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)
        lift (InfixC x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (mkNameG_v "InfixC" "template-haskell" "Language.Haskell.TH.Syntax"))
                    (lift x0))
                 (lift x1))
              (lift x2)
        lift (ForallC x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (mkNameG_v "ForallC" "template-haskell" "Language.Haskell.TH.Syntax"))
                    (lift x0))
                 (lift x1))
              (lift x2)

instance Lift Clause where
        lift (Clause x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (mkNameG_v "Clause" "template-haskell" "Language.Haskell.TH.Syntax"))
                    (lift x0))
                 (lift x1))
              (lift x2)

instance Lift Guard where
        lift (NormalG x0)
          = appE
              (conE
                 (mkNameG_v "NormalG" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (PatG x0)
          = appE
              (conE
                 (mkNameG_v "PatG" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)

instance Lift Strict where
        lift (IsStrict)
          = conE
              (mkNameG_v "IsStrict" "template-haskell" "Language.Haskell.TH.Syntax")
        lift (NotStrict)
          = conE
              (mkNameG_v "NotStrict" "template-haskell" "Language.Haskell.TH.Syntax")

instance Lift FunDep where
        lift (FunDep x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "FunDep" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)

instance Lift Foreign where
        lift (ImportF x0 x1 x2 x3 x4)
          = appE
              (appE
                 (appE
                    (appE
                       (appE
                          (conE
                             (mkNameG_v "ImportF" "template-haskell" "Language.Haskell.TH.Syntax"))
                          (lift x0))
                       (lift x1))
                    (lift x2))
                 (lift x3))
              (lift x4)
        lift (ExportF x0 x1 x2 x3)
          = appE
              (appE
                 (appE
                    (appE
                       (conE
                          (mkNameG_v "ExportF" "template-haskell" "Language.Haskell.TH.Syntax"))
                       (lift x0))
                    (lift x1))
                 (lift x2))
              (lift x3)

instance Lift Callconv where
        lift (CCall)
          = conE
              (mkNameG_v "CCall" "template-haskell" "Language.Haskell.TH.Syntax")
        lift (StdCall)
          = conE
              (mkNameG_v "StdCall" "template-haskell" "Language.Haskell.TH.Syntax")

instance Lift Safety where
        lift (Unsafe)
          = conE
              (mkNameG_v "Unsafe" "template-haskell" "Language.Haskell.TH.Syntax")
        lift (Safe)
          = conE
              (mkNameG_v "Safe" "template-haskell" "Language.Haskell.TH.Syntax")
        lift (Threadsafe)
          = conE
              (mkNameG_v "Threadsafe" "template-haskell" "Language.Haskell.TH.Syntax")

instance Lift TyVarBndr where
        lift (PlainTV x0)
          = appE
              (conE
                 (mkNameG_v "PlainTV" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (KindedTV x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "KindedTV" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)

instance Lift Kind where
        lift (StarK)
          = conE
              (mkNameG_v "StarK" "template-haskell" "Language.Haskell.TH.Syntax")
        lift (ArrowK x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "ArrowK" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)

instance Lift Match where
        lift (Match x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (mkNameG_v "Match" "template-haskell" "Language.Haskell.TH.Syntax"))
                    (lift x0))
                 (lift x1))
              (lift x2)

instance Lift Pred where
        lift (ClassP x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "ClassP" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)
        lift (EqualP x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "EqualP" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)


instance Lift Stmt where
        lift (BindS x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "BindS" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)
        lift (LetS x0)
          = appE
              (conE
                 (mkNameG_v "LetS" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (NoBindS x0)
          = appE
              (conE
                 (mkNameG_v "NoBindS" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (ParS x0)
          = appE
              (conE
                 (mkNameG_v "ParS" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)

instance Lift Range where
        lift (FromR x0)
          = appE
              (conE
                 (mkNameG_v "FromR" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (FromThenR x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "FromThenR" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)
        lift (FromToR x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "FromToR" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)
        lift (FromThenToR x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (mkNameG_v "FromThenToR" "template-haskell" "Language.Haskell.TH.Syntax"))
                    (lift x0))
                 (lift x1))
              (lift x2)

instance Lift Type where
        lift (ForallT x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (mkNameG_v "ForallT" "template-haskell" "Language.Haskell.TH.Syntax"))
                    (lift x0))
                 (lift x1))
              (lift x2)
        lift (VarT x0)
          = appE
              (conE
                 (mkNameG_v "VarT" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (ConT x0)
          = appE
              (conE
                 (mkNameG_v  "ConT" "template-haskell"  "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (TupleT x0)
          = appE
              (conE
                 (mkNameG_v "TupleT" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (ArrowT)
          = conE
              (mkNameG_v "ArrowT" "template-haskell" "Language.Haskell.TH.Syntax")
        lift (ListT)
          = conE
              (mkNameG_v "ListT" "template-haskell" "Language.Haskell.TH.Syntax")
        lift (AppT x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "AppT" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)

instance Lift Info where
        lift (ClassI x0 x1)
          = appE
                (conE
                   (mkNameG_v "ClassI" "template-haskell" "Language.Haskell.TH.Syntax"))
                (lift x0)
        lift (ClassOpI x0 x1 x2 x3)
          = appE
              (appE
                 (appE
                    (appE
                       (conE
                          (mkNameG_v "ClassOpI" "template-haskell" "Language.Haskell.TH.Syntax"))
                       (lift x0))
                    (lift x1))
                 (lift x2))
              (lift x3)
        lift (TyConI x0)
          = appE
              (conE
                 (mkNameG_v "TyConI" "template-haskell" "Language.Haskell.TH.Syntax"))
              (lift x0)
        lift (PrimTyConI x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (mkNameG_v "PrimTyConI" "template-haskell" "Language.Haskell.TH.Syntax"))
                    (lift x0))
                 (lift x1))
              (lift x2)
        lift (DataConI x0 x1 x2 x3)
          = appE
              (appE
                 (appE
                    (appE
                       (conE     
                          (mkNameG_v  "DataConI" "template-haskell" "Language.Haskell.TH.Syntax"))
                       (lift x0))
                    (lift x1))
                 (lift x2))
              (lift x3)
        lift (VarI x0 x1 x2 x3)
          = appE
              (appE
                 (appE
                    (appE
                       (conE
                          (mkNameG_v "VarI" "template-haskell" "Language.Haskell.TH.Syntax"))
                       (lift x0))
                    (lift x1))
                 (lift x2))
              (lift x3)
        lift (TyVarI x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "TyVarI" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)

instance Lift Fixity where
        lift (Fixity x0 x1)
          = appE
              (appE
                 (conE
                    (mkNameG_v "Fixity" "template-haskell" "Language.Haskell.TH.Syntax"))
                 (lift x0))
              (lift x1)

instance Lift FixityDirection where
        lift (InfixL)
          = conE
              (mkNameG_v "InfixL" "template-haskell" "Language.Haskell.TH.Syntax")
        lift (InfixR)
          = conE
              (mkNameG_v "InfixR" "template-haskell" "Language.Haskell.TH.Syntax")
        lift (InfixN)
          = conE
              (mkNameG_v "InfixN" "template-haskell" "Language.Haskell.TH.Syntax")
