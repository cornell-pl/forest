{-# LANGUAGE DeriveDataTypeable #-}

module Language.Pads.Syntax where

import Data.Generics
import Language.Haskell.TH  as TH hiding (Lit,CharL,StringL) 

data Lit    = CharL Char | StringL String  | EorL | EofL | VoidL
  deriving (Eq, Data, Typeable, Show)

data TermCond = TyTC PadsTy | LengthTC TH.Exp
  deriving (Eq, Data, Typeable, Show)

type FieldInfo = (Maybe String, PadsTy, Maybe TH.Exp)

data PadsTy = Plit Lit
            | Pname String
            | Ptuple [PadsTy] 
            | Pmaybe PadsTy
            | Pline PadsTy
            | Ptry PadsTy
            | Papp PadsTy TH.Exp
            | Ptrans PadsTy PadsTy TH.Exp   {- Src, Dest, and pair of functions to do transformation -}
            | Ptypedef TH.Pat PadsTy TH.Exp  {- pattern bound to underlying type, underlying type, predicate -}
            | Precord String [FieldInfo]
            | Punion  String [FieldInfo]
            | Pswitch String TH.Exp [(TH.Pat, FieldInfo)]
            | Plist  PadsTy (Maybe PadsTy) (Maybe TermCond)
   deriving (Eq, Data, Typeable, Show)

newtype PadsDecl = PadsDecl (Id, Maybe TH.Pat, PadsTy)
   deriving (Eq, Data, Typeable, Show)

data Id = Id String
        | AntiId String
    deriving (Eq, Ord, Data, Typeable, Show)

