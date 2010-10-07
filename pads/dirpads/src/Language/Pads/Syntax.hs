{-# LANGUAGE DeriveDataTypeable #-}

module Language.Pads.Syntax where

import Data.Generics
import qualified Language.Haskell.TH  as TH 

data Lit    = CharL Char | StringL String  | EorL | EofL | VoidL | RegL String | Hid String | Hexp TH.Exp | IntL Integer
  deriving (Eq, Data, Typeable, Show)

litToExp :: Lit -> TH.Exp
litToExp lit = case lit of
  CharL   c -> TH.LitE (TH.CharL   c)
  StringL s -> TH.LitE (TH.StringL s)
  IntL    i -> TH.LitE (TH.IntegerL i)
  RegL    r -> TH.AppE (TH.ConE (TH.mkName "RE")) (TH.LitE (TH.StringL r))
  Hid    id -> TH.VarE (TH.mkName id)
  Hexp  exp -> exp

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

