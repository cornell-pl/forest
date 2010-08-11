{-# LANGUAGE DeriveDataTypeable #-}

module Language.Pads.Syntax where

import Data.Generics
import Language.Haskell.TH as TH

data Lit    = CharL Char | StringL String
  deriving (Eq, Data, Typeable)

data PadsTy = Plit Language.Pads.Syntax.Lit
            | Pname String
            | Ptuple [PadsTy] 
            | Pline PadsTy
            | Papp PadsTy TH.Exp
            | Ptrans PadsTy PadsTy TH.Exp   {- Src, Dest, and pair of functions to do transformation -}
            | Ptypedef TH.Pat PadsTy TH.Exp  {- pattern bound to underlying type, underlying type, predicate -}
            | Precord String [(Maybe String, PadsTy, Maybe TH.Exp)]
            | Punion  String [(Maybe String, PadsTy, Maybe TH.Exp)]
   deriving (Eq, Data, Typeable)

newtype PadsDecl = PadsDecl (Id, Maybe TH.Pat, PadsTy)
   deriving (Eq, Data, Typeable)

data Id = Id String
        | AntiId String
    deriving (Eq, Ord, Data, Typeable)

