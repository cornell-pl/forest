{-# LANGUAGE TemplateHaskell #-}

module Language.Pads.TH where

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Data.Data
import Control.Monad
import Char



mergeMaybe m1 m2 = case (m1,m2) of
  (Nothing, Nothing) -> Nothing
  (Just d1, Just d2) -> Just (d1,d2)
  _ -> error "mergeMaybe given two maybes in different states."


flattenMaybeList xs = case xs of
  [] -> []
  (Nothing: xxs) -> flattenMaybeList xxs
  (Just x : xxs) -> x : flattenMaybeList xxs

mapFstChar f [] = []
mapFstChar f (c:cs) = (f c) : cs

strToUpper = mapFstChar toUpper
strToLower = mapFstChar toLower


mk_newTyD ty_name ty = NewtypeD [] ty_name [] con derives
    where con = NormalC ty_name [(NotStrict,ty)]           -- How should we determine whether a type should be Strict or not?
          derives = (map mkName ["Show", "Eq"]) ++  [''Typeable, ''Data]

mk_TySynD ty_name ty = TySynD ty_name [] ty

arrowTy ty1 ty2 = AppT (AppT ArrowT     ty1  ) ty2

tyListToListTy  tys      = foldl AppT ListT                                tys

tyListToTupleTy (ty:tys) = foldl AppT (AppT (TupleT (1 + length tys) ) ty) tys


tupleTyToListofTys (AppT (TupleT n) ty) = (n, collect ty)
  where collect (AppT ty' tys') = ty' : (collect tys')
        collect ty = [ty]

tupleTyToListofTys ty = collect ty []
  where collect (TupleT n) acc = (n, acc)
        collect (AppT tys ty) acc = collect tys (ty:acc)

genPE name = (VarE name, VarP name)

doGenPE str = do {
  ; name <- newName str
  ; return (VarE name, VarP name)
  }

doGenPEs :: Int -> String -> Q([TH.Exp], [TH.Pat])
doGenPEs n str = do 
  { varpats <- replicateM n (doGenPE str)
  ; return (unzip varpats)
  }

{- XXX: need to add location information so can report location of error messages. -}
patToTy :: TH.Pat -> TH.Type
patToTy pat = case pat of
  LitP l      -> litToTy l
  VarP n      -> error ("Variable "++ (showName n) ++ " needs a type annotation.")
  TupP pats   -> tyListToTupleTy (map patToTy pats)
  InfixP p1 n p2 -> error ("Infix constructor "++ (showName n) ++ " application needs a type annotation.")
  TildeP p    -> patToTy p
  BangP  p    -> patToTy p
  AsP n p     -> patToTy p
  WildP       -> error "Wild card patterns are not supported in PADS declarations."
  RecP name fieldPats -> ConT name   {-  I think this is the correct represtentation of a line type. -}
  ListP pats  -> tyListToListTy (map patToTy pats)
  SigP p ty   -> ty
  
litToTy :: TH.Lit -> TH.Type
litToTy lit = 
  let name = case lit of
       Language.Haskell.TH.Syntax.CharL c       -> ''Char
       Language.Haskell.TH.Syntax.StringL s     -> ''String
       IntegerL i    -> ''Integer
       RationalL r   -> ''Rational
       IntPrimL  i   -> ''Integer
       WordPrimL i   -> ''Integer
       FloatPrimL f  -> ''Rational
       DoublePrimL d -> ''Rational
  in ConT name

patToExp :: TH.Pat -> TH.Exp
patToExp pat = case pat of
  LitP l      -> LitE l
  VarP n      -> VarE n
  TupP pats   -> TupE (map patToExp pats)
  InfixP p1 n p2 -> InfixE (Just (patToExp p1)) (VarE n) (Just (patToExp p2))
  TildeP p    -> patToExp p
  BangP  p    -> patToExp p
  AsP n p     -> VarE n
  WildP       -> error "Wild card patterns are not supported in PADS declarations. Can't convert to expression"
  RecP name fieldPats -> RecConE name (map fieldPatToExp fieldPats)   {-  I think this is the correct represtentation of a line type. -}
  ListP pats  -> ListE (map patToExp pats)
  SigP p ty   -> patToExp p

fieldPatToExp (n,p) = (n, patToExp p)
