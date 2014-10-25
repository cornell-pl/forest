{-# LANGUAGE DoAndIfThenElse, TemplateHaskell, TupleSections #-}

module Language.Forest.TH where
	
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad
--import Data.Generics.TH
import Data.Generics
import Language.Forest.Syntax
	
-- | Computes the set of external variables used by an arbitrary Haskell expression
expVars :: Exp -> Set Name
expVars (VarE n) = Set.singleton n
expVars (ConE n) = Set.empty
expVars (LitE l) = Set.empty
expVars (AppE e1 e2) = expVars e1 `Set.union` expVars e2
expVars (InfixE mbe e mbe1) = maybe Set.empty expVars mbe `Set.union` expVars e `Set.union` maybe Set.empty expVars mbe1
expVars (UInfixE e e1 e2) = expVars e `Set.union` expVars e1 `Set.union` expVars e2
expVars (ParensE e) = expVars e
expVars (LamE pats e) = let (pv,ev) = patsVars pats in (expVars e `Set.union` ev) `Set.difference` pv
expVars (TupE es) = unionMap expVars es
expVars (CondE e e1 e2) = expVars e `Set.union` expVars e1 `Set.union` expVars e2
expVars (MultiIfE l) = unionMap guardedVars l 
expVars (LetE decs e) = let (pv,ev) = decsVars decs in ev `Set.union` (expVars e `Set.difference` pv)
expVars (CaseE e ms) = expVars e `Set.union` unionMap matchVars ms
expVars (DoE stmts) = stmtsVars stmts
expVars (CompE stmts) = stmtsVars stmts
expVars (ArithSeqE rng) = rangeVars rng
expVars (ListE es) = unionMap expVars es
expVars (SigE e t) = expVars e
expVars (RecConE n l) = unionMap (expVars . snd) l
expVars (RecUpdE e l) = expVars e `Set.union` unionMap (expVars . snd) l
expVars e = error $ show e

stmtsVars :: [Stmt] -> Set Name
stmtsVars [] = Set.empty
stmtsVars (BindS pat e:stmts) = let (pv,ev) = patVars pat in expVars e `Set.union` ((ev `Set.union` stmtsVars stmts) `Set.difference` pv)
stmtsVars (LetS decs:stmts) = let (pv,ev) = decsVars decs in ev `Set.union` (stmtsVars stmts `Set.difference` pv)
stmtsVars [NoBindS e] = expVars e
stmtsVars (ParS stmts:stmts') = stmtsVars (concat stmts++stmts')

patsVars :: [Pat] -> (Set Name,Set Name)
patsVars [] = (Set.empty,Set.empty)
patsVars (pat:pats) = let (pv,ev) = patVars pat
                          (pvs,evs) = patsVars pats
                      in (pv `Set.union` pvs,ev `Set.union` (evs `Set.difference` pv))

patPVars :: Pat -> Set Name
patPVars = fst . patVars

-- | Computes the set of variables defined in a pattern, and the set of variables used in view pattern expressions
patVars :: Pat -> (Set Name,Set Name)
patVars = everythingBut pairwiseUnion (mkQ z notExp `extQ` yesName) where
	z = ((Set.empty,Set.empty),False)
	notExp = (,True) . (Set.empty,) . expVars
	yesName = (,False) . getName
	
--patVars = $(let notExp t = do isExp <- eqType [t|Exp|] t
--                              if isExp then do h <- [|(\x -> (Set.empty,x)) . expVars|]
--                                               return (h,True)
--                              else liftM (,False) (mkQ [| (Set.empty,Set.empty) |] 'getName t)          
--            in everythingBut [| pairwiseUnion |] notExp [t| Pat |])

-- | Computes the set of variables defined in a declaration, and the set of variables used in declaration expressions
decsVars :: [Dec] -> (Set Name,Set Name)
decsVars [] = (Set.empty,Set.empty)
decsVars (dec:decs) = let (pv,ev) = decVars dec
                          (pvs,evs) = decsVars decs
                      in (pv `Set.union` pvs,ev `Set.union` (evs `Set.difference`) pv)

decVars :: Dec -> (Set Name,Set Name)
decVars (FunD name cls) = (Set.singleton name,unionMap clauseVars cls)
decVars (ValD pat b decs) = (fst (patVars pat),clauseVars (Clause [pat] b decs))
decVars _ = (Set.empty,Set.empty)

clauseVars :: Clause -> Set Name
clauseVars (Clause pats b decs) = let (pv,ev) = patsVars pats
                                      (pv',ev') = decsVars decs
                                  in ((bodyVars b `Set.union` ev') `Set.difference` (pv `Set.union` pv')) `Set.union` (ev `Set.difference` pv)

matchVars :: Match -> Set Name
matchVars (Match pat b decs) = clauseVars (Clause [pat] b decs)

bodyVars :: Body -> Set Name
bodyVars (GuardedB l) = unionMap guardedVars l
bodyVars (NormalB e) = expVars e

guardedVars :: (Guard,Exp) -> Set Name
guardedVars (NormalG e,e') = expVars e `Set.union` expVars e'
guardedVars (PatG stmts,e) = stmtsVars (stmts++[NoBindS e])

rangeVars :: Range -> Set Name
rangeVars = everything Set.union (mkQ Set.empty expVars)
--	$(everything [| Set.union |] (mkQ [| Set.empty |] 'expVars') [t| Range |])

unionMap :: Ord b => (a -> Set b) -> [a] -> Set b
unionMap f = foldr (\x s -> f x `Set.union` s) Set.empty

pairwiseUnion :: (Ord a,Ord b) => (Set a,Set b) -> (Set a,Set b) -> (Set a,Set b)
pairwiseUnion (s1,s2) (s1',s2') = (s1 `Set.union` s1',s2 `Set.union` s2')

pairwiseUnionMap :: (Ord b,Ord c) => (a -> (Set b,Set c)) -> [a] -> (Set b,Set c)
pairwiseUnionMap f = foldr (\x (s1,s2) -> let (bs,cs) = f x in (bs `Set.union` s1,cs `Set.union` s2)) (Set.empty,Set.empty)


getCompFieldExternalName explicitName externalE = case explicitName of
	Just str -> str
	Nothing -> case externalE of
		VarE name -> nameBase name
	
-- | Gets all the variables used in expressions inside Forest specifications, with care for variables defined by Forest
forestTyVars :: ForestTy -> Set Name
forestTyVars (Directory (Record _ fields)) = fieldsVars fields
forestTyVars (File (s,Nothing)) = Set.empty
forestTyVars (File (s,Just e)) = expVars e
forestTyVars (Archive _ ty) = forestTyVars ty
forestTyVars (Named s) = Set.empty
forestTyVars (FMaybe ty) = forestTyVars ty
forestTyVars SymLink = Set.empty
forestTyVars (FConstraint (VarP name) ty exp) = constraintVars name ty exp
forestTyVars (Fapp ty exps) = Set.unions (map expVars exps) `Set.union` forestTyVars ty
--forestTyVars (FComp c) = fieldVars

constraintVars :: Name -> ForestTy -> Exp -> Set Name
constraintVars name ty exp = (expVars exp `Set.difference` vars) `Set.union` forestTyVars ty
	where vars = Set.fromList [mkName (nameBase name),mkName (nameBase name++"_md"),mkName (nameBase name++"_att")]

generatorVars :: Generator -> Set Name
generatorVars (Explicit exp) = expVars exp
generatorVars (Matches exp) = Set.empty

fieldsVars :: [Field] -> Set Name
fieldsVars [] = Set.empty
fieldsVars [f] = fst $ fieldVars f
fieldsVars (f:fs) = fvars `Set.union` (fieldsVars fs `Set.difference` fdecvars)
	where (fvars,fdecvars) = fieldVars f

fieldPVars = fst . fieldVars

-- used variables, declared field internal variables
fieldVars :: Field -> (Set Name,Set Name)
fieldVars (Simple (internal, isForm, externalE, ty, Nothing)) = (expVars externalE `Set.union` forestTyVars ty,vars)
	where vars = Set.fromList [mkName internal,mkName (internal++"_md")]
fieldVars (Simple (internal, isForm, externalE, ty, Just pred)) = (expVars externalE `Set.union` constraintVars (mkName internal) ty pred,vars)
	where vars = Set.fromList [mkName internal,mkName (internal++"_md")]
fieldVars (Comp (CompField internal tyConNameOpt explicitName externalE ty (VarP name) generatorG Nothing)) = 
		((expVars externalE `Set.difference` vars) `Set.union` generatorVars generatorG `Set.union` forestTyVars ty,decvars)
	where
	externalName = getCompFieldExternalName explicitName externalE
	vars = Set.fromList [name,mkName (nameBase name++"_att")]
	decvars = Set.fromList [mkName internal,mkName (internal++"_md")]
fieldVars (Comp (CompField internal tyConNameOpt explicitName externalE ty (VarP name) generatorG (Just pred))) =
		((expVars externalE `Set.difference` vars) `Set.union` generatorVars generatorG `Set.union` constraintVars (mkName externalName) ty pred,decvars)
	where
	externalName = getCompFieldExternalName explicitName externalE
	vars = Set.fromList [name,mkName (nameBase name++"_att")]
	decvars = Set.fromList [mkName internal,mkName (internal++"_md")]


getName :: Name -> (Set Name,Set Name)
getName x = (Set.singleton x,Set.empty)

nameVars :: Name -> Set Name
nameVars = Set.singleton

