{-# LANGUAGE ConstraintKinds, FlexibleContexts, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, ViewPatterns  #-}

module Language.Forest.CodeGen.Utils where

import Control.Monad.Incremental
import Language.Forest.Syntax as PS
import Language.Forest.MetaData
import Language.Forest.Errors
import Language.Forest.Generic
import qualified Language.Forest.Errors as E
import Language.Forest.FS.FSDelta
import System.Directory
import System.FilePath.Posix

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Pads.Padsc hiding (lift)
import Language.Pads.TH
import Language.Forest.IO
import Language.Forest.TH
import Language.Forest.FS.FSRep
import Language.Forest.BX

import Data.Data
import Data.Maybe
import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Control.Exception as CE
import Control.Monad
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Monad.State (State(..),StateT(..))
import qualified Control.Monad.State as State
import Language.Forest.ValueDelta

--import qualified Control.Lens as L
--import Control.Lens.TH

getTyConArity :: String -> Q Int
getTyConArity str = do 
  { let tyConName = mkName str
  ; info <- qReify tyConName
  ; case info of
     TyConI dec -> return (getDecArity dec)
     otherwise -> error ("Forest: "++ str ++" should be a type constructor.")
  }

constExp :: TH.Exp -> TH.Exp
constExp e = AppE (VarE 'Prelude.const) e

getDecArity :: TH.Dec -> Int
getDecArity dec = case dec of
  DataD cxt name args cons derives -> length args
  NewtypeD cxt name args con derives -> length args
  TySynD name args ty -> length args

splitMD md = (\file path -> return md,fileInfo md)

getCompName explicitName externalE = case explicitName of
	Just str -> mkName str
	Nothing -> case externalE of
		VarE name -> name

getFileAttNames explicitName externalE = case explicitName of
	Just str ->
		let getMDName = mkName $ str ++ "_getMD"
		    attName = mkName (str++"att")
		in (mkName str, VarP getMDName,VarE getMDName,attName)
	Nothing -> case externalE of
		VarE name -> 
			let getMDName = mkName ((nameBase name) ++"_getMD")
			    attName = mkName ((nameBase name) ++"_att")			
			in (name, VarP getMDName,VarE getMDName,attName)
		otherwise -> error "getFileAttNames"

getAttMDPat explicitName externalE = 
  case explicitName of 
      Just str ->
		let getMDName = mkName (str++"_getMD")
		    attName = mkName (str++"att")
		in (VarE getMDName,TildeP (TupP [VarP (mkName str),ViewP (VarE 'splitMD) $ TupP [VarP getMDName,VarP attName] ]))
      Nothing -> getAttMDPatFromExp externalE
getAttMDPatFromExp externalE = case externalE of
  VarE name ->
	let getMDName = mkName ((nameBase name) ++"_getMD")
	    attName = mkName ((nameBase name) ++"_att")
	in (VarE getMDName,TildeP (TupP [VarP name,ViewP (VarE 'splitMD) $ TupP [VarP getMDName,VarP attName] ]))
  otherwise -> error "Forest: Couldn't convert file expression to pattern; please supply an explicit name with 'name as exp' form"

getAttPat explicitName externalE = 
  case explicitName of 
      Just str -> TildeP (TupP[VarP (mkName str), VarP (mkName (str++"_att"))])
      Nothing -> getAttPatFromExp externalE

getAttPatFromExp externalE = case externalE of
  VarE name -> TildeP (TupP[VarP name, VarP (mkName ((nameBase name) ++"_att"))])
  otherwise -> error "Forest: Couldn't convert file expression to pattern; please supply an explicit name with 'name as exp' form"

getRepMDPat explicitName externalE = 
  case explicitName of 
      Just str -> TildeP (TupP[VarP (mkName str), VarP (mkName (str++"_md"))])
      Nothing -> getRepMDPatFromExp externalE

getRepMDPatFromExp externalE = case externalE of
  VarE name -> TildeP (TupP[VarP name, VarP (mkName ((nameBase name) ++"_md"))])
  otherwise -> error "Forest: Couldn't convert file expression to pattern; please supply an explicit name with 'name as exp' form"

modPredE externalP predE = case externalP of
  VarP name -> let md_name = mkName ((nameBase name) ++"_md")  
                   attP    = VarP (mkName ((nameBase name) ++"_att"))
                   pat     = TildeP (TupP[VarP name, VarP md_name])
                   initE   = AppE (VarE 'get_fileInfo) (VarE md_name)
                   bodyE   = DoE [BindS attP initE,NoBindS predE]      -- let name_att = fileInfo name_md in predE
               in LamE [pat] bodyE            
  otherwise -> error "Forest: Couldn't convert constraint pattern to pattern for meta data.  Use simple pattern."

modPredEComp externalP predE = case externalP of
  VarP name -> let attP    = VarP (mkName ((nameBase name) ++"_att_thunk"))
                   pats     = [VarP name, attP]
                   bodyE   = predE     -- let name_att = fileInfo name_md in predE
               in LamE pats bodyE            
  otherwise -> error "Forest: Couldn't convert constraint pattern to pattern for meta data.  Use simple pattern."



buildFieldLens :: Name -> Q TH.Exp
buildFieldLens field = do
	r <- newName "r"
	f <- newName "f"
	let (rE,rP) = genPE r
	let (fE,fP) = genPE f
	let getE = VarE field
	let putE = LamE [rP,fP] $ RecUpdE rE [(field,fE)]
	return $ appE2 (ConE 'Lens) getE putE

forestTupleTy :: [TH.Type] -> TH.Type
forestTupleTy ts = foldl1' (appT2 (ConT ''(:*:))) ts

forestTupleP :: [Pat] -> Pat
forestTupleP = foldl1' (\p1 p2 -> UInfixP p1 ('(:*:)) p2)

forestTupleE :: [Exp] -> Exp
forestTupleE = foldl1' (\e1 e2 -> UInfixE e1 (ConE '(:*:)) e2)

-- generates a function that collects the fields of all the specifications in a directory and returns a list of @Forest_md@s
genMergeFieldsMDErrors :: [Field] -> Q TH.Exp
genMergeFieldsMDErrors fields = do
	let names_md = map (mkName . (++"_md")) $ fieldnames fields
	let gets_fmd = map genMergeFieldMDErrors fields
	let md = mkName "md"
	let mds = map (\(name_md,get_fmd) -> AppE get_fmd $ AppE (VarE name_md) (VarE md)) $ zip names_md gets_fmd
	return $ LamE [VarP md] $ appE2 (VarE 'liftM) (VarE 'mergeMDErrors) $ AppE (VarE 'sequence) $ ListE mds

genMergeFieldMDErrors :: Field -> TH.Exp
genMergeFieldMDErrors (Simple _) = VarE 'get_errors
genMergeFieldMDErrors (Comp compField) = case tyConNameOpt compField of
	Nothing -> VarE 'merge_list_errors
	Just str -> VarE 'merge_container_errors

unBindS (BindS _ e) = NoBindS e
unBindSWith f (BindS _ e) = NoBindS $ f e

filterWPred :: [(String, IO (a,b), ((a,b) -> Bool))] -> IO [ (String, (a,b))] 
filterWPred [] = return [] 
filterWPred (~(name, action, pred):rest) = do 
	~(rep,md) <- action
	if pred (rep,md)
		then do 
			resRes <- filterWPred rest
			return ((name,(rep,md)) : resRes)  
		else filterWPred rest

mkArgsE :: [(Name,TH.Exp)] -> TH.Exp -> TH.Exp
mkArgsE argsE e = foldr (\(x,argE) doE -> InfixE (Just argE) (VarE '(>>=)) $ Just $ LamE [VarP x] doE) e argsE

class (FSRep fs,ForestInput fs FSThunk Inside) => MDContainer fs md where
	collect_container_mds :: ForestLayer fs l => md -> ForestL fs l [Forest_md fs]
	merge_container_errors :: ForestLayer fs l => md -> ForestL fs l Forest_err
	merge_container_errors md = liftM mergeMDErrors $ mapM get_errors =<< collect_container_mds md

merge_list_errors :: (ForestLayer fs l,ForestMD fs md) => [(FilePath,md)] -> ForestL fs l Forest_err
merge_list_errors md = liftM mergeMDErrors $ mapM get_errors =<< collect_list_mds md

collect_list_mds :: (ForestLayer fs l,ForestMD fs md) => [(FilePath,md)] -> ForestL fs l [Forest_md fs]
collect_list_mds = mapM (get_fmd_header . snd)

instance (ForestMD fs md,BuildContainer1 c md) => MDContainer fs (c (FilePath,md)) where
	collect_container_mds = collect_list_mds . toList1
instance (ForestMD fs md,BuildContainer2 c md) => MDContainer fs (c FilePath md) where
	collect_container_mds = collect_list_mds . toList2
	
instance (Eq a,MDContainer fs a) => MDContainer fs (ForestFSThunkI fs a) where
	collect_container_mds t = collect_container_mds =<< inside (fsforce t)

insertRepMDsList :: ForestMD fs md => [(String, ForestO fs (rep,md))] -> ForestO fs ([(String,rep)], [(String,md)])
insertRepMDsList inputs = do 
    let (paths, rep_mdIOs) = unzip inputs
    rep_mds <- sequence rep_mdIOs
    let (reps, mds) = unzip rep_mds
    let repList = zip paths reps
    let mdList = zip paths mds
    return (repList, mdList)

insertRepMDsGeneric1 :: (ForestMD fs b, BuildContainer1 c a, BuildContainer1 c b) => [(FilePath, ForestO fs (a,b))] -> ForestO fs (c (FilePath, a), c (FilePath, b))
insertRepMDsGeneric1 inputs = do 
    (repList, mdList) <- insertRepMDsList inputs
    return (buildContainer1 repList, buildContainer1 mdList)

insertRepMDsGeneric2 :: (ForestMD fs b, BuildContainer2 c a, BuildContainer2 c b) => [(FilePath, ForestO fs (a,b))] -> ForestO fs (c FilePath a, c FilePath b)
insertRepMDsGeneric2 inputs = do 
    (repList, mdList) <- insertRepMDsList inputs
    return (buildContainer2 repList, buildContainer2 mdList)

tyConNameOptBuild :: Maybe String -> Q TH.Exp
tyConNameOptBuild Nothing = return $ VarE 'id
tyConNameOptBuild (Just str) = do
	arity <- getTyConArity str
	case arity of
		1 -> return $ VarE 'buildContainer1
		2 -> return $ VarE 'buildContainer2

tyConNameOptIso :: Maybe String -> Q TH.Exp
tyConNameOptIso Nothing = return $ appE2 (ConE 'Iso) (VarE 'id) (VarE 'id)
tyConNameOptIso (Just str) = do
	arity <- getTyConArity str
	case arity of
		1 -> return $ appE2 (ConE 'Iso) (VarE 'toList1) (VarE 'buildContainer1)
		2 -> return $ appE2 (ConE 'Iso) (VarE 'toList2) (VarE 'buildContainer2)

tyConNameOptInsertRepMDs :: Maybe String -> Q TH.Exp
tyConNameOptInsertRepMDs Nothing = return $ VarE 'insertRepMDsList
tyConNameOptInsertRepMDs (Just str) = do
	arity <- getTyConArity str
	case arity of
		1 -> return $ VarE 'insertRepMDsGeneric1
		2 -> return $ VarE 'insertRepMDsGeneric2

appConE :: Name -> [TH.Exp] -> TH.Exp
appConE con = foldl' AppE (ConE con)

appendStringM str1E str2E = 
   AppE (AppE (VarE '(++)) str1E) str2E

mkStrLitM s = LitE (StringL s)

wrapRepP :: Name -> ForestTy -> TH.Pat -> TH.Pat
wrapRepP repN fty repP = case fty of
  Directory _ -> repP
  otherwise   -> ConP repN [repP]

guardM :: Monad m => m Bool -> m a -> m a -> m a
guardM cond ifTrue ifFalse = cond >>= \b -> if b then ifTrue else ifFalse

returnExp :: TH.Exp -> TH.Exp
returnExp e = AppE (VarE 'return) e

returnExp2 :: TH.Exp -> TH.Exp
returnExp2 e = AppE (VarE 'returnFun2) e

returnFun2 :: Monad m => (a -> b -> c) -> (a -> b -> m c)
returnFun2 f x y = return $ f x y

getPEforFields :: (String -> Name) -> [Field] ->  ([TH.Exp], [TH.FieldPat])
getPEforFields mkFieldNm fields = 
  let eps =  List.map (getPEforField mkFieldNm) fields
      (es, fps) = List.unzip eps
  in (es, fps)

getPEforField :: (String -> Name) -> Field -> (TH.Exp, TH.FieldPat)
getPEforField mkFieldNm field = 
  let fieldStr = case field of
                   Simple (name, _, _, _, _) -> name
                   Comp r -> internalName r
      (varE, varP) = genPE (mkFieldNm fieldStr)
  in  (varE, (mkFieldNm fieldStr, varP))

singleton = (:[])

--genNewTypeIso :: Name -> Q TH.Exp
--genNewTypeIso ty_name = do
--	x <- newName "x"
--	return $ AppE (AppE (VarE 'L.iso) (LamE [ConP ty_name [VarP x]] (VarE x)) ) (ConE ty_name)

emptyUpd = ListE []

tyListToTupleTy (ty:tys) = foldl AppT (AppT (TupleT (1 + length tys) ) ty) tys
mk_newTyD fsName (unty_name,ty_name) ty = NewtypeD [] ty_name [PlainTV fsName] con derives
    where con = RecC ty_name [(unty_name,NotStrict,ty)]           -- How should we determine whether a type should be Strict or not?
          derives = [''Typeable,''Eq]

mk_TySynD fsName ty_name ty = TySynD ty_name [PlainTV fsName] ty

appE2 f x y = AppE (AppE f x) y
appE3 f x y z = AppE (AppE (AppE f x) y) z
appE4 f x y z w = AppE (AppE (AppE (AppE f x) y) z) w
appE5 f x y z w q = AppE (AppE (AppE (AppE (AppE f x) y) z) w) q
appE6 f x y z w q e = AppE (AppE (AppE (AppE (AppE (AppE f x) y) z) w) q) e
appE7 f x y z w q e x1 = AppE (AppE (AppE (AppE (AppE (AppE (AppE f x) y) z) w) q) e) x1
appE8 f x y z w q e x1 x2 = AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE f x) y) z) w) q) e) x1) x2
appE9 f x y z w q e x1 x2 x3 = AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE f x) y) z) w) q) e) x1) x2) x3
appE10 f x y z w q e x1 x2 x3 x4 = AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE f x) y) z) w) q) e) x1) x2) x3) x4
appE11 f x y z w q e x1 x2 x3 x4 x5 = AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE f x) y) z) w) q) e) x1) x2) x3) x4) x5
appE12 f x y z w q e x1 x2 x3 x4 x5 x6 = AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE f x) y) z) w) q) e) x1) x2) x3) x4) x5) x6
appE13 f x y z w q e x1 x2 x3 x4 x5 x6 x7 = AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE f x) y) z) w) q) e) x1) x2) x3) x4) x5) x6) x7
appE14 f x y z w q e x1 x2 x3 x4 x5 x6 x7 x8 = AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE f x) y) z) w) q) e) x1) x2) x3) x4) x5) x6) x7) x8
appE15 f x y z w q e x1 x2 x3 x4 x5 x6 x7 x8 x9 = AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE (AppE f x) y) z) w) q) e) x1) x2) x3) x4) x5) x6) x7) x8) x9

appT2 f x y = AppT (AppT f x) y
appT3 f x y z = AppT (AppT (AppT f x) y) z
appT4 f x y z w = AppT (AppT (AppT (AppT f x) y) z) w

tupT2 :: Type -> Type -> Type
tupT2 t1 t2 = AppT (AppT (TupleT 2) t1) t2

{- Name manipulation functions -}
genUniqueName base = newName base
genRepName = genUniqueName "rep" 
genRepMDName = genUniqueName "rep_md" 
genManName = genUniqueName "manifest" 
genMdName  = genUniqueName "md" 
genBMdName = genUniqueName "bmd"
getMDName pname = mkName ((strToUpper pname) ++ "_md")

getStructInnerName name = 
  let str = show name
  in mkName (str++"_inner")
getStructInnerMDName name = 
  let str = show name
  in mkName (str++"_inner_md")
getFieldMDName str = mkName (str++"_md")
getFieldName str = mkName str

getBranchMDNameU str = mkName ((strToUpper str)++"_md")
getBranchNameU str = mkName (strToUpper str)

getBranchMDNameL str = mkName ((strToLower str)++"_md")
getBranchNameL   str = mkName  (strToLower str)

getTyName pname = mkName  (strToUpper pname)
getUnTyName pname = mkName  ("un"++strToUpper pname)

getWriteManifestName pname = mkName ((strToLower pname) ++ "_updateManifest")
getGenManifestName pname = mkName ((strToLower pname) ++ "_generateManifest")

appTyFS fsName ty_name = AppT (ConT ty_name) (VarT fsName)

secondFourth :: (a,b,c,d) -> (b,d)
secondFourth (x,y,z,w) = (y,w)

mkOptConstraintFun :: Name -> Maybe TH.Exp -> TH.Exp
mkOptConstraintFun field predM = case predM of
	Nothing -> ConE 'Nothing
	Just pred ->
		let predFnE = modPredE (VarP field) pred
		in AppE (ConE 'Just) predFnE

isSameFileName :: String -> String -> Bool
isSameFileName s1 s2 = s1 == s2

anyProxy :: Proxy args
anyProxy = Proxy

idLensE :: TH.Exp
idLensE = appE2 (ConE 'Lens) (VarE 'id) (AppE (VarE 'curry) $ VarE 'snd)

mkOptConstraintFunComp :: Name -> Maybe TH.Exp -> TH.Exp
mkOptConstraintFunComp field predM = case predM of
	Nothing -> constExp $ constExp $ returnExp $ ConE 'True
	Just predE -> modPredEComp (VarP field) predE


genForestTupleNames :: Int -> String -> Q [Name]
genForestTupleNames i str = mapM (\i -> newName (str++show i)) [1..i]

mergeFieldDeltas :: [Name] -> TH.Exp
mergeFieldDeltas [] = returnExp $ ConE 'Id
mergeFieldDeltas ds = appE2 (VarE 'foldl1) (VarE '(>:>)) $ ListE $ map VarE ds	

sndExp :: TH.Exp -> TH.Exp
sndExp = AppE (VarE 'snd)

