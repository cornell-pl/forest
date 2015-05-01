{-# LANGUAGE DataKinds, UndecidableInstances, KindSignatures, ViewPatterns, TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, ConstraintKinds, TypeSynonymInstances, FlexibleInstances #-}

module Language.Forest.IC.CodeGen.Utils where

import Language.Haskell.TH.Syntax
import Language.Forest.FS.FSRep
import Language.Forest.IC.ICRep
import Control.Monad.Incremental as Inc
import Language.Haskell.TH.Syntax as TH
import Language.Forest.Syntax
import Language.Forest.IC.MetaData
import Language.Forest.IC.MetaData
import Language.Forest.Errors
import Control.Monad
--import qualified Language.Forest.CodeGen.Utils as Pure
import Language.Forest.IO.Utils
import Language.Pads.Padsc hiding (lift)
import Language.Forest.IC.BX as BX
import Language.Pads.TH
import Language.Forest.IC.Generic
import Language.Forest.IC.ValueDelta
import Data.Generics
import Language.Haskell.TH.Quote
import Data.List as List

proxyT :: Type -> TH.Exp
proxyT ty = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) ty)

proxyN :: Name -> TH.Exp
proxyN name = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) $ VarT name)

mergeFieldNSDeltas :: [Name] -> TH.Exp
mergeFieldNSDeltas [] = returnExp $ AppE (ConE 'StableVD) $ ConE 'Id
mergeFieldNSDeltas ds = appE2 (VarE 'foldl1) (VarE '(>::>)) $ ListE $ map (VarE) ds

mergeFieldDeltas :: [Name] -> TH.Exp
mergeFieldDeltas [] = returnExp $ ConE 'Id
mergeFieldDeltas ds = appE2 (VarE 'foldl1) (VarE '(>:>)) $ ListE $ map (AppE (VarE 'liftSValueDelta) . VarE) ds

idLensE :: TH.Exp
idLensE = appE2 (ConE 'Lens) (VarE 'id) (AppE (VarE 'curry) $ VarE 'snd)

tyConNameOptIso :: Maybe String -> Q TH.Exp
tyConNameOptIso Nothing = return $ appE2 (ConE 'Iso) (VarE 'id) (VarE 'id)
tyConNameOptIso (Just str) = do
	arity <- getTyConArity str
	case arity of
		1 -> return $ appE2 (ConE 'Iso) (VarE 'toList1) (VarE 'buildContainer1)
		2 -> return $ appE2 (ConE 'Iso) (VarE 'toList2) (VarE 'buildContainer2)
	
buildFieldLens :: Name -> Q TH.Exp
buildFieldLens field = do
	r <- newName "r"
	f <- newName "f"
	let (rE,rP) = genPE r
	let (fE,fP) = genPE f
	let getE = VarE field
	let putE = LamE [rP,fP] $ RecUpdE rE [(field,fE)]
	return $ appE2 (ConE 'Lens) getE putE
	
class (IncK (IncForest fs) FileInfo,IncK (IncForest fs) Forest_err,ICRep fs,ForestInput fs FSThunk Inside) => MDContainer fs md where
	collect_container_mds :: ForestLayer fs l => md -> ForestL fs l [Forest_md fs]
	merge_container_errors :: ForestLayer fs l => md -> ForestL fs l Forest_err
	merge_container_errors md = liftM mergeMDErrors $ mapM get_errors =<< collect_container_mds md

instance (ForestMD fs md,BuildContainer1 c key md) => MDContainer fs (c (key,md)) where
	collect_container_mds = collect_list_mds . toList1
instance (ForestMD fs md,BuildContainer2 c key md) => MDContainer fs (c key md) where
	collect_container_mds = collect_list_mds . toList2
	
instance (IncK (IncForest fs) a,MDContainer fs a) => MDContainer fs (ForestFSThunkI fs a) where
	collect_container_mds t = collect_container_mds =<< inside (Inc.get t)

merge_list_errors :: (ForestLayer fs l,ForestMD fs md) => [(key ,md)] -> ForestL fs l Forest_err
merge_list_errors md = liftM mergeMDErrors $ mapM get_errors =<< collect_list_mds md

collect_list_mds :: (ForestLayer fs l,ForestMD fs md) => [(key,md)] -> ForestL fs l [Forest_md fs]
collect_list_mds = mapM (get_fmd_header . snd)


modPredE externalP predE = case externalP of
  VarP name -> let md_name = mkName ((nameBase name) ++"_md")  
                   attP    = VarP (mkName ((nameBase name) ++"_att"))
                   pat     = TildeP (TupP[VarP name, VarP md_name])
                   initE   = AppE (VarE 'get_info) (VarE md_name)
                   bodyE   = DoE [BindS attP initE,NoBindS predE]      -- let name_att = fileInfo name_md in predE
               in LamE [pat] bodyE            
  otherwise -> error "Forest: Couldn't convert constraint pattern to pattern for meta data.  Use simple pattern."

zmodPredE externalP predE = case externalP of
  VarP name -> let rep_name = mkName ((nameBase name))  
                   attP    = VarP (mkName ((nameBase name) ++"_att"))
                   pat     = TildeP (TupP[VarP name])
                   initE   = AppE (VarE 'get_info) (VarE rep_name)
                   bodyE   = DoE [BindS attP initE,NoBindS predE]      -- let name_att = fileInfo name_md in predE
               in LamE [pat] bodyE            
  otherwise -> error "Forest: Couldn't convert constraint pattern to pattern for meta data.  Use simple pattern."

modPredEComp externalP predE = case externalP of
  VarP name -> let attP    = VarP (mkName ((nameBase name) ++"_att_thunk"))
                   pats     = [VarP name, attP]
                   bodyE   = predE     -- let name_att = fileInfo name_md in predE
               in LamE pats bodyE            
  otherwise -> error "Forest: Couldn't convert constraint pattern to pattern for meta data.  Use simple pattern."

mk_newTyD fsName (unty_name,ty_name) ty = NewtypeD [] ty_name [PlainTV fsName] con derives
    where con = RecC ty_name [(unty_name,NotStrict,ty)]           -- How should we determine whether a type should be Strict or not?
          derives = [''Typeable,''Eq]

mk_newTyDEC needsEC ecName fsName ty_nameEC (unty_name,ty_name) ty = if needsEC
	then NewtypeD [] ty_nameEC [KindedTV ecName (ConT ''EC),KindedTV fsName (ConT ''FS)] con derives
	else NewtypeD [] ty_nameEC [KindedTV fsName (ConT ''FS)] con derives
  where
	con = RecC (ty_name) [(unty_name,NotStrict,ty)]           -- How should we determine whether a type should be Strict or not?
	derives = [''Typeable]

mk_TySynD fsName ty_name ty = TySynD ty_name [PlainTV fsName] ty
mk_TySynDEC ecName fsName ty_name ty = TySynD ty_name [KindedTV ecName (ConT ''EC),KindedTV fsName (ConT ''FS)] ty
mk_TySynDE fsName ty_name ty = TySynD ty_name [KindedTV fsName (ConT ''FS)] ty
mk_TySynDMode fsName ty_name ty = TySynD ty_name [PlainTV fsName] ty

unKind :: Data a => a -> a
unKind = everywhere (mkT unKindTyVarBndr) where
	unKindTyVarBndr :: TyVarBndr -> TyVarBndr
	unKindTyVarBndr (KindedTV n t) = PlainTV n

-- generates a function that collects the fields of all the specifications in a directory and returns a list of @Forest_md@s
genMergeFieldsMDErrors :: [Field] -> Q TH.Exp
genMergeFieldsMDErrors fields = do
	let names_md = map (mkName . (++"_md")) $ fieldnames fields
	let gets_fmd = map genMergeFieldMDErrors fields
	let md = mkName "md"
	let mds = map (\(name_md,get_fmd) -> AppE get_fmd $ AppE (VarE name_md) (VarE md)) $ zip names_md gets_fmd
	return $ LamE [VarP md] $ appE2 (VarE 'liftM) (VarE 'mergeMDErrors) $ AppE (VarE 'sequence) $ ListE mds

zgenMergeFieldsMDErrors :: [Field] -> Q TH.Exp
zgenMergeFieldsMDErrors fields = do
	let names_md = map (mkName) $ fieldnames fields
	let gets_fmd = map genMergeFieldMDErrors fields
	let md = mkName "md"
	let mds = map (\(name_md,get_fmd) -> AppE get_fmd $ AppE (VarE name_md) (VarE md)) $ zip names_md gets_fmd
	return $ LamE [VarP md] $ appE2 (VarE 'liftM) (VarE 'mergeMDErrors) $ AppE (VarE 'sequence) $ ListE mds

genMergeFieldMDErrors :: Field -> TH.Exp
genMergeFieldMDErrors (Simple _) = VarE 'get_errors
genMergeFieldMDErrors (Comp compField) = VarE 'merge_container_errors
	
zgenMergeFieldMDErrors :: Field -> TH.Exp
zgenMergeFieldMDErrors (Simple _) = VarE 'get_errors
zgenMergeFieldMDErrors (Comp compField) = VarE 'merge_container_errors



	
appTyModeFS mode fsName ty_name = appT2 (ConT ty_name) (PromotedT mode) (VarT fsName)
appTyModeFS' modeName fsName ty_name = appT2 (ConT ty_name) (VarT modeName) (VarT fsName)
appTyModeFS'' modeT fsName ty_name = appT2 (ConT ty_name) modeT (VarT fsName)

allMDArgs :: Data a => a -> [Type]
allMDArgs = listify mdArg where
	mdArg :: Type -> Bool
	mdArg ty@(AppT (AppT (AppT (ConT ((==''MDArgs) -> True)) mode) md) args) = True
	mdArg _ = False

allTypes :: Data a => a -> [Type]
allTypes = listify ty where
	ty :: Type -> Bool
	ty x = True

removePred :: Data a => (Pred -> Bool) -> a -> a
removePred p = everywhere (mkT remP) where
	remP :: [Pred] -> [Pred]
	remP ps = filter (not . p) ps
	
replaceType :: Data a => (Type -> Type) -> a -> a
replaceType f = everywhere (mkT f)


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
		AppE (VarE ((=='return) -> True)) (VarE name) -> name
		e -> error $ "getCompName: " ++ show e

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
		AppE (VarE ((=='return) -> True)) (VarE name) ->
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
  AppE (VarE ((=='return) -> True)) (VarE name) ->
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
  AppE (VarE ((=='return) -> True)) (VarE name) -> TildeP (TupP[VarP name, VarP (mkName ((nameBase name) ++"_att"))])
  otherwise -> error "Forest: Couldn't convert file expression to pattern; please supply an explicit name with 'name as exp' form"

getRepMDPat explicitName externalE = 
  case explicitName of 
      Just str -> TildeP (TupP[VarP (mkName str), VarP (mkName (str++"_md"))])
      Nothing -> getRepMDPatFromExp externalE

getRepMDPatFromExp externalE = case externalE of
  VarE name -> TildeP (TupP[VarP name, VarP (mkName ((nameBase name) ++"_md"))])
  otherwise -> error "Forest: Couldn't convert file expression to pattern; please supply an explicit name with 'name as exp' form"





forestTupleTy :: [TH.Type] -> TH.Type
forestTupleTy ts = foldl1' (appT2 (ConT ''(:*:))) ts

forestTupleP :: [Pat] -> Pat
forestTupleP = foldl1' (\p1 p2 -> UInfixP p1 ('(:*:)) p2)

forestTupleE :: [Exp] -> Exp
forestTupleE = foldl1' (\e1 e2 -> InfixE (Just e1) (ConE '(:*:)) (Just e2))


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




tyConNameOptBuild :: Maybe String -> Q TH.Exp
tyConNameOptBuild Nothing = return $ VarE 'id
tyConNameOptBuild (Just str) = do
	arity <- getTyConArity str
	case arity of
		1 -> return $ VarE 'buildContainer1
		2 -> return $ VarE 'buildContainer2

tyConNameOptToList :: Maybe String -> Q TH.Exp
tyConNameOptToList Nothing = return $ VarE 'id
tyConNameOptToList (Just str) = do
	arity <- getTyConArity str
	case arity of
		1 -> return $ VarE 'toList1
		2 -> return $ VarE 'toList2

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
appT5 f x y z w q = AppT (AppT (AppT (AppT (AppT f x) y) z) w) q

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
getRepTyName pname = mkName ((strToUpper pname) ++ "_rep")
getRepUnTyName pname = mkName  ("un"++strToUpper pname ++ "_rep")

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

anyProxy :: Proxy args
anyProxy = Proxy

anyFSProxy :: Proxy (fs :: FS)
anyFSProxy = Proxy

mkOptConstraintFunComp :: Name -> Maybe TH.Exp -> TH.Exp
mkOptConstraintFunComp field predM = case predM of
	Nothing -> constExp $ constExp $ returnExp $ ConE 'True
	Just predE -> modPredEComp (VarP field) predE


genForestTupleNames :: Int -> String -> Q [Name]
genForestTupleNames i str = mapM (\i -> newName (str++show i)) [1..i]

sndExp :: TH.Exp -> TH.Exp
sndExp = AppE (VarE 'snd)