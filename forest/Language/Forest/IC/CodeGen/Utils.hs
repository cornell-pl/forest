{-# LANGUAGE KindSignatures, ViewPatterns, TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, ConstraintKinds, TypeSynonymInstances, FlexibleInstances #-}

module Language.Forest.IC.CodeGen.Utils where

import Language.Haskell.TH.Syntax
import Language.Forest.FS.FSRep
import Language.Forest.IC.ICRep
import Control.Monad.Incremental as Inc
import Language.Haskell.TH.Syntax as TH
import Language.Forest.Syntax
import Language.Forest.IC.MetaData
import qualified Language.Forest.Pure.MetaData as Pure
import Language.Forest.Errors
import Control.Monad
import qualified Language.Forest.Pure.CodeGen.Utils as Pure
import Language.Forest.IO.Utils
import Language.Pads.Padsc hiding (lift)
import Language.Forest.IC.BX as BX
import Language.Pads.TH
import Language.Forest.IC.Generic
import Language.Forest.IC.ValueDelta
import Data.Generics
import Language.Haskell.TH.Quote

proxyN :: Name -> TH.Exp
proxyN name = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) $ VarT name)

mergeFieldDeltas :: [Name] -> TH.Exp
mergeFieldDeltas [] = Pure.returnExp $ ConE 'Id
mergeFieldDeltas ds = Pure.appE2 (VarE 'foldl1) (VarE '(>:>)) $ ListE $ map (AppE (VarE 'liftSValueDelta) . VarE) ds

idLensE :: TH.Exp
idLensE = Pure.appE2 (ConE 'Lens) (VarE 'id) (AppE (VarE 'curry) $ VarE 'snd)

tyConNameOptIso :: Maybe String -> Q TH.Exp
tyConNameOptIso Nothing = return $ Pure.appE2 (ConE 'Iso) (VarE 'id) (VarE 'id)
tyConNameOptIso (Just str) = do
	arity <- Pure.getTyConArity str
	case arity of
		1 -> return $ Pure.appE2 (ConE 'Iso) (VarE 'toList1) (VarE 'buildContainer1)
		2 -> return $ Pure.appE2 (ConE 'Iso) (VarE 'toList2) (VarE 'buildContainer2)
	
buildFieldLens :: Name -> Q TH.Exp
buildFieldLens field = do
	r <- newName "r"
	f <- newName "f"
	let (rE,rP) = genPE r
	let (fE,fP) = genPE f
	let getE = VarE field
	let putE = LamE [rP,fP] $ RecUpdE rE [(field,fE)]
	return $ Pure.appE2 (ConE 'Lens) getE putE
	
class (ICRep fs,ForestInput fs FSThunk Inside) => MDContainer fs md where
	collect_container_mds :: ForestLayer fs l => md -> ForestL fs l [Forest_md fs]
	merge_container_errors :: ForestLayer fs l => md -> ForestL fs l Forest_err
	merge_container_errors md = liftM Pure.mergeMDErrors $ mapM get_errors =<< collect_container_mds md

instance (ForestMD fs md,BuildContainer1 c md) => MDContainer fs (c (FilePath,md)) where
	collect_container_mds = collect_list_mds . toList1
instance (ForestMD fs md,BuildContainer2 c md) => MDContainer fs (c FilePath md) where
	collect_container_mds = collect_list_mds . toList2
	
instance (Eq a,MDContainer fs a) => MDContainer fs (ForestFSThunkI fs a) where
	collect_container_mds t = collect_container_mds =<< inside (Inc.get t)

merge_list_errors :: (ForestLayer fs l,ForestMD fs md) => [(FilePath,md)] -> ForestL fs l Forest_err
merge_list_errors md = liftM Pure.mergeMDErrors $ mapM get_errors =<< collect_list_mds md

collect_list_mds :: (ForestLayer fs l,ForestMD fs md) => [(FilePath,md)] -> ForestL fs l [Forest_md fs]
collect_list_mds = mapM (get_fmd_header . snd)


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

mk_newTyD fsName (unty_name,ty_name) ty = NewtypeD [] ty_name [PlainTV fsName] con derives
    where con = RecC ty_name [(unty_name,NotStrict,ty)]           -- How should we determine whether a type should be Strict or not?
          derives = [''Typeable,''Eq]

mk_TySynD fsName ty_name ty = TySynD ty_name [PlainTV fsName] ty
mk_TySynDMode modeName fsName ty_name ty = TySynD ty_name [KindedTV modeName (ConT ''ICMode),PlainTV fsName] ty

-- generates a function that collects the fields of all the specifications in a directory and returns a list of @Forest_md@s
genMergeFieldsMDErrors :: [Field] -> Q TH.Exp
genMergeFieldsMDErrors fields = do
	let names_md = map (mkName . (++"_md")) $ fieldnames fields
	let gets_fmd = map genMergeFieldMDErrors fields
	let md = mkName "md"
	let mds = map (\(name_md,get_fmd) -> AppE get_fmd $ AppE (VarE name_md) (VarE md)) $ zip names_md gets_fmd
	return $ LamE [VarP md] $ Pure.appE2 (VarE 'liftM) (VarE 'Pure.mergeMDErrors) $ AppE (VarE 'sequence) $ ListE mds

genMergeFieldMDErrors :: Field -> TH.Exp
genMergeFieldMDErrors (Simple _) = VarE 'get_errors
genMergeFieldMDErrors (Comp compField) = case tyConNameOpt compField of
	Nothing -> VarE 'merge_list_errors
	Just str -> VarE 'merge_container_errors

modeT :: ICMode -> Type
modeT = PromotedT . modeN

modeN :: ICMode -> Name
modeN ICExpr = 'ICExpr
modeN ICData = 'ICData

modeProxy :: ICMode -> Exp
modeProxy mode = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) $ modeT mode) 
	
appTyModeFS mode fsName ty_name = Pure.appT2 (ConT ty_name) (PromotedT mode) (VarT fsName)
appTyModeFS' modeName fsName ty_name = Pure.appT2 (ConT ty_name) (VarT modeName) (VarT fsName)
appTyModeFS'' modeT fsName ty_name = Pure.appT2 (ConT ty_name) modeT (VarT fsName)

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


