{-# LANGUAGE DataKinds, ConstraintKinds, TypeFamilies, OverlappingInstances, TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, ViewPatterns  #-}

{-
** *********************************************************************
*                                                                      *
*              This software is part of the pads package               *
*           Copyright (c) 2005-2011 AT&T Knowledge Ventures            *
*                      and is licensed under the                       *
*                        Common Public License                         *
*                      by AT&T Knowledge Ventures                      *
*                                                                      *
*                A copy of the License is available at                 *
*                    www.padsproj.org/License.html                     *
*                                                                      *
*  This program contains certain software code or other information    *
*  ("AT&T Software") proprietary to AT&T Corp. ("AT&T").  The AT&T     *
*  Software is provided to you "AS IS". YOU ASSUME TOTAL RESPONSIBILITY*
*  AND RISK FOR USE OF THE AT&T SOFTWARE. AT&T DOES NOT MAKE, AND      *
*  EXPRESSLY DISCLAIMS, ANY EXPRESS OR IMPLIED WARRANTIES OF ANY KIND  *
*  WHATSOEVER, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF*
*  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, WARRANTIES OF  *
*  TITLE OR NON-INFRINGEMENT.  (c) AT&T Corp.  All rights              *
*  reserved.  AT&T is a registered trademark of AT&T Corp.             *
*                                                                      *
*                   Network Services Research Center                   *
*                          AT&T Labs Research                          *
*                           Florham Park NJ                            *
*                                                                      *
*              Kathleen Fisher <kfisher@research.att.com>              *
*                                                                      *
************************************************************************
-}

module Language.Forest.IC.CodeGen where

import Data.Derive.Memo
import Data.Unique
import Data.Derive.Eq
import Data.Generics.Aliases as SYB
import Data.Generics as SYB
import Language.Forest.IC.CodeGen.ZDeltaStoring
import GHC.Exts
import Data.Foldable as Foldable
import Control.Monad.Trans
import Language.Forest.IC.Default
import Language.Forest.IC.IO.ZDefault
--import qualified Language.Forest.CodeGen.Utils as Pure
import Language.Forest.IC.ICRep
import Data.DeepTypeable
import Language.Forest.IC.BX as BX
import Data.WithClass.Derive.MData
import Language.Forest.IC.IO.ZLoading
import Language.Forest.IC.IO.ZDeltaLoading
import Language.Forest.IC.IO.ZStoring
import Language.Forest.IC.IO.ZDeltaStoring
import Data.IORef
import Data.WithClass.Derive.DeepTypeable
import Control.Monad.Incremental
import Language.Forest.IC.ValueDelta
import Language.Forest.Syntax as PS
import Language.Forest.IC.MetaData
import Language.Forest.Errors
import Language.Forest.IC.Generic as IC
import qualified Language.Forest.Errors as E
--import Language.Forest.MetaData (FileInfo(..),FileType(..),(:*:)(..),Arg(..))
import Language.Forest.Manifest
import Language.Forest.FS.FSDelta
import System.Directory
import System.FilePath.Posix
import Data.DeriveTH
import Data.WithClass.MData

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Pads.Padsc hiding (lift)
import Language.Pads.TH
import Language.Forest.IO.Utils
import Language.Forest.TH
import Language.Forest.FS.FSRep

import Data.Data as SYB
import Data.WithClass.MData
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
import Control.Monad.Reader (Reader(..),ReaderT(..))
import qualified Control.Monad.Reader as Reader

import Language.Forest.IC.CodeGen.Utils
--import Language.Forest.IC.CodeGen.Loading
import Language.Forest.IC.CodeGen.ZLoading
--import Language.Forest.IC.CodeGen.Default
import Language.Forest.IC.CodeGen.ZDefault
import Data.Maybe
--import Language.Forest.IC.CodeGen.DeltaLoading
import Language.Forest.IC.CodeGen.ZDeltaLoading
--import Language.Forest.IC.CodeGen.Storing
import Language.Forest.IC.CodeGen.ZStoring

type GenQ = StateT [Pred] Q

make_zforest_declarations :: [ForestDecl] -> Q [Dec]
make_zforest_declarations ds = make_zforest_declarations' ds Nothing

make_zforest_declarations' :: [ForestDecl] -> Maybe [Type] -> Q [Dec]
make_zforest_declarations' ds mb_fsTy = do
--	ds' <- liftM List.concat $ mapM splitForestDecl ds
	fmap List.concat $ mapM (flip make_zforest_declaration mb_fsTy) ds

make_zforest_declaration :: ForestDecl -> Maybe [Type] -> Q [Dec]
make_zforest_declaration (ForestDecl (isTop,id, pats, forestTy)) mb_fsTy = do
	let ty_name = getTyName id
	ecName <- newName "ec"
	fsName <- newName "fs"
	let unty_name = getUnTyName id
	let arg_infos = map (\pat -> (pat,patToTy pat)) pats
	((ty_decl,aux_decls),ks) <- State.runStateT (genZRepMDDecl isTop ecName fsName forestTy (unty_name,ty_name) arg_infos) [] -- Generate representation and meta-data decls for padsTy

	loadM :: Exp <- Reader.runReaderT (genZLoadM isTop ty_name forestTy arg_infos) (fsName,Map.empty)
	loadDeltaM :: Exp <- Reader.runReaderT (genZLoadDeltaM isTop (unty_name,ty_name) forestTy arg_infos) (fsName,Map.empty)
	manifestM :: Exp <- Reader.runReaderT (genZManifestM isTop ty_name forestTy arg_infos) (fsName,Map.empty)
	manifestDeltaM :: Exp <- Reader.runReaderT (genZManifestDeltaM isTop ty_name forestTy arg_infos) (fsName,Map.empty)
	defaultM :: Exp <- Reader.runReaderT (genZDefaultM isTop ty_name forestTy arg_infos) (fsName,Map.empty)
	
	inst <- genZFInst isTop mb_fsTy ecName fsName  loadM loadDeltaM manifestM manifestDeltaM defaultM ty_name      forestTy arg_infos ks

	let args_ty = case arg_infos of
		[] -> TupleT 0
		otherwise -> forestTupleTy $ map (AppT (ConT ''Arg) . snd) arg_infos
	let proxyArgs_alias = FunD (mkName $ "proxyZArgs_"++nameBase ty_name) [Clause [] (NormalB $ proxyT args_ty) []]

	return $ ty_decl:aux_decls++inst++[proxyArgs_alias]

genZFInst :: Bool -> Maybe [Type] -> Name -> Name -> Exp -> Exp -> Exp -> Exp -> Exp -> Name -> ForestTy -> [(Pat,Type)] -> [Pred] -> Q [Dec]
genZFInst isTop mb_fsTy ecName fsName loadM loadDeltaM manifestM manifestDeltaM defaultM ty_name forestTy pat_infos ks = do
	repName <- newName "rep"
	let tyName = AppT (ConT ty_name) (VarT fsName)
	let (arg_ty,inst) = case pat_infos of
		[] -> (TupleT 0,appT2 (ConT ''ZippedICForest) (VarT fsName) tyName )  
		otherwise -> (forestTupleTy $ map (AppT (ConT ''Arg) . snd) pat_infos,appT2 (ConT ''ZippedICForest) (VarT fsName) tyName )
	let args_ctx = flip map pat_infos $ \(_,ty) -> ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),ty]
	let diff_value_method = ValD (VarP 'diffValue) (NormalB $ if isTop then VarE 'diffValueThunk else VarE 'diffValueAny) [] 
	let load_method = ValD (VarP 'zloadScratch) (NormalB loadM) []
	let load_method_gen = ValD (VarP 'zloadScratchGeneric) (NormalB $ if isTop then VarE 'zloadScratchMemo else VarE 'zloadScratch) [] 
	let loadDelta_method = ValD (VarP 'zloadDelta) (NormalB loadDeltaM) []
	let loadDelta_method_gen = ValD (VarP 'zloadDeltaGeneric) (NormalB $ if isTop then VarE 'zloadDeltaMemo else VarE 'zloadDelta) [] 
	let manifest_method = ValD (VarP 'zupdateManifestScratch) (NormalB manifestM) []
	let manifest_method_gen = ValD (VarP 'zupdateManifestScratchGeneric) (NormalB $ if isTop then VarE 'zupdateManifestScratchMemo else VarE 'zupdateManifestScratch) [] 
	let manifestD_method = ValD (VarP 'zupdateManifestDelta) (NormalB manifestDeltaM) []
	let manifestD_method_gen = ValD (VarP 'zupdateManifestDeltaGeneric) (NormalB $ if isTop then VarE 'zupdateManifestDeltaMemo else VarE 'zupdateManifestDelta) [] 
	let default_method = ValD (VarP 'zdefaultScratch) (NormalB defaultM) []
	let default_method_gen = ValD (VarP 'zdefaultScratchGeneric) (NormalB $ if isTop then VarE 'zdefaultScratchMemo else VarE 'zdefaultScratch) [] 
	let forestArgs = TySynInstD ''ForestArgs $ TySynEqn [tyName] arg_ty
	
	let mkInst mb_ty =
		let ctx = args_ctx ++ ks ++ (if isTop then [ClassP ''FTKK [VarT fsName,tyName]] else []) ++ [
		      ClassP ''Forest [VarT fsName]
		     ,ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),tyName]
		     ,ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),TupleT 0]
		     ,ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),ConT ''Forest_err]
		     ,ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),ConT ''FilePath]
		     ,ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),ConT ''FileInfo]
		     ,ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),AppT (ConT ''Forest_md) (VarT fsName)]
		     ,ClassP ''Typeable [VarT fsName],ClassP ''ZippedICMemo [VarT fsName]
		     ]
		    instD = InstanceD ctx inst [diff_value_method,load_method,load_method_gen,loadDelta_method,loadDelta_method_gen,manifest_method,manifest_method_gen,manifestD_method,manifestD_method_gen,default_method,default_method_gen]
		    instD' = SYB.everywhere (SYB.mkT $ \(t::Type) -> if t == VarT ecName then (PromotedT 'E) else t) instD
		in case mb_ty of
			Nothing -> [forestArgs,instD']
			Just ty -> [forestArgs,SYB.everywhere (SYB.mkT $ \(t::Type) -> if t == VarT fsName then ty else t) instD']
	
	case mb_fsTy of
		Nothing -> return $ mkInst Nothing
		Just tys -> return $ List.concatMap (mkInst . Just) tys

-- * Zipped representation/metadata

-- | Generates representation and metadata type declarations for a Forest declaration
genZRepMDDecl :: Bool -> Name -> Name -> ForestTy -> (Name,Name) -> [(Pat,Type)] -> GenQ (Dec, [Dec])
genZRepMDDecl isTop ecName fsName ty (unty_name,ty_name) pat_infos = case ty of
	Directory dirTy -> genZRepMDDir isTop False ecName fsName dirTy ty_name pat_infos
	(FConstraint _ (Directory dirTy) _) -> genZRepMDDir isTop True ecName fsName dirTy ty_name pat_infos
	otherwise -> do
		let tyNameE = mkName $ nameBase ty_name ++ "E"
		let tyNameC = mkName $ nameBase ty_name ++ "C"
		let tyNameEC = mkName $ nameBase ty_name ++ "EC"
		rep <- genZRepMDTy isTop ecName fsName ty
		let repE = SYB.everywhere (SYB.mkT $ \(t::Type) -> if t == VarT ecName then (PromotedT 'E) else t) rep
		let repC = SYB.everywhere (SYB.mkT $ \(t::Type) -> if t == VarT ecName then (PromotedT 'C) else t) rep
		let needsEC = hasVar ecName rep
		let ty_decEC = mk_newTyDEC needsEC ecName fsName tyNameEC (unty_name,ty_name) rep
		let ty_decECK = unKind ty_decEC
		
		let ty_dec = if needsEC
			then TySynD ty_name [KindedTV fsName (ConT ''FS)] $ appT2 (ConT tyNameEC) (PromotedT 'E) (VarT fsName)
			else TySynD ty_name [KindedTV fsName (ConT ''FS)] $ AppT (ConT tyNameEC) (VarT fsName)
		let ty_decC = if needsEC
			then TySynD tyNameC [KindedTV fsName (ConT ''FS)] $ appT2 (ConT tyNameEC) (PromotedT 'C) (VarT fsName)
			else TySynD tyNameC [KindedTV fsName (ConT ''FS)] $ AppT (ConT tyNameEC) (VarT fsName)
		mdataInstance <- lift $ deriveFromDec makeMData ty_decECK
		deepTypeableInstance <- lift $ deriveFromDec makeDeepTypeable ty_decECK
		forestRepInstance <- if isTop
			then lift $ mkNewTypeForestRepEC needsEC ecName fsName tyNameEC tyNameE tyNameC (unty_name,ty_name) (unthunkTy rep)
			else lift $ mkNewTypeForestMDEC needsEC ecName fsName tyNameEC tyNameE tyNameC (unty_name,ty_name) rep
		memoInstance <- lift $ deriveMemoFromDec ty_decECK
		eqInstance <- lift (makeForestECClass makeEq ''Eq ecName fsName ty_decECK) 
		ordInstance <- lift (makeForestECClass makeOrd ''Ord ecName fsName ty_decECK)
		let tyEC = if needsEC
			then appT2 (ConT tyNameEC) (VarT ecName) (VarT fsName)
			else AppT (ConT tyNameEC) (VarT fsName)
		let tyE = if needsEC
			then appT2 (ConT tyNameEC) (PromotedT 'E) (VarT fsName)
			else AppT (ConT tyNameEC) (VarT fsName)
		let tyC = if needsEC
			then appT2 (ConT tyNameEC) (PromotedT 'C) (VarT fsName)
			else AppT (ConT tyNameEC) (VarT fsName)
		-- identity or a lens
		let ms = mkName "ms"
		let mv = mkName "mv"
		let getE = LamE [VarP ms] $ appE2 (VarE 'liftM) (ConE ty_name) $ AppE (AppE (VarE 'getM) (VarE 'lens_content)) $ appE2 (VarE 'liftM) (VarE unty_name) (VarE ms)
		let putE = LamE [VarP ms,VarP mv] $ appE2 (VarE 'liftM) (ConE ty_name) $ appE2 (AppE (VarE 'putM) (VarE 'lens_content)) (appE2 (VarE 'liftM) (VarE unty_name) (VarE ms)) (appE2 (VarE 'liftM) (VarE unty_name) (VarE mv))
		let forestContentInstance = if isTop
			then [InstanceD [ClassP ''ICRep [VarT fsName],ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),ConT ''FileInfo]] (appT2 (ConT ''ForestContent) (VarT fsName) tyEC) [
				 TySynInstD ''ForestContentTy $ TySynEqn [VarT fsName,tyEC] tyEC
				,ValD (VarP 'lens_content) (NormalB $ VarE 'idLensM) []]]
			else [InstanceD [ClassP ''ICRep [VarT fsName],ClassP ''ForestContent [VarT fsName,repE]] (appT2 (ConT ''ForestContent) (VarT fsName) tyE) [
				 TySynInstD ''ForestContentTy $ TySynEqn [VarT fsName,tyE] tyC
				,ValD (VarP 'lens_content) (NormalB $ appE2 (ConE 'LensM) getE putE) []]]
		return (ty_decEC, ty_decC:ty_dec:forestRepInstance++mdataInstance++deepTypeableInstance++forestContentInstance++memoInstance++eqInstance++ordInstance)

{- Generate a representation and meta-data type for maybe. -}
genZRepMDMaybe :: Bool -> Name -> Name -> ForestTy -> GenQ Type
genZRepMDMaybe isTop ecName fsName ty = do
	rep_orig <- genZRepMDTy False ecName fsName ty
	let rep_ty = AppT (ConT ''Maybe) rep_orig                 -- rep is Maybe ty where ty is rep of nested type
	let rep_ty'  = appT3 (ConT ''ECMd) (VarT ecName) (VarT fsName) rep_ty  -- md is a pair of a base md for the maybe and the underlying md.
	if isTop
		then fsthunkTyQ fsName rep_ty'
		else return rep_ty

hasVar :: Data a => Name -> a -> Bool
hasVar n = SYB.everything (||) (SYB.mkQ False $ \(t::Type) -> if t == VarT n then True else False)

{- Generate a representation and meta-data type for a directory with named fields. -}
genZRepMDDir :: Bool -> Bool -> Name -> Name -> DirectoryTy -> Name ->  [(Pat,Type)] -> GenQ (Dec, [Dec])
genZRepMDDir isTop hasConstraint ecName fsName (Record _ fields) ty_name pat_infos = do
	reps <- mapM (genZRepMDField ecName fsName) fields
	let ty_nameEC = mkName $ nameBase ty_name ++ "EC"
	let ty_nameC = mkName $ nameBase ty_name ++ "C"
	let inner_ty_name = getStructInnerName ty_name
	let inner_ty_nameEC = mkName $ nameBase inner_ty_name ++ "EC"
	let derives      = [''Typeable]
	let needsEC = hasVar ecName reps
	let ty_con       = TH.RecC inner_ty_name reps
	let inner_ty_decl      = if needsEC
		then DataD [] inner_ty_nameEC [KindedTV ecName (ConT ''EC),KindedTV fsName (ConT ''FS)] [ty_con] derives
		else DataD [] inner_ty_nameEC [KindedTV fsName (ConT ''FS)] [ty_con] derives
	let ty = if needsEC
		then appT3 (ConT ''ECMd) (VarT ecName) (VarT fsName) $ appT2 (ConT inner_ty_nameEC) (VarT ecName) (VarT fsName)
		else appT3 (ConT ''ECMd) (VarT ecName) (VarT fsName) $ AppT (ConT inner_ty_nameEC) (VarT fsName)
	let tyE = if needsEC
		then tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName) , appT2 (ConT inner_ty_nameEC) (PromotedT 'E) (VarT fsName) ]
		else tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName) , AppT (ConT inner_ty_nameEC) (VarT fsName) ]
	let tyC = if needsEC
		then tyListToTupleTy [(ConT ''FileInfo) , appT2 (ConT inner_ty_nameEC) (PromotedT 'C) (VarT fsName) ]
		else tyListToTupleTy [(ConT ''FileInfo) , AppT (ConT inner_ty_nameEC) (VarT fsName) ]
	ty' <- if hasConstraint
		then return $ appT3 (ConT ''ECErr) (VarT ecName) (VarT fsName) ty
		else return ty
	ty'' <- if isTop then fsthunkTyQ fsName ty' else return ty'
	tyE' <- if hasConstraint
		then do
			err_ty <- fsthunkTyQ fsName (ConT ''Forest_err)
			return $ tyListToTupleTy [ err_ty ,tyE ]
		else return tyE
	tyE'' <- if isTop then fsthunkTyQ fsName tyE' else return tyE'
	tyC' <- if isTop then fsthunkTyQ fsName tyC else return tyC
	let ty_declEC = mk_TySynDEC ecName fsName ty_nameEC ty''
	let ty_decl = mk_TySynDE fsName ty_name tyE''
	let ty_declC = mk_TySynDE fsName ty_nameC $ tyC'
	
	State.modify $ \xs -> ClassP ''IncK [AppT (ConT ''IncForest) $ VarT fsName,tyE] : xs
	
	if length reps == 0
		then error ("Error: Directory " ++ (show ty_name) ++ " must contain at least one named field.")
		else do
			let inner_ty_declK = unKind inner_ty_decl
			mdataInstance_rep <- lift $ deriveFromDec makeMData inner_ty_declK
			deepTypeableRepInstance <- lift $ deriveFromDec makeDeepTypeable inner_ty_declK
			forestContent <- mkDirForestContentEC needsEC fsName ty_name inner_ty_name inner_ty_nameEC fields
			memoInstance <- lift $ deriveMemoFromDec inner_ty_declK
			eqInstance <- lift (makeForestECClass makeEq ''Eq ecName fsName inner_ty_declK)
			ordInstance <- lift (makeForestECClass makeOrd ''Ord ecName fsName inner_ty_declK)
			return (ty_declEC, (ty_decl:ty_declC:inner_ty_decl:memoInstance++eqInstance++ordInstance++mdataInstance_rep++deepTypeableRepInstance++forestContent))

makeForestECClass :: Derivation -> Name -> Name -> Name -> Dec -> Q [Dec]
makeForestECClass drv cls ecName fsName dec = do
	[InstanceD ctx ty decs] <- deriveFromDec drv dec
	case ty of
		AppT eq (AppT (AppT con ec) fs) -> do
			let ctx' = everything (++) (mkQ [] (collectECs ec fs)) dec
			let ctx'' = ClassP ''ICRep [fs] : ctx'
			return [InstanceD ctx'' ty decs]
		AppT eq (AppT con fs) -> do
			let ctx'' = ClassP ''ICRep [fs] : []
			return [InstanceD ctx'' ty decs]
  where
	collectECs :: Type -> Type -> Type -> [Pred]
	collectECs ec fs t@(AppT (AppT (AppT (ConT ((==''ECErr) -> True)) _) _) ity) = [ClassP cls [everywhere (mkT $ replaceECFS ec fs) t]]
	collectECs ec fs t@(AppT (AppT (AppT (ConT ((==''ECMd) -> True)) _) _) ity) = [ClassP cls [everywhere (mkT $ replaceECFS ec fs) t]]
	collectECs ec fs t = []
	replaceECFS :: Type -> Type -> Type -> Type
	replaceECFS ec fs (VarT ((==ecName) -> True)) = ec
	replaceECFS ec fs (VarT ((==fsName) -> True)) = fs
	replaceECFS ec fs t = t

mkDirForestContentEC :: Bool -> Name -> Name -> Name -> Name -> [Field] -> GenQ [Dec]
mkDirForestContentEC needsEC fsName ty_name inner_ty_name inner_ty_nameEC fields = do
	let tyE = if needsEC
		then appT2 (ConT inner_ty_nameEC) (PromotedT 'E) $ VarT fsName
		else AppT (ConT inner_ty_nameEC) $ VarT fsName
	let tyC = if needsEC
		then appT2 (ConT inner_ty_nameEC) (PromotedT 'C) $ VarT fsName
		else AppT (ConT inner_ty_nameEC) $ VarT fsName
	let (es,map snd -> ps) = getPEforFields mkName fields
	let (es',map snd -> ps') = getPEforFields (mkName . (++"'")) fields
	let (es'',map snd -> ps'') = getPEforFields (mkName . (++"''")) fields
	let ms = mkName "ms"
	let mv = mkName "mv"
	let lns_get = LamE [VarP ms] $ DoE $
		[BindS (SigP (ConP inner_ty_name ps) tyE) (VarE ms)] ++
		map (\(s,v) -> BindS v $ appE2 (VarE 'BX.getM) (VarE 'lens_content) (returnExp s)) (zip es ps') ++
		[NoBindS $ returnExp $ SigE (List.foldl AppE (ConE inner_ty_name) es') tyC ]	
	let lns_put = LamE [VarP ms,VarP mv] $ DoE $
		[BindS (SigP (ConP inner_ty_name ps) tyE) (VarE ms),BindS (SigP (ConP inner_ty_name ps') tyC) (VarE mv)] ++
		map (\(s,v,s') -> BindS s' $ appE3 (VarE 'BX.putM) (VarE 'lens_content) (returnExp s) (returnExp v) ) (zip3 es es' ps'') ++
		[NoBindS $ returnExp $ SigE (List.foldl AppE (ConE inner_ty_name) es'') tyE ]	
	let lns = appE2 (ConE 'LensM) lns_get lns_put
	let fun = FunD 'lens_content [Clause [] (NormalB lns) []]
	let dec = InstanceD [ClassP ''ICRep [VarT fsName],ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),ConT ''FileInfo]] (appT2 (ConT ''ForestContent) (VarT fsName) tyE) [
		 TySynInstD ''ForestContentTy $ TySynEqn [VarT fsName,tyE] tyC
		,fun]
	return [dec]

genZRepMDField :: Name -> Name -> Field -> GenQ (VST)
genZRepMDField ecName fsName (Simple (internal, isForm, external, ty, predM)) = do
	(rep_ty) <- genZRepMDTy False ecName fsName ty
	let rep_ty' = case predM of
		Nothing -> rep_ty
		Just _ -> appT3 (ConT ''ECErr) (VarT ecName) (VarT fsName) rep_ty
	State.modify $ \xs ->
		ClassP ''ForestMD [VarT fsName,rep_ty'] :
		ClassP ''MData [ConT ''NoCtx,AppT (ConT ''ForestI) (VarT fsName),rep_ty] : xs
	return (getFieldName   internal, TH.NotStrict, rep_ty')
genZRepMDField ecName fsName (Comp (info @ CompField {internalName, tyConNameOpt, descTy, ..})) = do
	(rep_ty) <- genZRepMDComp ecName fsName info
	return (getFieldName   internalName, TH.NotStrict, {-fsthunkTyQ fsName -} rep_ty)

genZRepMDComp :: Name -> Name -> CompField -> GenQ Type
genZRepMDComp ecName fsName (CompField {internalName, tyConNameOpt, descTy, generatorTy, predEOpt, ..}) = do
	(rng_rep_ty) <- genZRepMDTy False ecName fsName descTy
	let key_ty = case generatorTy of
		Just (key_ty_name,key_arg) -> ConT $ getTyName key_ty_name
		Nothing -> ConT ''String
	(rep_ty) <- case tyConNameOpt of 
		Nothing ->  return (mkKeyListTy key_ty rng_rep_ty)
		Just str -> do
			arity <- lift $ getTyConArity str
			case arity of 
				1 -> return (mkKeyConTupleTy (mkName str) key_ty rng_rep_ty) 
				2 -> return (mkKeyConCurryTy (mkName str) key_ty rng_rep_ty) 
	State.modify $ \xs -> 
		ClassP ''IncK [AppT (ConT ''IncForest) $ VarT fsName,key_ty] :
		ClassP ''MData [ConT ''NoCtx,AppT (ConT ''ForestI) $ VarT fsName,rng_rep_ty] :
		ClassP ''ForestMD [VarT fsName,rng_rep_ty] : 
		ClassP ''IncK [AppT (ConT ''IncForest) $ VarT fsName,rep_ty] :
		ClassP ''IncK [AppT (ConT ''IncForest) $ VarT fsName,ConT ''Bool] : xs
	return (rep_ty)
	
genZRepMDCompTy :: Bool -> Name -> Name -> CompField -> GenQ Type
genZRepMDCompTy isTop ecName fsName info = do
	(rep_ty) <- genZRepMDComp ecName fsName info
	let rep_ty' = appT3 (ConT ''ECMd) (VarT ecName) (VarT fsName) rep_ty
	State.modify $ \xs -> ClassP ''IncK [AppT (ConT ''IncForest) $ VarT fsName,appT2 (TupleT 2) (AppT (ConT ''Forest_md) $ VarT fsName) rep_ty] : xs
	if isTop
		then fsthunkTyQ fsName rep_ty' 
		else return rep_ty'

{- Generate type and meta-data representations. -}
genZRepMDTy :: Bool -> Name -> Name -> ForestTy -> GenQ (Type)
genZRepMDTy isTop ecName fsName ty = case ty of
	Directory _          -> error "Forest: Directory declarations must appear at the top level."
	FFile (ty_name,arg)   -> do
		State.modify $ \xs -> 
			ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),appT2 (TupleT 2) (appT2 (TupleT 2) (AppT (ConT ''Forest_md) $ VarT fsName) (ConT $ getMDName ty_name)) (ConT $ getTyName ty_name)] :
			ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),appT2 (TupleT 2) (appT2 (TupleT 2) (ConT ''FileInfo) (ConT $ getMDName ty_name)) (ConT $ getTyName ty_name)] : xs
		if isTop
			then do
				let con_ty = appT3 (ConT ''ECMd) (VarT ecName) (VarT fsName) $ ConT (getMDName ty_name)
				fsthunkTyQ fsName $ tyListToTupleTy [ con_ty, ConT (getTyName ty_name) ] 
			else do
				let con_ty = appT3 (ConT ''ECMd) (VarT ecName) (VarT fsName) $ ConT (getMDName ty_name)
				return $ tyListToTupleTy [ con_ty, ConT (getTyName ty_name) ] 
	Archive archtype ty              -> do
		rep_ty <- genZRepMDTy False ecName fsName ty
		let con_ty = appT3 (ConT ''ECMd) (VarT ecName) (VarT fsName) rep_ty
		State.modify $ \xs -> 
			ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),appT2 (TupleT 2) (AppT (ConT ''Forest_md) (VarT fsName)) rep_ty] : xs
		if isTop
			then fsthunkTyQ fsName con_ty
			else return con_ty
	FSymLink -> if isTop
		then do
			State.modify $ \xs -> 
				ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),appT2 (TupleT 2) (appT2 (TupleT 2) (ConT ''FileInfo) (ConT ''Base_md)) (ConT ''FilePath)] : xs
			let con_ty = appT3 (ConT ''ECMd) (VarT ecName) (VarT fsName) $ ConT ''Base_md
			fsthunkTyQ fsName $ tyListToTupleTy [ con_ty, ConT ''FilePath ] 
		else do
			let con_ty = appT3 (ConT ''ECMd) (VarT ecName) (VarT fsName) $ ConT ''Base_md
			return $ tyListToTupleTy [ con_ty, ConT ''FilePath ]
	Named ty_name        -> do
		argName <- lift $ newName "arg"
		let name = getTyName ty_name
		let rep_ty = AppT (ConT $ name) (VarT fsName)
		State.modify $ \xs -> ClassP ''ZippedICForest [VarT fsName,rep_ty] : xs
		if isTop
			then do
				fsthunkTyQ fsName rep_ty
			else do
				return rep_ty
	FConstraint p ty pred -> do
		rep_ty <- genZRepMDTy False ecName fsName ty
		let con_ty = appT3 (ConT ''ECErr) (VarT ecName) (VarT fsName) rep_ty
		State.modify $ \xs ->
			ClassP ''MData [ConT ''NoCtx,AppT (ConT ''ForestI) (VarT fsName),rep_ty] : xs
		if isTop
			then fsthunkTyQ fsName con_ty
			else return con_ty
	FMaybe ty            -> genZRepMDMaybe isTop ecName fsName ty
	Fapp ty arg          -> genZRepMDTy isTop ecName fsName ty
	FComp cinfo          -> genZRepMDCompTy isTop ecName fsName cinfo

-- * Unzipped representation/metadata

unthunkTy :: Type -> Type
unthunkTy (AppT (AppT _ _) ty) = ty
unthunkTy t = error $ show t
	
mkNewTypeForestRepEC :: Bool -> Name -> Name -> Name -> Name -> Name -> (Name,Name) -> Type -> Q [Dec]
mkNewTypeForestRepEC needsEC ecName fsName ty_nameEC ty_nameE ty_nameC (unty_name,ty_name) rep = do
	
	let repE = SYB.everywhere (SYB.mkT $ replaceEC ecName fsName E) rep
	let repC = SYB.everywhere (SYB.mkT $ replaceEC ecName fsName C) rep
	
	thunkName <- newName "thunk"
	let content = ValD (VarP 'iso_rep_thunk) (NormalB $ LamE [WildP] $ appE2 (ConE 'Iso) (VarE unty_name) (ConE ty_name)) []
	let inst = if needsEC
		then [
			InstanceD [] (appT2 (ConT ''ForestRep) (VarT fsName) (appT2 (ConT ty_nameEC) (PromotedT 'E) (VarT fsName))) [TySynInstD ''ForestRepTy $ TySynEqn [VarT fsName,appT2 (ConT ty_nameEC) (PromotedT 'E) (VarT fsName)] repE,content]
			,InstanceD [] (appT2 (ConT ''ForestRep) (VarT fsName) (appT2 (ConT ty_nameEC) (PromotedT 'C) (VarT fsName))) [TySynInstD ''ForestRepTy $ TySynEqn [VarT fsName,appT2 (ConT ty_nameEC) (PromotedT 'C) (VarT fsName)] repC,content]]
		else [InstanceD [] (appT2 (ConT ''ForestRep) (VarT fsName) (AppT (ConT ty_nameEC) (VarT fsName))) [TySynInstD ''ForestRepTy $ TySynEqn [VarT fsName,AppT (ConT ty_nameEC) (VarT fsName)] repE,content]]
	return inst

replaceEC :: Name -> Name -> EC -> Type -> Type
replaceEC ec fs n (AppT (AppT (AppT (ConT ((==''ECErr) -> True)) (VarT ((==ec) -> True))) (VarT ((==fs) -> True))) a) = case n of
	E -> appT2 (TupleT 2) (appT2 (ConT ''ForestFSThunkI) (VarT fs) (ConT ''Forest_err)) a
	C -> a
replaceEC ec fs n (AppT (AppT (AppT (ConT ((==''ECMd) -> True)) (VarT ((==ec) -> True))) (VarT ((==fs) -> True))) a) = case n of
	E -> appT2 (TupleT 2) (AppT (ConT ''Forest_md) (VarT fs)) a
	C -> appT2 (TupleT 2) (ConT ''FileInfo) a
replaceEC ec fs n t = t
	
mkNewTypeForestMDEC :: Bool -> Name -> Name -> Name -> Name -> Name -> (Name,Name) -> Type -> Q [Dec]
mkNewTypeForestMDEC needsEC ecName fsName ty_nameEC ty_nameE ty_nameC (unty_name,ty_name) rep = do
	x <- newName "x"
	f <- newName "f"
	let repE = SYB.everywhere (SYB.mkT $ replaceEC ecName fsName E) rep
	let ctx = [
		ClassP ''ICRep [VarT fsName]
		,ClassP ''ForestMD [VarT fsName,repE]
		]
	let inst = [
		InstanceD ctx (appT2 (ConT ''ForestMD) (VarT fsName) (appT2 (ConT ty_nameEC) (PromotedT 'E) (VarT fsName))) [
			FunD 'isUnevaluatedMDThunk [Clause [] (NormalB $ UInfixE (VarE 'isUnevaluatedMDThunk) (VarE '(.)) (VarE unty_name)) []]
			,FunD 'get_fmd_header [Clause [] (NormalB $ UInfixE (VarE 'get_fmd_header) (VarE '(.)) (VarE unty_name)) []]
			,FunD 'replace_fmd_header [Clause [ConP ty_name [VarP x],VarP f] (NormalB $ AppE (AppE (VarE 'liftM) $ ConE ty_name) $ appE2 (VarE 'replace_fmd_header) (VarE x) (VarE f)) []]
			]
		]
	return inst
	
--replace_fmd_header :: md -> (Forest_md fs -> ForestI fs (Forest_md fs)) -> ForestI fs md

makeForestEq :: Name -> Dec -> Q [Dec]
makeForestEq modeName dec = do
	[InstanceD _ (AppT (ConT eq) (AppT (AppT n mode) fs)) decs] <- deriveFromDec makeEq dec
	let icDataInst = InstanceD [ClassP ''ICRep [fs],ClassP ''IncK [AppT (ConT ''IncForest) fs,ConT ''FileInfo]] (AppT (ConT eq) (AppT n fs)) decs
	return [icDataInst]

thunksTy :: Name -> [(Pat,Type)] -> Type
thunksTy fsName pat_infos = thunksTy' $ map (appT2 (ConT ''ForestICThunkI) (VarT fsName) . snd) pat_infos
	where thunksTy' :: [Type] -> Type
	      thunksTy' = foldl1' (appT2 (ConT ''(:*:)))

-- named comprehensions will be loaded as directories and get their own @Forest_md@, whereas inlined comprehensions inside Directorys do not get their own @Forest_md@
type VST = (TH.Name, TH.Strict, Type)


mkStringConTupleTy con ty = AppT (ConT con)  (tyListToTupleTy [ConT ''String, ty])
mkStringConCurryTy con ty = AppT (AppT (ConT con) (ConT ''String)) ty
mkStringListTy ty = AppT ListT (tyListToTupleTy [ConT ''String, ty])

mkKeyConTupleTy con key ty = AppT (ConT con)  (tyListToTupleTy [key, ty])
mkKeyConCurryTy con key ty = AppT (AppT (ConT con) (key)) ty
mkKeyListTy key ty = AppT ListT (tyListToTupleTy [key, ty])


uTy :: Name -> Type -> Type
uTy fsName ty = appT2 (ConT ''ForestICThunkI) (VarT fsName) ty

fsthunkTyQ :: Name -> Type -> GenQ Type
fsthunkTyQ fsName ty = do
	State.modify $ \xs -> 
		ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),ty] :
		ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),appT2 (ConT ''ForestFSThunkI) (VarT fsName) ty] : xs
	return $ appT2 (ConT ''ForestFSThunkI) (VarT fsName) ty

predTy :: Pred -> Type
predTy (ClassP n tys) = appConT n tys

appConT :: Name -> [Type] -> Type
appConT con = Foldable.foldl' AppT (ConT con)

type instance ForestArgs (ForestFSThunkI fs ((Forest_md fs,md),pads)) = Arg (PadsArg pads)

instance (arg ~ PadsArg pads,
	MData NoCtx (ForestI fs) arg,
	IncK (IncForest fs) Forest_err,IncK (IncForest fs) FileInfo,
	IncK (IncForest fs) (ForestFSThunkI fs ((Forest_md fs, md), pads)),
	IncK (IncForest fs) arg,
	Eq arg,Data arg,
	IncK (IncForest fs) ((Forest_md fs, md), pads),
	IncK (IncForest fs) ((FileInfo, md), ForestContentTy fs pads),
	ForestDiff fs arg,Eq md,Eq pads,Typeable arg,MData (CopyFSThunksDict fs Outside) (ForestO fs) md,
	MData (CopyFSThunksDict fs Outside) (ForestO fs) pads,
	MData NoCtx (ForestO fs) pads,
	MData NoCtx (ForestO fs) md,
	DeepTypeable md,DeepTypeable pads,
	ForestContent fs pads,ZippedICMemo fs,Pads1 (PadsArg pads) pads md
	) => ZippedICForest fs (ForestFSThunkI fs ((Forest_md fs,md),pads)) where
		diffValue = diffValueThunk
		zloadScratchGeneric = zloadScratchMemo
		zloadDeltaGeneric = zloadDeltaMemo
		zupdateManifestScratchGeneric = zupdateManifestScratchMemo
		zupdateManifestDeltaGeneric = zupdateManifestDeltaMemo
		zdefaultScratchGeneric = zdefaultScratchMemo
		
		zloadScratch proxy marg pathfilter path tree getMD = doZLoadFile1 Proxy marg pathfilter path tree getMD
		zloadDelta (marg,darg) mpath tree (rep,getMD) path' df tree' dv = liftM StableVD $ doZLoadDeltaFile1 (isEmptyDelta darg) marg mpath path' tree df tree' dv (rep,getMD)
		zupdateManifestScratch marg path tree rep man = doZManifestFile1 marg path tree rep man
		zupdateManifestDelta (marg,darg) path path' tree df tree' rep dv man = doZDeltaManifestFile1 (isEmptyDelta darg) marg path path' tree df tree' rep dv man
		zdefaultScratch proxy fs marg path = doZDefaultFile1 fs marg path



