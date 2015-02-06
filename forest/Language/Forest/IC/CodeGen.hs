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

import Data.Derive.Eq
import Data.Generics.Aliases as SYB
import Data.Generics as SYB
import Language.Forest.IC.CodeGen.ZDeltaStoring
import GHC.Exts
import Data.Foldable as Foldable
import Control.Monad.Trans
import Language.Forest.IC.Default
import Language.Forest.IC.IO.ZDefault
import qualified Language.Forest.Pure.CodeGen.Utils as Pure
import Language.Forest.IC.ICRep
import Data.DeepTypeable
import Language.Forest.IC.BX
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
import Language.Forest.Pure.MetaData (FileInfo(..),FileType(..),(:*:)(..),Arg(..))
import Language.Forest.Manifest
import Language.Forest.IC.FS.FSDelta
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
import Language.Forest.IC.CodeGen.Loading
import Language.Forest.IC.CodeGen.ZLoading
import Language.Forest.IC.CodeGen.Default
import Language.Forest.IC.CodeGen.ZDefault
import Data.Maybe
import Language.Forest.IC.CodeGen.DeltaLoading
import Language.Forest.IC.CodeGen.ZDeltaLoading
import Language.Forest.IC.CodeGen.Storing
import Language.Forest.IC.CodeGen.ZStoring

type GenQ = StateT (Set Pred) Q

{- Code generation routines -}
make_forest_declarations :: Maybe ICMode -> [ForestDecl] -> Q [Dec]
make_forest_declarations mode ds = fmap List.concat (mapM (make_forest_declaration mode) ds)

make_forest_declaration :: Maybe ICMode -> ForestDecl -> Q [Dec]
make_forest_declaration mb (ForestDecl (id, pats, forestTy)) = do
	let ty_name = Pure.getRepTyName id
	let unty_name = Pure.getRepUnTyName id
	let md_ty_name = Pure.getMDName id
	fsName <- newName "fs"
	modeName <- newName "mode"
	let mode = maybe modeName modeN mb
	let arg_infos = map (\pat -> (pat,patToTy pat)) pats
	(ty_decl, md_ty_decls, md_ty) <- genRepMDDecl modeName fsName forestTy (unty_name,ty_name) md_ty_name arg_infos  -- Generate representation and meta-data decls for padsTy

	loadM :: Exp <- Reader.runReaderT (genLoadM ty_name md_ty_name forestTy arg_infos) (mode,fsName,Map.empty)
	loadDeltaM :: Exp <- Reader.runReaderT (genLoadDeltaM (unty_name,ty_name) md_ty_name forestTy arg_infos) (mode,fsName,Map.empty)
	manifestM :: Exp <- Reader.runReaderT (genManifestM ty_name md_ty_name forestTy arg_infos) (mode,fsName,Map.empty)
	defaultM :: Exp <- genDefaultM ty_name md_ty_name forestTy arg_infos
	
	let mkInst modeT = genFInst modeName modeT fsName  loadM loadDeltaM manifestM defaultM ty_name md_ty_name      forestTy arg_infos
	
	insts <- case mb of
		Nothing -> do
			inst1 <- mkInst $ PromotedT 'ICData
			inst2 <- mkInst $ PromotedT 'ICExpr
			return $ inst1++inst2
		Just ICData -> mkInst $ PromotedT 'ICData
		Just ICExpr -> mkInst $ PromotedT 'ICExpr

	let args_ty = case arg_infos of
		[] -> TupleT 0
		otherwise -> Pure.forestTupleTy $ map (AppT (ConT ''Arg) . snd) arg_infos
	let loadDelta_alias = FunD (mkName $ "loadDelta_"++nameBase ty_name) [Clause [VarP modeName] (NormalB $ Pure.appE2 (VarE 'loadDelta) (VarE modeName) (proxyT args_ty)) []]
	let proxyArgs_alias = FunD (mkName $ "proxyArgs_"++nameBase ty_name) [Clause [] (NormalB $ proxyT args_ty) []]

	return $ ty_decl:md_ty_decls++insts++[loadDelta_alias,proxyArgs_alias]

genFInst modeName modeT fsName loadM loadDeltaM manifestM defaultM ty_name md_ty_name forestTy pat_infos = do
	mdName <- newName "md"
	let (inst,mdT) = case pat_infos of
		[] -> (Pure.appT5 (ConT ''ICForest) modeT (VarT fsName) (TupleT 0) (Pure.appTyFS fsName ty_name) (VarT mdName) , (appTyModeFS'' modeT fsName md_ty_name))  
		otherwise -> (Pure.appT5 (ConT ''ICForest) modeT (VarT fsName) (Pure.forestTupleTy $ map (AppT (ConT ''Arg) . snd) pat_infos) (Pure.appTyFS fsName ty_name) (VarT mdName) , (appTyModeFS'' modeT fsName md_ty_name))
	let load_method = ValD (VarP 'loadScratch) (NormalB loadM) []
	let loadDelta_method = ValD (VarP 'loadDelta) (NormalB loadDeltaM) []
	let manifest_method = ValD (VarP 'updateManifestScratch) (NormalB manifestM) []
	let default_method = ValD (VarP 'IC.defaultMd) (NormalB defaultM) []

	let ctx = [ClassP ''Typeable [VarT fsName],ClassP ''ICMemo [VarT fsName]
			,ClassP ''ForestMD [VarT fsName,(appTyModeFS'' modeT fsName md_ty_name)]
			,ClassP ''ForestOutput [VarT fsName,ConT ''ICThunk,ConT ''Inside]
			, (VarT mdName) ` EqualP` mdT
			]
	
	return $ [InstanceD ctx inst [load_method,loadDelta_method,manifest_method,default_method]]

make_zforest_declarations :: [ForestDecl] -> Q [Dec]
make_zforest_declarations ds = make_zforest_declarations' ds Nothing

make_zforest_declarations' :: [ForestDecl] -> Maybe [Type] -> Q [Dec]
make_zforest_declarations' ds mb_fsTy = fmap List.concat (mapM (flip make_zforest_declaration mb_fsTy) ds)

make_zforest_declaration :: ForestDecl -> Maybe [Type] -> Q [Dec]
make_zforest_declaration (ForestDecl (id, pats, forestTy)) mb_fsTy = do
	let ty_name = Pure.getTyName id
	ecName <- newName "ec"
	fsName <- newName "fs"
	let unty_name = Pure.getUnTyName id
	let arg_infos = map (\pat -> (pat,patToTy pat)) pats
	((ty_decl,aux_decls),ks) <- State.runStateT (genZRepMDDecl ecName fsName forestTy (unty_name,ty_name) arg_infos) Set.empty -- Generate representation and meta-data decls for padsTy

	loadM :: Exp <- Reader.runReaderT (genZLoadM ty_name forestTy arg_infos) (fsName,Map.empty)
	loadDeltaM :: Exp <- Reader.runReaderT (genZLoadDeltaM (unty_name,ty_name) forestTy arg_infos) (fsName,Map.empty)
	manifestM :: Exp <- Reader.runReaderT (genZManifestM ty_name forestTy arg_infos) (fsName,Map.empty)
	manifestDeltaM :: Exp <- Reader.runReaderT (genZManifestDeltaM ty_name forestTy arg_infos) (fsName,Map.empty)
	defaultM :: Exp <- Reader.runReaderT (genZDefaultM ty_name forestTy arg_infos) (fsName,Map.empty)
	
	inst <- genZFInst mb_fsTy ecName fsName  loadM loadDeltaM manifestM manifestDeltaM defaultM ty_name      forestTy arg_infos ks

	let args_ty = case arg_infos of
		[] -> TupleT 0
		otherwise -> Pure.forestTupleTy $ map (AppT (ConT ''Arg) . snd) arg_infos
	let proxyArgs_alias = FunD (mkName $ "proxyZArgs_"++nameBase ty_name) [Clause [] (NormalB $ proxyT args_ty) []]

	return $ ty_decl:aux_decls++inst++[proxyArgs_alias]

genZFInst :: Maybe [Type] -> Name -> Name -> Exp -> Exp -> Exp -> Exp -> Exp -> Name -> ForestTy -> [(Pat,Type)] -> Set Pred -> Q [Dec]
genZFInst mb_fsTy ecName fsName loadM loadDeltaM manifestM manifestDeltaM defaultM ty_name forestTy pat_infos ks = do
	repName <- newName "rep"
	let tyName = AppT (ConT ty_name) (VarT fsName)
	let (inst) = case pat_infos of
		[] -> (Pure.appT3 (ConT ''ZippedICForest) (VarT fsName) (TupleT 0) tyName )  
		otherwise -> (Pure.appT3 (ConT ''ZippedICForest) (VarT fsName) (Pure.forestTupleTy $ map (AppT (ConT ''Arg) . snd) pat_infos) tyName )
	let load_method = ValD (VarP 'zloadScratch) (NormalB loadM) []
	let loadDelta_method = ValD (VarP 'zloadDelta) (NormalB loadDeltaM) []
	let manifest_method = ValD (VarP 'zupdateManifestScratch) (NormalB manifestM) []
	let manifestD_method = ValD (VarP 'zupdateManifestDelta) (NormalB manifestDeltaM) []
	let default_method = ValD (VarP 'zdefaultScratch) (NormalB defaultM) []
	
	
	let mkInst mb_ty =
		let ctx = Set.toList ks ++ [ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),ConT ''FileInfo],ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),ConT ''FilePath],ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),ConT ''Forest_err],ClassP ''Typeable [VarT fsName],ClassP ''ZippedICMemo [VarT fsName]]
		    instD = InstanceD ctx inst [load_method,loadDelta_method,manifest_method,manifestD_method,default_method]
		    instD' = SYB.everywhere (SYB.mkT $ \(t::Type) -> if t == VarT ecName then (PromotedT 'E) else t) instD
		in case mb_ty of
			Nothing -> instD'
			Just ty -> SYB.everywhere (SYB.mkT $ \(t::Type) -> if t == VarT fsName then ty else t) instD'
	
	case mb_fsTy of
		Nothing -> return [mkInst Nothing]
		Just tys -> return $ map (mkInst . Just) tys

-- * Zipped representation/metadata

-- | Generates representation and metadata type declarations for a Forest declaration
genZRepMDDecl :: Name -> Name -> ForestTy -> (Name,Name) -> [(Pat,Type)] -> GenQ (Dec, [Dec])
genZRepMDDecl ecName fsName ty (unty_name,ty_name) pat_infos = case ty of
	Directory dirTy -> genZRepMDDir False ecName fsName dirTy ty_name pat_infos
	(FConstraint _ (Directory dirTy) _) -> genZRepMDDir True ecName fsName dirTy ty_name pat_infos
	otherwise -> do
		let tyNameC = mkName $ nameBase ty_name ++ "C"
		let tyNameEC = mkName $ nameBase ty_name ++ "EC"
		rep <- genZRepMDTy True ecName fsName ty
		let ty_decEC = mk_newTyDEC ecName fsName tyNameEC (unty_name,ty_name) rep
		
		let ty_dec = TySynD ty_name [KindedTV fsName (ConT ''FS)] $ Pure.appT2 (ConT tyNameEC) (PromotedT 'E) (VarT fsName)
		let ty_decC = TySynD tyNameC [KindedTV fsName (ConT ''FS)] $ Pure.appT2 (ConT tyNameEC) (PromotedT 'C) (VarT fsName)
		mdataInstance <- lift $ deriveFromDec makeMData ty_decEC
		deepTypeableInstance <- lift $ deriveFromDec makeDeepTypeable ty_decEC
		forestRepInstance <- lift $ mkNewTypeForestRepEC ecName fsName tyNameEC (unty_name,ty_name) rep
		return (ty_decEC, ty_decC:ty_dec:forestRepInstance:mdataInstance++deepTypeableInstance)

{- Generate a representation and meta-data type for maybe. -}
genZRepMDMaybe :: Bool -> Name -> Name -> ForestTy -> GenQ Type
genZRepMDMaybe isTop ecName fsName ty = do
	rep_orig <- genZRepMDTy False ecName fsName ty
	let rep_ty = AppT (ConT ''Maybe) rep_orig                 -- rep is Maybe ty where ty is rep of nested type
	let rep_ty'  = Pure.appT3 (ConT ''ECMd) (VarT ecName) (VarT fsName) rep_ty  -- md is a pair of a base md for the maybe and the underlying md.
	if isTop
		then fsthunkTyQ fsName rep_ty'
		else return rep_ty

{- Generate a representation and meta-data type for a directory with named fields. -}
genZRepMDDir :: Bool -> Name -> Name -> DirectoryTy -> Name ->  [(Pat,Type)] -> GenQ (Dec, [Dec])
genZRepMDDir hasConstraint ecName fsName (Record _ fields) ty_name pat_infos = do
	reps <- mapM (genZRepMDField ecName fsName) fields
	let ty_nameEC = mkName $ nameBase ty_name ++ "EC"
	let ty_nameC = mkName $ nameBase ty_name ++ "C"
	let inner_ty_name = Pure.getStructInnerName ty_name
	let inner_ty_nameEC = mkName $ nameBase inner_ty_name ++ "EC"
	let derives      = [''Typeable]
	let ty_con       = TH.RecC inner_ty_name reps
	let inner_ty_decl      = DataD [] inner_ty_nameEC [PlainTV ecName,PlainTV fsName] [ty_con] derives
	let ty = Pure.appT3 (ConT ''ECMd) (VarT ecName) (VarT fsName) $ Pure.appT2 (ConT inner_ty_nameEC) (VarT ecName) (VarT fsName)
	let tyE = Pure.tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName) , Pure.appT2 (ConT inner_ty_nameEC) (PromotedT 'E) (VarT fsName) ]
	let tyC = Pure.tyListToTupleTy [(ConT ''FileInfo) , Pure.appT2 (ConT inner_ty_nameEC) (PromotedT 'C) (VarT fsName) ]
	ty' <- if hasConstraint
		then fsthunkTyQ fsName $ Pure.appT3 (ConT ''ECErr) (VarT ecName) (VarT fsName) ty
		else fsthunkTyQ fsName ty
	tyE' <- if hasConstraint
		then do
			err_ty <- fsthunkTyQ fsName (ConT ''Forest_err)
			fsthunkTyQ fsName $ Pure.tyListToTupleTy [ err_ty ,tyE ]
		else fsthunkTyQ fsName tyE
	tyC' <- fsthunkTyQ fsName tyC
	let ty_declEC = mk_TySynDEC ecName fsName ty_nameEC ty'
	let ty_decl = mk_TySynDE fsName ty_name tyE'
	let ty_declC = mk_TySynDE fsName ty_nameC $ tyC'
	if length reps == 0
		then error ("Error: Directory " ++ (show ty_name) ++ " must contain at least one named field.")
		else do
			mdataInstance_rep <- lift $ deriveFromDec makeMData inner_ty_decl
			deepTypeableRepInstance <- lift $ deriveFromDec makeDeepTypeable inner_ty_decl
			forestContent <- mkDirForestContentEC fsName ty_name inner_ty_nameEC fields
			eqInstance <- lift $ makeForestECEq ecName fsName inner_ty_decl
			return (ty_declEC, (ty_decl:ty_declC:inner_ty_decl:eqInstance++mdataInstance_rep++deepTypeableRepInstance++forestContent))

makeForestECEq :: Name -> Name -> Dec -> Q [Dec]
makeForestECEq ecName fsName dec = do
	[InstanceD ctx ty@(AppT eq (AppT (AppT con ec) fs)) decs] <- deriveFromDec makeEq dec
	let ctx' = everything (++) (mkQ [] (collectECEqs ec fs)) dec
	let ctx'' = ClassP ''ICRep [fs] : ctx'
	return [InstanceD ctx'' ty decs]
  where
	collectECEqs :: Type -> Type -> Type -> [Pred]
	collectECEqs ec fs t@(AppT (AppT (AppT (ConT ((==''ECErr) -> True)) _) _) ity) = [ClassP ''Eq [everywhere (mkT $ replaceECFS ec fs) t]]
	collectECEqs ec fs t@(AppT (AppT (AppT (ConT ((==''ECMd) -> True)) _) _) ity) = [ClassP ''Eq [everywhere (mkT $ replaceECFS ec fs) t]]
	collectECEqs ec fs t = []
	replaceECFS :: Type -> Type -> Type -> Type
	replaceECFS ec fs (VarT ((==ecName) -> True)) = ec
	replaceECFS ec fs (VarT ((==fsName) -> True)) = fs
	replaceECFS ec fs t = t

mkDirForestContentEC :: Name -> Name -> Name -> [Field] -> GenQ [Dec]
mkDirForestContentEC fsName ty_name inner_ty_name fields = do
	let dec = InstanceD [] (Pure.appT2 (ConT ''ForestContent) (Pure.appT2 (ConT inner_ty_name) (PromotedT 'E) $ VarT fsName) (Pure.appT2 (ConT inner_ty_name) (PromotedT 'C) $ VarT fsName)) []
	return [dec]

genZRepMDField :: Name -> Name -> Field -> GenQ (VST)
genZRepMDField ecName fsName (Simple (internal, isForm, external, ty, predM)) = do
	(rep_ty) <- genZRepMDTy False ecName fsName ty
	let rep_ty' = case predM of
		Nothing -> rep_ty
		Just _ -> Pure.appT3 (ConT ''ECErr) (VarT ecName) (VarT fsName) rep_ty
	return (Pure.getFieldName   internal, TH.NotStrict, rep_ty')
genZRepMDField ecName fsName (Comp (info @ CompField {internalName, tyConNameOpt, descTy, ..})) = do
	(rep_ty) <- genZRepMDComp ecName fsName info
	return (Pure.getFieldName   internalName, TH.NotStrict, {-fsthunkTyQ fsName -} rep_ty)

genZRepMDComp :: Name -> Name -> CompField -> GenQ Type
genZRepMDComp ecName fsName (CompField {internalName, tyConNameOpt, descTy, predEOpt, ..}) = do
	(rng_rep_ty) <- genZRepMDTy False ecName fsName descTy
	(rep_ty) <- case tyConNameOpt of 
		Nothing ->  return (mkStringListTy rng_rep_ty)
		Just str -> do
			arity <- lift $ Pure.getTyConArity str
			case arity of 
				1 -> return (mkStringConTupleTy (mkName str) rng_rep_ty) 
				2 -> return (mkStringConCurryTy (mkName str) rng_rep_ty) 
	return (rep_ty)
	
genZRepMDCompTy :: Bool -> Name -> Name -> CompField -> GenQ Type
genZRepMDCompTy isTop ecName fsName info = do
	(rep_ty) <- genZRepMDComp ecName fsName info
	let rep_ty' = Pure.appT3 (ConT ''ECMd) (VarT ecName) (VarT fsName) rep_ty
	if isTop
		then fsthunkTyQ fsName rep_ty' 
		else return rep_ty'

{- Generate type and meta-data representations. -}
genZRepMDTy :: Bool -> Name -> Name -> ForestTy -> GenQ (Type)
genZRepMDTy isTop ecName fsName ty = case ty of
	Directory _          -> error "Forest: Directory declarations must appear at the top level."
	FFile (ty_name,arg)   -> do
		let con_ty = Pure.appT3 (ConT ''ECMd) (VarT ecName) (VarT fsName) $ ConT (Pure.getMDName ty_name)
		repTy <- fsthunkTyQ fsName $ Pure.tyListToTupleTy [ con_ty, ConT (Pure.getTyName ty_name) ] 
		return repTy 
	Archive archtype ty              -> do
		rep_ty <- genZRepMDTy False ecName fsName ty
		let con_ty = Pure.appT3 (ConT ''ECMd) (VarT ecName) (VarT fsName) rep_ty
		if isTop
			then fsthunkTyQ fsName con_ty
			else return con_ty
	FSymLink              -> return $ AppT (ConT ''SymLink) (VarT fsName)
	Named ty_name        -> do
		argName <- lift $ newName "arg"
		let ty_nameEC = mkName $ nameBase (Pure.getTyName ty_name) ++ "EC"
		let rep_ty = Pure.appT2 (ConT $ ty_nameEC) (VarT ecName) (VarT fsName)
		return rep_ty
	FConstraint p ty pred -> do
		rep_ty <- genZRepMDTy False ecName fsName ty
		let con_ty = Pure.appT3 (ConT ''ECErr) (VarT ecName) (VarT fsName) rep_ty
		if isTop
			then fsthunkTyQ fsName con_ty
			else return con_ty
	FMaybe ty            -> genZRepMDMaybe isTop ecName fsName ty
	Fapp ty arg          -> genZRepMDTy isTop ecName fsName ty
	FComp cinfo          -> genZRepMDCompTy isTop ecName fsName cinfo

-- * Unzipped representation/metadata

-- | Generates representation and metadata type declarations for a Forest specification
genRepMDDecl :: Name -> Name -> ForestTy -> (Name,Name) -> Name -> [(Pat,Type)] -> Q (Dec, [Dec], Type)
genRepMDDecl modeName fsName ty (unty_name,ty_name) md_ty_name pat_infos = case ty of
	Directory dirTy -> genRepMDDir False modeName fsName dirTy ty_name md_ty_name pat_infos
	(FConstraint _ (Directory dirTy) _) -> genRepMDDir True modeName fsName dirTy ty_name md_ty_name pat_infos
	otherwise -> do
		(rep,md) <- genRepMDTy modeName fsName ty
		let ty_dec = mk_newTyD fsName (unty_name,ty_name) rep
		mdataInstance <- deriveFromDec makeMData ty_dec
		deepTypeableInstance <- deriveFromDec makeDeepTypeable ty_dec
		forestRepInstance <- mkNewTypeForestRep fsName (unty_name,ty_name) rep
		let md' = case pat_infos of
			[] -> md
			otherwise -> Pure.appT3 (ConT ''MDArgs) (VarT modeName) md (thunksTy fsName pat_infos) -- adds argument thunks to the metadata
		return (ty_dec, forestRepInstance:mdataInstance++deepTypeableInstance++[mk_TySynDMode modeName fsName md_ty_name md'], md') 

mkNewTypeForestRep :: Name -> (Name,Name) -> Type -> Q Dec
mkNewTypeForestRep fsName (unty_name,ty_name) rep = do
	thunkName <- newName "thunk"
	let content = ValD (VarP 'iso_rep_thunk) (NormalB $ InfixE (Just $ Pure.appE2 (ConE 'Iso) (VarE unty_name) (ConE ty_name)) (VarE 'isoComp) (Just $ VarE 'iso_rep_thunk)) []
	return $ InstanceD [ClassP ''ForestRep [rep,VarT thunkName]] (Pure.appT2 (ConT ''ForestRep) (AppT (ConT ty_name) (VarT fsName)) (VarT thunkName)) [content]
	
mkNewTypeForestRepEC :: Name -> Name -> Name -> (Name,Name) -> Type -> Q Dec
mkNewTypeForestRepEC ecName fsName ty_nameEC (unty_name,ty_name) rep = do
	thunkName <- newName "thunk"
	let content = ValD (VarP 'iso_rep_thunk) (NormalB $ InfixE (Just $ Pure.appE2 (ConE 'Iso) (VarE unty_name) (ConE ty_name)) (VarE 'isoComp) (Just $ VarE 'iso_rep_thunk)) []
	return $ InstanceD [ClassP ''ForestRep [rep,VarT thunkName]] (Pure.appT2 (ConT ''ForestRep) (Pure.appT2 (ConT ty_nameEC) (VarT ecName) (VarT fsName)) (VarT thunkName)) [content]	

{- Generate a representation and meta-data type for maybe. -}
genRepMDMaybe :: Name -> Name -> ForestTy -> Q (Type, Type)
genRepMDMaybe modeName fsName ty = do
	(rep_orig, md_orig) <- genRepMDTy modeName fsName ty
	let rep_ty = AppT (ConT ''Maybe) rep_orig                 -- rep is Maybe ty where ty is rep of nested type
	let md'_ty = AppT (ConT ''Maybe) md_orig                  -- underyling md is Maybe of md of nested type
	let md_ty  = Pure.tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName), md'_ty ]    -- md is a pair of a base md for the maybe and the underlying md.
	return (fsthunkTy fsName rep_ty, fsthunkTy fsName md_ty)

{- Generate a representation and meta-data type for a directory with named fields. -}
genRepMDDir :: Bool -> Name -> Name -> DirectoryTy -> Name -> Name ->  [(Pat,Type)] -> Q (Dec, [Dec], Type)
genRepMDDir hasConstraint modeName fsName (Record _ fields) ty_name md_ty_name pat_infos = do
	reps <- mapM (genRepMDField modeName fsName) fields
	let (vsts', md_vsts') = unzip reps
	let inner_ty_name = Pure.getStructInnerName ty_name
	let derives      = [''Typeable,''Eq]
	let ty_con       = TH.RecC inner_ty_name vsts'
--	let ty_md_eq = map (ClassP ''Eq . (:[])) $ allMDArgs ty_con
	let inner_ty_decl      = DataD [] inner_ty_name [PlainTV fsName] [ty_con] derives
	let ty = fsthunkTy fsName $ AppT (ConT inner_ty_name) (VarT fsName)
	let ty_decl = mk_TySynD fsName ty_name ty
	let inner_md_name = Pure.getStructInnerMDName ty_name   -- ty name is the same as the declared pads type name
	let imd_con       = TH.RecC inner_md_name md_vsts'
--	let imd_con_eq    = map (ClassP ''Eq . (:[])) $ allMDArgs imd_con
	let imd_decl      = TH.DataD [] inner_md_name [PlainTV modeName,PlainTV fsName] [imd_con] [''Typeable]   -- declaration of line for nested components
	let imd_ty        = appTyModeFS' modeName fsName inner_md_name
	let md_ty         = fsthunkTy fsName $ Pure.tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName), {-fsthunkTy fsName-} imd_ty]
	let md_ty_thunk = case pat_infos of
		[] -> md_ty
		otherwise -> Pure.appT3 (ConT ''MDArgs) (VarT modeName) md_ty $ thunksTy fsName pat_infos -- adds argument thunks to the metadata
	let md_ty_thunk' = if hasConstraint
		then Pure.appT3 (ConT ''MDArgs) (VarT modeName) md_ty_thunk $ uTy fsName (ConT ''Bool)
		else md_ty_thunk
	let  md_decl       = mk_TySynDMode modeName fsName md_ty_name md_ty_thunk'
	if length vsts' == 0
		then error ("Error: Directory " ++ (show ty_name) ++ " must contain at least one named field.")
		else do
			eqInsts <- makeForestEq modeName imd_decl
			mdataInstance_rep <- deriveFromDec makeMData inner_ty_decl
			mdataInstance_md <- deriveFromDec makeMData imd_decl
			deepTypeableRepInstance <- deriveFromDec makeDeepTypeable inner_ty_decl
			deepTypeableMdInstance <- deriveFromDec makeDeepTypeable imd_decl
			return (ty_decl, {-lenses++lenses_md++-} (inner_ty_decl:mdataInstance_rep++deepTypeableRepInstance)++(imd_decl:md_decl:eqInsts++mdataInstance_md++deepTypeableMdInstance), md_ty_thunk')

makeForestEq :: Name -> Dec -> Q [Dec]
makeForestEq modeName dec = do
	[InstanceD _ (AppT (ConT eq) (AppT (AppT n mode) fs)) decs] <- deriveFromDec makeEq dec
	let icDataInst = InstanceD [ClassP ''ICRep [fs]] (AppT (ConT eq) (AppT (AppT n (PromotedT 'ICData)) fs)) decs
	let icExprInst = InstanceD [ClassP ''ICRep [fs]] (AppT (ConT eq) (AppT (AppT n (PromotedT 'ICExpr)) fs)) decs
	return [icDataInst,icExprInst]

thunksTy :: Name -> [(Pat,Type)] -> Type
thunksTy fsName pat_infos = thunksTy' $ map (Pure.appT2 (ConT ''ForestICThunkI) (VarT fsName) . snd) pat_infos
	where thunksTy' :: [Type] -> Type
	      thunksTy' = foldl1' (Pure.appT2 (ConT ''(:*:)))

-- named comprehensions will be loaded as directories and get their own @Forest_md@, whereas inlined comprehensions inside Directorys do not get their own @Forest_md@
type VST = (TH.Name, TH.Strict, Type)

genRepMDField :: Name -> Name -> Field -> Q (VST, VST)
genRepMDField modeName fsName (Simple (internal, isForm, external, ty, predM)) = do
	(rep_ty,md_ty) <- genRepMDTy modeName fsName ty
	let md_ty' = case predM of
		Nothing -> md_ty
		Just pred -> Pure.appT3 (ConT ''MDArgs) (VarT modeName) md_ty $ uTy fsName (ConT ''Bool)
	return ((Pure.getFieldName   internal, TH.NotStrict, rep_ty),(Pure.getFieldMDName internal, TH.NotStrict, md_ty'))
genRepMDField modeName fsName (Comp (info @ CompField {internalName, tyConNameOpt, descTy, ..})) = do
	(rep_ty, md_ty) <- genRepMDComp modeName fsName info
	return ((Pure.getFieldName   internalName, TH.NotStrict, {-fsthunkTy fsName -} rep_ty),(Pure.getFieldMDName internalName, TH.NotStrict, {-fsthunkTy fsName-} md_ty))

genRepMDComp modeName fsName (CompField {internalName, tyConNameOpt, descTy, predEOpt, ..}) = do
	(rng_rep_ty, rng_md_ty) <- genRepMDTy modeName fsName descTy
	let rng_md_ty' = if isJust predEOpt -- if there is a constraint
		then Pure.appT3 (ConT ''MDArgs) (VarT modeName) rng_md_ty $ Pure.tyListToTupleTy [Pure.appT2 (ConT ''ForestFSThunkI) (VarT fsName) (ConT ''FileInfo),Pure.appT2 (ConT ''ForestICThunkI) (VarT fsName) (ConT ''Bool)]
		else Pure.appT3 (ConT ''MDArgs) (VarT modeName) rng_md_ty $ Pure.appT2 (ConT ''ForestFSThunkI) (VarT fsName) (ConT ''FileInfo)
	(rep_ty, md_ty) <- case tyConNameOpt of 
		Nothing ->  return (mkStringListTy rng_rep_ty, mkStringListTy rng_md_ty')
		Just str -> do
			arity <- Pure.getTyConArity str
			case arity of 
				1 -> return (mkStringConTupleTy (mkName str) rng_rep_ty, mkStringConTupleTy (mkName str) rng_md_ty') 
				2 -> return (mkStringConCurryTy (mkName str) rng_rep_ty, mkStringConCurryTy (mkName str) rng_md_ty') 
	return (fsthunkTy fsName rep_ty,fsthunkTy fsName md_ty)
	
genRepMDCompTy modeName fsName info = do
	(rep_ty, md'_ty) <- genRepMDComp modeName fsName info
	let md_ty  = Pure.tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName), md'_ty ]    -- md is a pair of a base md for the maybe and the underlying md.
	return (fsthunkTy fsName rep_ty,fsthunkTy fsName md_ty)


mkStringConTupleTy con ty = AppT (ConT con)  (Pure.tyListToTupleTy [ConT ''String, ty])
mkStringConCurryTy con ty = AppT (AppT (ConT con) (ConT ''String)) ty
mkStringListTy ty = AppT ListT (Pure.tyListToTupleTy [ConT ''String, ty])


{- Generate type and meta-data representations. -}
genRepMDTy :: Name -> Name -> ForestTy -> Q (Type, Type)
genRepMDTy modeName fsName ty = case ty of
	Directory _          -> error "Forest: Directory declarations must appear at the top level."
	FFile (ty_name,arg)   -> do
		let repTy = fsthunkTy fsName $ ConT (Pure.getTyName ty_name)
		let mdTy = fsthunkTy fsName $ Pure.tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName), fsthunkTy fsName $ ConT (Pure.getMDName ty_name)] 
		return (repTy,mdTy) 
	Archive archtype ty              -> do
		(rep_ty,md_ty) <- genRepMDTy modeName fsName ty
		return (fsthunkTy fsName rep_ty,fsthunkTy fsName $ Pure.tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName),md_ty])
	FSymLink              -> return (fsthunkTy fsName $ ConT ''FilePath, fsthunkTy fsName $ Pure.tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName), ConT ''Base_md])
	Named ty_name        -> return (Pure.appTyFS fsName $ Pure.getTyName ty_name, appTyModeFS' modeName fsName $ Pure.getMDName ty_name)
	FConstraint p ty pred -> do
		(rep_ty,md_ty) <- genRepMDTy modeName fsName ty
		return (rep_ty,Pure.appT3 (ConT ''MDArgs) (VarT modeName) md_ty $ uTy fsName $ ConT ''Bool)
	FMaybe ty            -> genRepMDMaybe modeName fsName ty
	Fapp ty arg          -> genRepMDTy modeName fsName ty
	FComp cinfo          -> genRepMDCompTy modeName fsName cinfo

uTy :: Name -> Type -> Type
uTy fsName ty = Pure.appT2 (ConT ''ForestICThunkI) (VarT fsName) ty

fsthunkTyQ :: Name -> Type -> GenQ Type
fsthunkTyQ fsName ty = do
	State.modify $ Set.insert (ClassP ''IncK [AppT (ConT ''IncForest) (VarT fsName),ty])
	return $ fsthunkTy fsName ty

fsthunkTy :: Name -> Type -> Type
fsthunkTy fsName ty = Pure.appT2 (ConT ''ForestFSThunkI) (VarT fsName) ty

predTy :: Pred -> Type
predTy (ClassP n tys) = appConT n tys

appConT :: Name -> [Type] -> Type
appConT con = Foldable.foldl' AppT (ConT con)

instance (ForestDiff fs arg,IncK (IncForest fs) Forest_err,IncK (IncForest fs) (ForestFSThunkI fs ((Forest_md fs, md), rep)),IncK (IncForest fs) arg,Eq rep,Eq md,IncK (IncForest fs) ((Forest_md fs, md), rep),MData NoCtx (ForestO fs) rep,MData NoCtx (ForestO fs) md,Data arg,Eq arg,MData NoCtx (ForestI fs) arg,ZippedICMemo fs,ICRep fs,Pads1 arg rep md) => ZippedICForest fs (Arg arg) (ForestFSThunkI fs ((Forest_md fs,md),rep)) where
	zloadScratch proxy marg pathfilter path tree getMD = marg >>= \arg -> doZLoadFile1 Proxy (Arg arg) pathfilter path tree getMD
	zloadDelta proxy (marg,darg) mpath tree (rep,getMD) path' df tree' dv = inside marg >>= \arg -> doZLoadDeltaFile1 (isEmptyDelta darg) (Arg arg) mpath path' tree df tree' dv (rep,getMD)
	zupdateManifestScratch proxy marg path tree rep man = lift (inside marg) >>= \arg -> doZManifestFile1 (Arg arg) path tree rep man
	zupdateManifestDelta proxy (marg,darg) path path' tree df tree' rep dv man = lift (inside marg) >>= \arg -> doZDeltaManifestFile1 (isEmptyDelta darg) (Arg arg) path path' tree df tree' rep dv man
	zdefaultScratch proxy marg path = inside marg >>= \arg -> doZDefaultFile1 (Arg arg) path

instance (IncK (IncForest fs) Forest_err,IncK (IncForest fs) (SymLink fs),IncK (IncForest fs) ((Forest_md fs, Base_md), FilePath),ZippedICMemo fs,ICRep fs) => ZippedICForest fs () (SymLink fs) where
	zloadScratch proxy args pathfilter path tree getMD = doZLoadSymLink path tree getMD
	zloadDelta proxy (margs,dargs) mpath tree (rep,getMD) path' df tree' dv = doZLoadDeltaSymLink mpath path' tree df tree' dv (rep,getMD)
	zupdateManifestScratch proxy args path tree rep man = doZManifestSymLink path tree rep man
	zupdateManifestDelta proxy (margs,dargs) path path' tree df tree' rep dv man = doZDeltaManifestSymLink path path' tree df tree' rep dv man
	zdefaultScratch proxy args path = doZDefaultSymLink path



