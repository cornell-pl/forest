{-# LANGUAGE TemplateHaskell, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses,
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

import qualified Language.Forest.Pure.CodeGen.Utils as Pure
import Language.Forest.IC.ICRep
import Data.DeepTypeable
import Language.Forest.IC.BX
import Data.WithClass.Derive.MData
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

import Data.Data
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
import Language.Forest.IC.CodeGen.Default
import Data.Maybe
import Language.Forest.IC.CodeGen.DeltaLoading
import Language.Forest.IC.CodeGen.Storing

{- Code generation routines -}
make_forest_declarations :: Maybe ICMode -> [ForestDecl] -> Q [Dec]
make_forest_declarations mode ds = fmap concat (mapM (make_forest_declaration mode) ds)

make_forest_declaration :: Maybe ICMode -> ForestDecl -> Q [Dec]
make_forest_declaration mb (ForestDecl (id, pats, forestTy)) = do
	let ty_name = Pure.getTyName id
	let unty_name = Pure.getUnTyName id
	let md_ty_name = Pure.getMDName id
	fsName <- newName "fs"
	modeName <- newName "mode"
	let mode = maybe modeName modeN mb
	let arg_infos = map (\pat -> (pat,patToTy pat)) pats
	(ty_decl, md_ty_decls, md_ty) <- genRepMDDecl modeName fsName forestTy (unty_name,ty_name) md_ty_name arg_infos  -- Generate representation and meta-data decls for padsTy

	loadM :: TH.Exp <- Reader.runReaderT (genLoadM ty_name md_ty_name forestTy arg_infos) (mode,fsName,Map.empty)
	loadDeltaM :: TH.Exp <- Reader.runReaderT (genLoadDeltaM (unty_name,ty_name) md_ty_name forestTy arg_infos) (mode,fsName,Map.empty)
	manifestM :: TH.Exp <- Reader.runReaderT (genManifestM ty_name md_ty_name forestTy arg_infos) (mode,fsName,Map.empty)
	defaultM :: TH.Exp <- genDefaultM ty_name md_ty_name forestTy arg_infos
	
	let mkInst modeT = genFInst modeT fsName  loadM loadDeltaM manifestM defaultM ty_name md_ty_name      forestTy arg_infos
	
	insts <- case mb of
		Nothing -> do
			inst1 <- mkInst $ PromotedT 'ICData
			inst2 <- mkInst $ PromotedT 'ICExpr
			return $ inst1++inst2
		Just ICData -> mkInst $ PromotedT 'ICData
		Just ICExpr -> mkInst $ PromotedT 'ICExpr

	return $ ty_decl:md_ty_decls++insts

genFInst modeT fsName loadM loadDeltaM manifestM defaultM ty_name md_ty_name forestTy pat_infos = do
	mdName <- newName "md"
	let (inst,mdT) = case pat_infos of
		[] -> (Pure.appT5 (ConT ''ICForest) modeT (VarT fsName) (TupleT 0) (Pure.appTyFS fsName ty_name) (VarT mdName) , (appTyModeFS'' modeT fsName md_ty_name))  
		otherwise -> (Pure.appT5 (ConT ''ICForest) modeT (VarT fsName) (Pure.forestTupleTy $ map (AppT (ConT ''Arg) . snd) pat_infos) (Pure.appTyFS fsName ty_name) (VarT mdName) , (appTyModeFS'' modeT fsName md_ty_name))
	let load_method = ValD (VarP 'loadScratch) (NormalB loadM) []
	let loadDelta_method = ValD (VarP 'loadDelta) (NormalB loadDeltaM) []
	let manifest_method = ValD (VarP 'updateManifestScratch) (NormalB manifestM) []
	let default_method = ValD (VarP 'IC.defaultMd) (NormalB defaultM) []

	let ctx = [ClassP ''Typeable [VarT fsName]
			,ClassP ''ForestMD [VarT fsName,(appTyModeFS'' modeT fsName md_ty_name)]
			,ClassP ''ForestOutput [VarT fsName,ConT ''ICThunk,ConT ''Inside]
			, (VarT mdName) ` EqualP` mdT
			]
	return $ [InstanceD ctx inst [load_method,loadDelta_method,manifest_method,default_method]]

-- | Generates representation and metadata type declarations for a Forest specification
genRepMDDecl :: Name -> Name -> ForestTy -> (Name,Name) -> Name -> [(TH.Pat,TH.Type)] -> Q (TH.Dec, [TH.Dec], TH.Type)
genRepMDDecl modeName fsName ty (unty_name,ty_name) md_ty_name pat_infos = case ty of
	Directory dirTy -> genRepMDDir False modeName fsName dirTy ty_name md_ty_name pat_infos
	(FConstraint _ (Directory dirTy) _) -> genRepMDDir True modeName fsName dirTy ty_name md_ty_name pat_infos
	otherwise -> do
		(rep,md) <- genRepMDTy modeName fsName ty
		let ty_dec = mk_newTyD fsName (unty_name,ty_name) rep
		mdataInstance <- deriveFromDec makeMData ty_dec
		deepTypeableInstance <- deriveFromDec makeDeepTypeable ty_dec
		forestRepInstance <- mkNewTypeForestRep modeName fsName (unty_name,ty_name) rep
		let md' = case pat_infos of
			[] -> md
			otherwise -> Pure.appT3 (ConT ''MDArgs) (VarT modeName) md (thunksTy fsName pat_infos) -- adds argument thunks to the metadata
		return (ty_dec, forestRepInstance:mdataInstance++deepTypeableInstance++[mk_TySynDMode modeName fsName md_ty_name md'], md') 

mkNewTypeForestRep :: Name -> Name -> (Name,Name) -> Type -> Q TH.Dec
mkNewTypeForestRep modeName fsName (unty_name,ty_name) rep = do
	thunkName <- newName "thunk"
	let content = ValD (VarP 'iso_rep_thunk) (NormalB $ InfixE (Just $ Pure.appE2 (ConE 'Iso) (VarE unty_name) (ConE ty_name)) (VarE 'isoComp) (Just $ VarE 'iso_rep_thunk)) []
	return $ InstanceD [ClassP ''ForestRep [rep,VarT thunkName]] (Pure.appT2 (ConT ''ForestRep) (AppT (ConT ty_name) (VarT fsName)) (VarT thunkName)) [content]
	
{- Generate a representation and meta-data type for maybe. -}
genRepMDMaybe :: Name -> Name -> ForestTy -> Q (TH.Type, TH.Type)
genRepMDMaybe modeName fsName ty = do
	(rep_orig, md_orig) <- genRepMDTy modeName fsName ty
	let rep_ty = AppT (ConT ''Maybe) rep_orig                 -- rep is Maybe ty where ty is rep of nested type
	let md'_ty = AppT (ConT ''Maybe) md_orig                  -- underyling md is Maybe of md of nested type
	let md_ty  = Pure.tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName), md'_ty ]    -- md is a pair of a base md for the maybe and the underlying md.
	return (fsthunkTy fsName rep_ty, fsthunkTy fsName md_ty)

{- Generate a representation and meta-data type for a directory with named fields. -}
genRepMDDir :: Bool -> Name -> Name -> DirectoryTy -> Name -> Name ->  [(TH.Pat,TH.Type)] -> Q (TH.Dec, [TH.Dec], TH.Type)
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

thunksTy :: Name -> [(TH.Pat,TH.Type)] -> TH.Type
thunksTy fsName pat_infos = thunksTy' $ map (Pure.appT2 (ConT ''ForestICThunkI) (VarT fsName) . snd) pat_infos
	where thunksTy' :: [TH.Type] -> TH.Type
	      thunksTy' = foldl1' (Pure.appT2 (ConT ''(:*:)))

-- named comprehensions will be loaded as directories and get their own @Forest_md@, whereas inlined comprehensions inside Directorys do not get their own @Forest_md@
type VST = (TH.Name, TH.Strict, TH.Type)

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
genRepMDTy :: Name -> Name -> ForestTy -> Q (TH.Type, TH.Type)
genRepMDTy modeName fsName ty = case ty of
	Directory _          -> error "Forest: Directory declarations must appear at the top level."
	File (ty_name,arg)   -> do
		let repTy = fsthunkTy fsName $ ConT (Pure.getTyName ty_name)
		let mdTy = fsthunkTy fsName $ Pure.tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName), fsthunkTy fsName $ ConT (Pure.getMDName ty_name)] 
		return (repTy,mdTy) 
	Archive archtype ty              -> do
		(rep_ty,md_ty) <- genRepMDTy modeName fsName ty
		return (fsthunkTy fsName rep_ty,fsthunkTy fsName $ Pure.tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName),md_ty])
	SymLink              -> return (fsthunkTy fsName $ ConT ''FilePath, fsthunkTy fsName $ Pure.tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName), ConT ''Base_md])
	Named ty_name        -> return (Pure.appTyFS fsName $ Pure.getTyName ty_name, appTyModeFS' modeName fsName $ Pure.getMDName ty_name)
	FConstraint p ty pred -> do
		(rep_ty,md_ty) <- genRepMDTy modeName fsName ty
		return (rep_ty,Pure.appT3 (ConT ''MDArgs) (VarT modeName) md_ty $ uTy fsName $ ConT ''Bool)
	FMaybe ty            -> genRepMDMaybe modeName fsName ty
	Fapp ty arg          -> genRepMDTy modeName fsName ty
	FComp cinfo          -> genRepMDCompTy modeName fsName cinfo

uTy :: Name -> TH.Type -> TH.Type
uTy fsName ty = Pure.appT2 (ConT ''ForestICThunkI) (VarT fsName) ty

fsthunkTy :: Name -> TH.Type -> TH.Type
fsthunkTy fsName ty = Pure.appT2 (ConT ''ForestFSThunkI) (VarT fsName) ty

