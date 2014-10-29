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

module Language.Forest.CodeGen where

import Data.DeepTypeable
import Language.Forest.BX
import Data.WithClass.Derive.MData
import Data.IORef
import Data.WithClass.Derive.DeepTypeable
import Control.Monad.Incremental
import Language.Forest.ValueDelta
import Language.Forest.Syntax as PS
import Language.Forest.MetaData
import Language.Forest.Errors
import Language.Forest.Generic
import qualified Language.Forest.Errors as E
import Language.Forest.Manifest
import Language.Forest.FS.FSDelta
import System.Directory
import System.FilePath.Posix
import Data.DeriveTH
import Data.WithClass.MData
--import Data.Derive.MDataWithClassForest

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Pads.Padsc hiding (lift)
import Language.Pads.TH
import Language.Forest.IO
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

--import qualified Control.Lens as L
--import Control.Lens.TH
import Language.Forest.CodeGen.Utils
import Language.Forest.CodeGen.Loading
import Data.Maybe
import Language.Forest.CodeGen.DeltaLoading
import Language.Forest.CodeGen.Storing

{- Code generation routines -}
make_forest_declarations :: [ForestDecl] -> Q [Dec]
make_forest_declarations ds = fmap concat (mapM make_forest_declaration ds)

make_forest_declaration :: ForestDecl -> Q [Dec]
make_forest_declaration (ForestDecl (id, pats, forestTy)) = do
   let ty_name    = getTyName    id
   let unty_name    = getUnTyName    id
   let md_ty_name = getMDName    id
   fsName <- newName "fs"
   let arg_infos = map (\pat -> (pat,patToTy pat)) pats
   (ty_decl, md_ty_decls, md_ty) <- genRepMDDecl fsName forestTy (unty_name,ty_name) md_ty_name arg_infos  -- Generate representation and meta-data decls for padsTy
   loadM :: TH.Exp        <- Reader.runReaderT (genLoadM   ty_name md_ty_name forestTy arg_infos) Map.empty
   loadDeltaM :: TH.Exp       <- Reader.runReaderT (genLoadDeltaM  (unty_name,ty_name) md_ty_name forestTy arg_infos) Map.empty
   manifestM :: TH.Exp <- Reader.runReaderT (genManifestM ty_name md_ty_name forestTy arg_infos) Map.empty
   forestInstance :: [Dec]   <- genFInst  fsName  loadM loadDeltaM manifestM ty_name md_ty_name      forestTy arg_infos
   return (   [ty_decl]    
           ++ md_ty_decls  
           ++ forestInstance
           )

genFInst fsName loadM loadDeltaM manifestM ty_name md_ty_name forestTy pat_infos = do
	let inst = case pat_infos of
		[] -> appT4 (ConT ''Forest) (VarT fsName) (TupleT 0) (appTyFS fsName ty_name) (appTyFS fsName md_ty_name)   -- Forest RepTy MDTy
		otherwise -> appT4 (ConT ''Forest) (VarT fsName) (forestTupleTy $ map (AppT (ConT ''Arg) . snd) pat_infos) (appTyFS fsName ty_name) (appTyFS fsName md_ty_name)
	let load_method = ValD (VarP $ mkName "loadScratch") (NormalB loadM) []
	let loadDelta_method = ValD (VarP $ mkName "loadDelta") (NormalB loadDeltaM) []
	let manifest_method = ValD (VarP $ mkName "updateManifestScratch") (NormalB manifestM) []

	let ctx = [ClassP ''Typeable [VarT fsName]
			,ClassP ''ForestMD [VarT fsName,(appTyFS fsName md_ty_name)]
			,ClassP ''ForestOutput [VarT fsName,ConT ''ICThunk,ConT ''Inside]
			]
	return $ [InstanceD ctx inst [load_method,loadDelta_method,manifest_method]]

-- | Generates representation and metadata type declarations for a Forest specification
genRepMDDecl :: Name -> ForestTy -> (Name,Name) -> Name -> [(TH.Pat,TH.Type)] -> Q (TH.Dec, [TH.Dec], TH.Type)
genRepMDDecl fsName ty (unty_name,ty_name) md_ty_name pat_infos = case ty of
	Directory dirTy -> genRepMDDir False fsName dirTy ty_name md_ty_name pat_infos
	(FConstraint _ (Directory dirTy) _) -> genRepMDDir True fsName dirTy ty_name md_ty_name pat_infos
	otherwise -> do
		(rep,md) <- genRepMDTy fsName ty
		let ty_dec = mk_newTyD fsName (unty_name,ty_name) rep
		mdataInstance <- deriveFromDec makeMData ty_dec
		deepTypeableInstance <- deriveFromDec makeDeepTypeable ty_dec
		forestRepInstance <- mkNewTypeForestRep fsName (unty_name,ty_name) rep
		let md' = case pat_infos of
			[] -> md
			otherwise -> tyListToTupleTy [md,thunksTy fsName pat_infos] -- adds argument thunks to the metadata
		return (ty_dec, forestRepInstance:mdataInstance++deepTypeableInstance++[mk_TySynD fsName md_ty_name md'], md') 

mkNewTypeForestRep :: Name -> (Name,Name) -> Type -> Q TH.Dec
mkNewTypeForestRep fsName (unty_name,ty_name) rep = do
	thunkName <- newName "thunk"
	let content = ValD (VarP 'iso_rep_thunk) (NormalB $ InfixE (Just $ appE2 (ConE 'Iso) (VarE unty_name) (ConE ty_name)) (VarE 'isoComp) (Just $ VarE 'iso_rep_thunk)) []
	return $ InstanceD [ClassP ''ForestRep [rep,VarT thunkName]] (appT2 (ConT ''ForestRep) (AppT (ConT ty_name) (VarT fsName)) (VarT thunkName)) [content]
	
{- Generate a representation and meta-data type for maybe. -}
genRepMDMaybe :: Name -> ForestTy -> Q (TH.Type, TH.Type)
genRepMDMaybe fsName ty = do
	(rep_orig, md_orig) <- genRepMDTy fsName ty
	let rep_ty = AppT (ConT ''Maybe) rep_orig                 -- rep is Maybe ty where ty is rep of nested type
	let md'_ty = AppT (ConT ''Maybe) md_orig                  -- underyling md is Maybe of md of nested type
	let md_ty  = tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName), md'_ty ]    -- md is a pair of a base md for the maybe and the underlying md.
	return (fsthunkTy fsName rep_ty, fsthunkTy fsName md_ty)

{- Generate a representation and meta-data type for a directory with named fields. -}
genRepMDDir :: Bool -> Name -> DirectoryTy -> Name -> Name ->  [(TH.Pat,TH.Type)] -> Q (TH.Dec, [TH.Dec], TH.Type)
genRepMDDir hasConstraint fsName (Record _ fields) ty_name md_ty_name pat_infos = do
	reps <- mapM (genRepMDField fsName) fields
	let (vsts', md_vsts') = unzip reps
	let inner_ty_name = getStructInnerName ty_name
	let derives      = [''Typeable,''Eq]
	let ty_con       = TH.RecC inner_ty_name vsts'
	let inner_ty_decl      = TH.DataD [] inner_ty_name [PlainTV fsName] [ty_con] derives
	let ty = fsthunkTy fsName $ AppT (ConT inner_ty_name) (VarT fsName)
	let ty_decl = mk_TySynD fsName ty_name ty
	let inner_md_name = getStructInnerMDName ty_name   -- ty name is the same as the declared pads type name
	let imd_con       = TH.RecC inner_md_name md_vsts'
	let imd_decl      = TH.DataD [] inner_md_name [PlainTV fsName] [imd_con] derives   -- declaration of line for nested components
	let imd_ty        = appTyFS fsName inner_md_name
	let md_ty         = fsthunkTy fsName $ tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName), {-fsthunkTy fsName-} imd_ty]
	let md_ty_thunk = case pat_infos of
		[] -> md_ty
		otherwise -> tyListToTupleTy [md_ty,thunksTy fsName pat_infos] -- adds argument thunks to the metadata
	let md_ty_thunk' = if hasConstraint then {-fsthunkTy fsName-} (tyListToTupleTy [md_ty_thunk,uTy fsName (ConT ''Bool)]) else md_ty_thunk
	let  md_decl       = mk_TySynD fsName md_ty_name md_ty_thunk'
	if length vsts' == 0
		then error ("Error: Directory " ++ (show ty_name) ++ " must contain at least one named field.")
		else do
--			lenses <- makeLensesForDec (L.set lensField (\n -> Just $ "lns_"++nameBase ty_name++"_"++n) lensRules) ty_decl
--			lenses_md <- makeLensesForDec (L.set lensField (\n -> Just $ "lns_"++nameBase ty_name++"_"++n) lensRules) imd_decl
			mdataInstance_rep <- deriveFromDec makeMData inner_ty_decl
			mdataInstance_md <- deriveFromDec makeMData imd_decl
			deepTypeableRepInstance <- deriveFromDec makeDeepTypeable inner_ty_decl
			deepTypeableMdInstance <- deriveFromDec makeDeepTypeable imd_decl
			return (ty_decl, {-lenses++lenses_md++-} (inner_ty_decl:mdataInstance_rep++deepTypeableRepInstance)++(imd_decl:md_decl:mdataInstance_md++deepTypeableMdInstance), md_ty_thunk')

thunksTy :: Name -> [(TH.Pat,TH.Type)] -> TH.Type
thunksTy fsName pat_infos = {-AppT (ConT ''Forest_args) $-} thunksTy' $ map (appT2 (ConT ''ForestICThunkI) (VarT fsName) . snd) pat_infos -- $ appT2 (ConT ''Thunks) (VarT fsName) $ forestTupleTy $ map snd pat_infos
	where thunksTy' :: [TH.Type] -> TH.Type
	      thunksTy' = foldl1' (appT2 (ConT ''(:*:)))

-- named comprehensions will be loaded as directories and get their own @Forest_md@, whereas inlined comprehensions inside Directorys do not get their own @Forest_md@
type VST = (TH.Name, TH.Strict, TH.Type)

genRepMDField :: Name -> Field -> Q (VST, VST)
genRepMDField fsName (Simple (internal, isForm, external, ty, predM)) = do
	(rep_ty,md_ty) <- genRepMDTy fsName ty
	let md_ty' = case predM of
		Nothing -> md_ty
		Just pred -> {-fsthunkTy fsName $-} tyListToTupleTy [md_ty,uTy fsName (ConT ''Bool)]	
	return ((getFieldName   internal, TH.NotStrict, rep_ty),(getFieldMDName internal, TH.NotStrict, md_ty'))
genRepMDField fsName (Comp (info @ CompField {internalName, tyConNameOpt, descTy, ..})) = do
	(rep_ty, md_ty) <- genRepMDComp fsName info
	return ((getFieldName   internalName, TH.NotStrict, {-fsthunkTy fsName -} rep_ty),(getFieldMDName internalName, TH.NotStrict, {-fsthunkTy fsName-} md_ty))

genRepMDComp fsName (CompField {internalName, tyConNameOpt, descTy, predEOpt, ..}) = do
	(rng_rep_ty, rng_md_ty) <- genRepMDTy fsName descTy
	let rng_md_ty' = if isJust predEOpt -- if there is a constraint
		then tyListToTupleTy [rng_md_ty,tyListToTupleTy [appT2 (ConT ''ForestFSThunkI) (VarT fsName) (ConT ''FileInfo),appT2 (ConT ''ForestICThunkI) (VarT fsName) (ConT ''Bool)]]
		else tyListToTupleTy [rng_md_ty,appT2 (ConT ''ForestFSThunkI) (VarT fsName) (ConT ''FileInfo)]
	(rep_ty, md_ty) <- case tyConNameOpt of 
		Nothing ->  return (mkStringListTy rng_rep_ty, mkStringListTy rng_md_ty')
		Just str -> do
			arity <- getTyConArity str
			case arity of 
				1 -> return (mkStringConTupleTy (mkName str) rng_rep_ty, mkStringConTupleTy (mkName str) rng_md_ty') 
				2 -> return (mkStringConCurryTy (mkName str) rng_rep_ty, mkStringConCurryTy (mkName str) rng_md_ty') 
	return ( rep_ty, md_ty)
	
genRepMDCompTy fsName info = do
	(rep_ty, md'_ty) <- genRepMDComp fsName info
	let md_ty  = tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName), md'_ty ]    -- md is a pair of a base md for the maybe and the underlying md.
	return (fsthunkTy fsName rep_ty,fsthunkTy fsName md_ty)


mkStringConTupleTy con ty = AppT (ConT con)  (tyListToTupleTy [ConT ''String, ty])
mkStringConCurryTy con ty = AppT (AppT (ConT con) (ConT ''String)) ty
mkStringListTy ty = AppT ListT (tyListToTupleTy [ConT ''String, ty])


{- Generate type and meta-data representations. -}
genRepMDTy ::  Name -> ForestTy -> Q (TH.Type, TH.Type)
genRepMDTy fsName ty = case ty of
	Directory _          -> error "Forest: Directory declarations must appear at the top level."
	File (ty_name,arg)   -> do
		let repTy = fsthunkTy fsName $ ConT (getTyName ty_name)
		let mdTy = fsthunkTy fsName $ tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName), fsthunkTy fsName $ ConT (getMDName ty_name)] 
		return (repTy,mdTy) 
	Archive archtype ty              -> do
		(rep_ty,md_ty) <- genRepMDTy fsName ty
		return (fsthunkTy fsName rep_ty,fsthunkTy fsName $ tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName),md_ty])
	SymLink              -> return (fsthunkTy fsName $ ConT ''FilePath, fsthunkTy fsName $ tyListToTupleTy [AppT (ConT ''Forest_md) (VarT fsName), ConT ''Base_md])
	Named ty_name        -> return (appTyFS fsName $ getTyName ty_name, appTyFS fsName $ getMDName ty_name)
	FConstraint p ty pred -> do
		(rep_ty,md_ty) <- genRepMDTy fsName ty
		return (rep_ty,{-fsthunkTy fsName $ -} tyListToTupleTy [md_ty,uTy fsName (ConT ''Bool)])
	FMaybe ty            -> genRepMDMaybe fsName ty
	Fapp ty arg          -> genRepMDTy fsName ty
	FComp cinfo          -> genRepMDCompTy fsName cinfo

uTy :: Name -> TH.Type -> TH.Type
uTy fsName ty = appT2 (ConT ''ForestICThunkI) (VarT fsName) ty

fsthunkTy :: Name -> TH.Type -> TH.Type
fsthunkTy fsName ty = appT2 (ConT ''ForestFSThunkI) (VarT fsName) ty

