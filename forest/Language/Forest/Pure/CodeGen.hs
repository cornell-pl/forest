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

module Language.Forest.Pure.CodeGen where

import Data.IORef
import Language.Forest.Syntax as PS
import Language.Forest.Pure.MetaData
import Language.Forest.Errors
import Language.Forest.Pure.Generic as Forest
import qualified Language.Forest.Errors as E
import Language.Forest.Manifest
import System.Directory
import System.FilePath.Posix
import Language.Forest.Pure.CodeGen.Loading
import Language.Forest.Pure.CodeGen.Storing
import Language.Forest.Pure.CodeGen.Default

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Pads.Padsc hiding (lift)
import Language.Pads.TH
import Language.Forest.TH
import Language.Forest.FS.FSRep

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
import Control.Monad.Reader (Reader(..),ReaderT(..))
import qualified Control.Monad.Reader as Reader

import Language.Forest.Pure.CodeGen.Utils
import Language.Forest.Pure.CodeGen.Loading
import Language.Forest.Pure.CodeGen.Storing

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
   loadM :: TH.Exp        <- genLoadM   ty_name md_ty_name forestTy arg_infos
   manifestM :: TH.Exp <- genManifestM ty_name md_ty_name forestTy arg_infos
   defaultM :: TH.Exp        <- genDefaultM   ty_name md_ty_name forestTy arg_infos
   forestInstance :: [Dec]   <- genFInst  fsName  loadM manifestM defaultM ty_name md_ty_name      forestTy arg_infos
   return (   [ty_decl]    
           ++ md_ty_decls  
           ++ forestInstance
           )

genFInst fsName loadM manifestM defaultM ty_name md_ty_name forestTy pat_infos = do
	let inst = case pat_infos of
		[] -> appT4 (ConT ''PureForest) (VarT fsName) (TupleT 0) (ConT ty_name) (ConT md_ty_name)   -- Forest RepTy MDTy
		otherwise -> appT4 (ConT ''PureForest) (VarT fsName) (forestTupleTy $ map ( snd) pat_infos) (ConT ty_name) (ConT md_ty_name)
	let load_method = ValD (VarP 'loadWithTree) (NormalB loadM) []
	let manifest_method = ValD (VarP 'updateManifestWithTree) (NormalB manifestM) []
	let default_method = ValD (VarP 'Forest.defaultMd) (NormalB defaultM) []

	let ctx = [ClassP ''FSRep [VarT fsName]
			,ClassP ''ForestMD [(ConT md_ty_name)]
			]
	return $ [InstanceD ctx inst [load_method,manifest_method,default_method]]

-- | Generates representation and metadata type declarations for a Forest specification
genRepMDDecl :: Name -> ForestTy -> (Name,Name) -> Name -> [(TH.Pat,TH.Type)] -> Q (TH.Dec, [TH.Dec], TH.Type)
genRepMDDecl fsName ty (unty_name,ty_name) md_ty_name pat_infos = case ty of
	Directory dirTy -> genRepMDDir False fsName dirTy ty_name md_ty_name pat_infos
	(FConstraint _ (Directory dirTy) _) -> genRepMDDir True fsName dirTy ty_name md_ty_name pat_infos
	otherwise -> do
		(rep,md) <- genRepMDTy fsName ty
		let ty_dec = mk_newTyD (unty_name,ty_name) rep
		return (ty_dec, [mk_TySynD md_ty_name md], md) 
	
{- Generate a representation and meta-data type for maybe. -}
genRepMDMaybe :: Name -> ForestTy -> Q (TH.Type, TH.Type)
genRepMDMaybe fsName ty = do
	(rep_orig, md_orig) <- genRepMDTy fsName ty
	let rep_ty = AppT (ConT ''Maybe) rep_orig                 -- rep is Maybe ty where ty is rep of nested type
	let md'_ty = AppT (ConT ''Maybe) md_orig                  -- underyling md is Maybe of md of nested type
	let md_ty  = tyListToTupleTy [(ConT ''Forest_md), md'_ty ]    -- md is a pair of a base md for the maybe and the underlying md.
	return (  rep_ty, md_ty)

{- Generate a representation and meta-data type for a directory with named fields. -}
genRepMDDir :: Bool -> Name -> DirectoryTy -> Name -> Name ->  [(TH.Pat,TH.Type)] -> Q (TH.Dec, [TH.Dec], TH.Type)
genRepMDDir hasConstraint fsName (Record _ fields) ty_name md_ty_name pat_infos = do
	reps <- mapM (genRepMDField fsName) fields
	let (vsts', md_vsts') = unzip reps
	let inner_ty_name = getStructInnerName ty_name
	let derives      = [''Typeable,''Eq,''Data]
	let ty_con       = TH.RecC inner_ty_name vsts'
	let inner_ty_decl      = TH.DataD [] inner_ty_name [] [ty_con] derives
	let ty = (ConT inner_ty_name)
	let ty_decl = mk_TySynD ty_name ty
	let inner_md_name = getStructInnerMDName ty_name   -- ty name is the same as the declared pads type name
	let imd_con       = TH.RecC inner_md_name md_vsts'
	let imd_decl      = TH.DataD [] inner_md_name [] [imd_con] derives   -- declaration of line for nested components
	let imd_ty        = ConT inner_md_name
	let md_ty         = tyListToTupleTy [(ConT ''Forest_md),imd_ty]
	let  md_decl       = mk_TySynD md_ty_name md_ty
	if length vsts' == 0
		then error ("Error: Directory " ++ (show ty_name) ++ " must contain at least one named field.")
		else do
			return (ty_decl,[inner_ty_decl,imd_decl,md_decl], md_ty)

-- named comprehensions will be loaded as directories and get their own @Forest_md@, whereas inlined comprehensions inside Directorys do not get their own @Forest_md@
type VST = (TH.Name, TH.Strict, TH.Type)

genRepMDField :: Name -> Field -> Q (VST, VST)
genRepMDField fsName (Simple (internal, isForm, external, ty, predM)) = do
	(rep_ty,md_ty) <- genRepMDTy fsName ty
	let md_ty' = case predM of
		Nothing -> md_ty
		Just pred -> md_ty	
	return ((getFieldName   internal, TH.NotStrict, rep_ty),(getFieldMDName internal, TH.NotStrict, md_ty'))
genRepMDField fsName (Comp (info @ CompField {internalName, tyConNameOpt, descTy, ..})) = do
	(rep_ty, md_ty) <- genRepMDComp fsName info
	return ((getFieldName   internalName, TH.NotStrict, {-fsthunkTy fsName -} rep_ty),(getFieldMDName internalName, TH.NotStrict, {-fsthunkTy fsName-} md_ty))

genRepMDComp fsName (CompField {internalName, tyConNameOpt, descTy, predEOpt, ..}) = do
	(rng_rep_ty, rng_md_ty) <- genRepMDTy fsName descTy
	(rep_ty, md_ty) <- case tyConNameOpt of 
		Nothing ->  return (mkStringListTy rng_rep_ty, mkStringListTy rng_md_ty)
		Just str -> do
			arity <- getTyConArity str
			case arity of 
				1 -> return (mkStringConTupleTy (mkName str) rng_rep_ty, mkStringConTupleTy (mkName str) rng_md_ty) 
				2 -> return (mkStringConCurryTy (mkName str) rng_rep_ty, mkStringConCurryTy (mkName str) rng_md_ty) 
	return ( rep_ty, md_ty)
	
genRepMDCompTy fsName info = do
	(rep_ty, md'_ty) <- genRepMDComp fsName info
	let md_ty  = tyListToTupleTy [ (ConT ''Forest_md), md'_ty ]    -- md is a pair of a base md for the maybe and the underlying md.
	return (rep_ty,md_ty)


mkStringConTupleTy con ty = AppT (ConT con)  (tyListToTupleTy [ConT ''String, ty])
mkStringConCurryTy con ty = AppT (AppT (ConT con) (ConT ''String)) ty
mkStringListTy ty = AppT ListT (tyListToTupleTy [ConT ''String, ty])


{- Generate type and meta-data representations. -}
genRepMDTy ::  Name -> ForestTy -> Q (TH.Type, TH.Type)
genRepMDTy fsName ty = case ty of
	Directory _          -> error "Forest: Directory declarations must appear at the top level."
	FFile (ty_name,arg)   -> do
		let repTy = ConT (getTyName ty_name)
		let mdTy = tyListToTupleTy [ (ConT ''Forest_md), ConT (getMDName ty_name)] 
		return (repTy,mdTy) 
	Archive archtype ty              -> do
		(rep_ty,md_ty) <- genRepMDTy fsName ty
		return (rep_ty,tyListToTupleTy [ (ConT ''Forest_md),md_ty])
	FSymLink              -> return (ConT ''FilePath,tyListToTupleTy [ (ConT ''Forest_md) , ConT ''Base_md])
	Named ty_name        -> return (ConT $ getTyName ty_name, ConT $ getMDName ty_name)
	FConstraint p ty pred -> do
		(rep_ty,md_ty) <- genRepMDTy fsName ty
		return (rep_ty,md_ty)
	FMaybe ty            -> genRepMDMaybe fsName ty
	Fapp ty arg          -> genRepMDTy fsName ty
	FComp cinfo          -> genRepMDCompTy fsName cinfo


