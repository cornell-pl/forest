{-# LANGUAGE DataKinds, TemplateHaskell #-}

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

module Language.Forest.Quote
    (ipads,forest,iforest)
    where

import Language.Forest.Pure.CodeGen.Utils as Pure
import Language.Forest.IC.BX
import Data.WithClass.Derive.DeepTypeable
import Data.WithClass.Derive.MData
import Data.DeriveTH
import Prelude hiding (exp, init)
import System.IO.Unsafe (unsafePerformIO)

import Language.Haskell.TH
import Language.Forest.FS.FSRep
import Language.Forest.IC.Generic
import Language.Forest.IC.ICRep
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Language.Forest.Pure.CodeGen as Pure
import Language.Forest.IC.CodeGen as IC
import qualified Language.Forest.Parser as P
import Language.Pads.Quote as P

import Language.Forest.Syntax

parse :: Monad m
      => ForestMode -> Loc
      -> P.Parser a
      -> String
      -> m a
parse mode loc p input = let
  fileName = loc_filename loc
  (line,column) = loc_start loc
  in case P.parse mode p fileName line column input of
       Left err -> error $ show err
       Right x  -> return x


fparse1 mode p pToQ s
    = do  loc <- location
          x <- Language.Forest.Quote.parse mode loc p s
          pToQ x

fquasiquote1 mode p = QuasiQuoter
	(error "parse expression")
	(error "parse pattern")
	(error "parse type")
	(fparse1 mode p $ make_decls mode)

fquasiquote1IC mb p = QuasiQuoter
	(error "parse expression")
	(error "parse pattern")
	(error "parse type")
	(fparse1 ICForest p $ IC.make_forest_declarations mb)

fquasiquote1z fsTys p = QuasiQuoter
	(error "parse expression")
	(error "parse pattern")
	(error "parse type")
	(fparse1 ICForest p $ flip IC.make_zforest_declarations' (Just fsTys))

make_decls PureForest = Pure.make_forest_declarations
make_decls ICForest = do
--	unzipped <- IC.make_forest_declarations Nothing
	zipped <- IC.make_zforest_declarations
	return $ {-unzipped ++ -} zipped

ipads :: QuasiQuoter
ipads = P.padsDerivation $ \dec -> do
	let (n,tyargs) = case dec of
		DataD _ n tyargs _ _ -> (n,map (VarT . tyVarBndrName) tyargs)
		NewtypeD _ n tyargs _ _ -> (n,map (VarT . tyVarBndrName) tyargs)
	mdata <- deriveFromDec makeMData dec
	deep <- deriveFromDec makeDeepTypeable dec
	let decty = foldl AppT (ConT n) tyargs
	let fsName = mkName "fs"
	let forestContent = InstanceD [ClassP ''ICRep [VarT fsName]] (Pure.appT3 (ConT ''ForestContent) (VarT fsName) decty decty) [ValD (VarP 'lens_content) (NormalB $ VarE 'idLensM) []]
	return $ mdata ++ deep ++ [forestContent]

tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV n) = n
tyVarBndrName (KindedTV n t) = n

-- | A quasi-quoter for Forest with pure functional data structures
forest :: QuasiQuoter
forest  = fquasiquote1 PureForest P.forestDecls

-- | A quasi-quoter for Forest with IC-specific data structures
iforest :: QuasiQuoter
--iforest  = fquasiquote1 ICForest P.forestDecls
iforest = fquasiquote1z [{-ConT 'TxVarFS,ConT 'TxICFS,-}ConT 'TxNILFS] P.forestDecls
	
--txforest :: QuasiQuoter
--txforest = fquasiquote1z (ConT 'TxVarFS) P.forestDecls
	
-- | A quasi-quoter for incremental forest with data thunks
idforest :: QuasiQuoter
idforest  = fquasiquote1IC (Just ICData) P.forestDecls

-- | A quasi-quoter for incremental forest with data and expression thunks
ieforest :: QuasiQuoter
ieforest  = fquasiquote1IC (Just ICExpr) P.forestDecls
    