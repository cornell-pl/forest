{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
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


module Language.Forest.Syntax where

import Data.Generics hiding (mkQ,everything)
import Language.Haskell.TH  as TH 
import Data.Generics.TH
--import Language.Forest.TH
--import Language.Haskell.TH.Instances.Lift
import Data.Set (Set(..))
import qualified Data.Set as Set

newtype ForestDecl = ForestDecl (String, Maybe TH.Pat, ForestTy)
   deriving (Ord, Eq, Data, Typeable, Show)

data ForestTy = Directory DirectoryTy 
              | File FileTy 
              | Gzip ForestTy
              | Tar ForestTy
              | Named String 
              | FMaybe ForestTy
              | SymLink
              | FConstraint TH.Pat ForestTy TH.Exp    {- pattern bound to underlying type, underlying type, predicate -}
              | Fapp ForestTy TH.Exp
              | FComp CompField
   deriving (Ord, Eq, Data, Typeable, Show)

data DirectoryTy = Record String [Field]
   deriving (Ord, Eq, Data, Typeable, Show)

type FileTy = (String, Maybe TH.Exp) -- type name, expression argument

-- internal name, isForm, external name, description type, optional predicate
type BasicField = (String, Bool, TH.Exp, ForestTy, Maybe TH.Exp)  

data Generator = Explicit TH.Exp | Matches TH.Exp
    deriving (Ord, Eq, Data, Typeable, Show)

data GeneratedPaths a = ExpPaths [FilePath] | MatchPaths a

data CompField = CompField 
        { internalName :: String
        , tyConNameOpt :: Maybe String
        , explicitName :: Maybe String
        , externalE    :: TH.Exp
        , descTy       :: ForestTy
        , generatorP   :: TH.Pat
        , generatorG   :: Generator
        , predEOpt     :: Maybe TH.Exp
        }
   deriving (Ord, Eq, Data, Typeable, Show)

data Field = Simple BasicField
           | Comp  CompField
   deriving (Ord, Eq, Data, Typeable, Show)

fieldnames = map fieldname
fieldname (Simple (x,_,_,_,_)) = x
fieldname (Comp c) = internalName c


