{-# LANGUAGE ScopedTypeVariables, UndecidableInstances, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, StandaloneDeriving, DeriveDataTypeable, TemplateHaskell #-}
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

import Language.Pads.Syntax
import Language.Pads.Padsc
import Language.Haskell.TH.Instances
import Data.Typeable
import Data.Data
--import Data.Generics hiding (mkQ,everything)
import Language.Haskell.TH  as TH 
--import Data.Generics.TH
--import Language.Forest.TH
--import Language.Haskell.TH.Instances.Lift
import Data.Set (Set(..))
import qualified Data.Set as Set
import System.FilePath.Posix
import Data.Char

import Language.Haskell.TH.Syntax
import Data.WithClass.MData
import Data.DeepTypeable
import Data.WithClass.Derive.DeepTypeable
import Data.DeriveTH
import Data.WithClass.Derive.MData

-- The mode in which forest is run, which affects parsing
data ForestMode = PureForest | ICForest deriving (Eq,Show,Typeable,Data)

newtype ForestDecl = ForestDecl (Bool,String,[TH.Pat], ForestTy)
   deriving ( Eq, Data, Typeable, Show)

data ForestTy = Directory DirectoryTy 
              | FFile FileTy 
              | Archive [ArchiveType] ForestTy -- list of archive types, e.g., tar.gz
              | Named String 
              | FMaybe ForestTy
              | FSymLink
              | FConstraint TH.Pat ForestTy TH.Exp    {- pattern bound to underlying type, underlying type, predicate -}
              | Fapp ForestTy [TH.Exp] -- non-empty list of arguments
              | FComp CompField
   deriving ( Eq, Data, Typeable, Show)

addArchiveTypes :: FilePath -> [ArchiveType] -> FilePath
addArchiveTypes = foldl (\path -> addExtension path . showArchiveType)

data ArchiveType = Gzip | Tar | Zip | Bzip | Rar
	deriving ( Eq, Data, Typeable, Show)

data DirectoryTy = Record String [Field]
   deriving ( Eq, Data, Typeable, Show)

type FileTy = (String, Maybe TH.Exp) -- type name, expression argument

-- internal name, isForm, external name, description type, optional predicate
type BasicField = (String, Bool, TH.Exp, ForestTy, Maybe TH.Exp)  

data Generator = Explicit TH.Exp | Matches TH.Exp
    deriving ( Eq, Data, Typeable, Show)

data GeneratedPaths a = ExpPaths [FilePath] | MatchPaths a

data CompField = CompField 
        { internalName :: String
        , tyConNameOpt :: Maybe String -- container type
        , explicitName :: Maybe String
        , externalE    :: TH.Exp
        , descTy       :: ForestTy -- the Forest type for values of the container
        , generatorP   :: TH.Pat
		, generatorTy :: Maybe (String,Maybe TH.Exp) -- the Pads type for keys of the container
        , generatorG   :: Generator
        , predEOpt     :: Maybe TH.Exp
        }
   deriving ( Eq, Data, Typeable, Show)
--[ explicitName :: descTy | generatorP :: <- generatorE , predEOpt ]

data Field = Simple BasicField
           | Comp  CompField
   deriving ( Eq, Data, Typeable, Show)

isSimpleField :: Field -> Bool
isSimpleField (Simple _) = True
isSimpleField (Comp _) = False

archiveExtension :: [ArchiveType] -> String
archiveExtension = foldl1 (\ext1 ext2 -> ext1 ++ "." ++ ext2) . map showArchiveType

showArchiveType :: ArchiveType -> String
showArchiveType Gzip = "gz"
showArchiveType Tar = "tar"
showArchiveType Zip = "zip"
showArchiveType Bzip = "bz2"
showArchiveType Rar = "rar"

fieldnames :: [Field] -> [String]
fieldnames = map fieldname
fieldname :: Field -> String
fieldname (Simple (x,_,_,_,_)) = x
fieldname (Comp c) = internalName c

--deriving instance Ord Exp
--deriving instance Ord Pat
--deriving instance Ord Lit
--deriving instance Ord Type
--deriving instance Ord TyLit
--deriving instance Ord TyVarBndr
--deriving instance Ord Pred
--deriving instance Ord Guard
--deriving instance Ord Stmt
--deriving instance Ord Dec
--deriving instance Ord Body
--deriving instance Ord Con
--deriving instance Ord Strict
--deriving instance Ord FamFlavour
--deriving instance Ord TH.Fixity
--deriving instance Ord Range
--deriving instance Ord TH.FixityDirection
--deriving instance Ord Foreign
--deriving instance Ord TH.Match
--deriving instance Ord Callconv
--deriving instance Ord Safety
--deriving instance Ord Pragma
--deriving instance Ord AnnTarget
--deriving instance Ord Inline
--deriving instance Ord Role
--deriving instance Ord Phases
--deriving instance Ord TySynEqn
--deriving instance Ord RuleBndr
--deriving instance Ord FunDep
--deriving instance Ord RuleMatch
--deriving instance Ord Clause

