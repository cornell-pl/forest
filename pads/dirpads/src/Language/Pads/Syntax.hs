{-# LANGUAGE DeriveDataTypeable #-}
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

module Language.Pads.Syntax where

import Data.Generics
import Language.Pads.RegExp
import System.FilePath.Glob
import qualified Language.Haskell.TH  as TH 

data Lit    = CharL Char | StringL String  | EorL | EofL | VoidL | RegL RE | GlobL String | Hid String | Hexp TH.Exp | IntL Integer
  deriving (Eq, Data, Typeable, Show)

litToExp :: Lit -> TH.Exp
litToExp lit = case lit of
  CharL   c -> TH.LitE (TH.CharL   c)
  StringL s -> TH.LitE (TH.StringL s)
  IntL    i -> TH.LitE (TH.IntegerL i)
--  RegL    r -> TH.AppE (TH.ConE (TH.mkName "RE")) (TH.LitE (TH.StringL r))
  RegL    r -> reToExp r
  GlobL   p -> TH.AppE (TH.ConE (TH.mkName "GL")) (TH.LitE (TH.StringL p))
  Hid    id -> TH.VarE (TH.mkName id)
  Hexp  exp -> exp

reToExp :: RE -> TH.Exp
reToExp re = case re of
  RE s -> TH.AppE (TH.ConE (TH.mkName "RE")) (TH.LitE (TH.StringL s))
  ReName id -> TH.VarE (TH.mkName id)

data TermCond = TyTC PadsTy | LengthTC TH.Exp
  deriving (Eq, Data, Typeable, Show)

type FieldInfo = (Maybe String, PadsTy, Maybe TH.Exp)

data PadsTy = Plit Lit
            | Pname String
            | Ptuple [PadsTy] 
            | Pmaybe PadsTy
            | Pline PadsTy
            | Ptry PadsTy
            | Papp PadsTy TH.Exp
            | Ptrans PadsTy PadsTy TH.Exp   {- Src, Dest, and pair of functions to do transformation -}
            | Ptypedef TH.Pat PadsTy TH.Exp  {- pattern bound to underlying type, underlying type, predicate -}
            | Precord String [FieldInfo]
            | Punion  String [FieldInfo]
            | Pswitch String TH.Exp [(TH.Pat, FieldInfo)]
            | Plist  PadsTy (Maybe PadsTy) (Maybe TermCond)
   deriving (Eq, Data, Typeable, Show)

newtype PadsDecl = PadsDecl (Id, Maybe TH.Pat, PadsTy)
   deriving (Eq, Data, Typeable, Show)

data Id = Id String
        | AntiId String
    deriving (Eq, Ord, Data, Typeable, Show)

