{-# LANGUAGE TypeFamilies,KindSignatures, DataKinds, ConstraintKinds, UndecidableInstances, FlexibleContexts, GeneralizedNewtypeDeriving, TemplateHaskell, QuasiQuotes, ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable, TypeSynonymInstances,FlexibleInstances #-}

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

module Language.Forest.IC.BaseTypes where

import Language.Forest.IC.Default
import Data.DeepTypeable
import Language.Haskell.TH.Syntax
import Language.Forest.Pure.Generic
import Language.Forest.Pure.MetaData hiding (Forest_md)
import Language.Forest.IC.MetaData 
import Language.Forest.Quote
import Language.Forest.Manifest
import Language.Pads.Padsc 
import Data.WithClass.MData
import Language.Forest.FS.FSRep
import Language.Forest.IC.ICRep

import Data.IORef
import Control.Monad.Incremental

[iforest|
  type TextFile   = File Text
  type BinaryFile = File Binary
  type AnyFile    = File Binary
|]


