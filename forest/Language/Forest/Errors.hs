{-# LANGUAGE ScopedTypeVariables, UndecidableInstances, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TemplateHaskell, DeriveDataTypeable #-}

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


module Language.Forest.Errors where
import Language.Pads.Errors as Pads (ErrInfo)
import Language.Pads.Source as Pads
import Language.Pads.MetaData as Pads
import Text.PrettyPrint.Mainland as PP
import Data.Data
import Data.WithClass.MData
import Data.DeriveTH
import Data.WithClass.Derive.MData
import Data.WithClass.Derive.DeepTypeable
import Data.DeepTypeable
import Language.Haskell.TH.Syntax

data Forest_err = Forest_err { numErrors :: Int, errorMsg :: Maybe ErrMsg } deriving (Eq,Ord,Typeable,Data,Show)

data ErrMsg = ForestError String
            | ForestIOException String
            | ForestPredicateFailure
            | MissingFile String
            | MissingDirectory String
            | MatchFailure String
            | NotADirectory String
            | ConstraintViolation
            | SystemError Int
            | MultipleMatches FilePath [String]
			| WrongFileExtension String FilePath
			| PadsError Pads.ErrInfo
     deriving (Typeable, Data, Show, Eq, Ord)

$( derive makeMData ''ErrMsg )
$( derive makeMData ''Forest_err )
$( derive makeDeepTypeable ''ErrMsg )
$( derive makeDeepTypeable ''Forest_err )

cleanForestErr = Forest_err 0 Nothing

isValidForestErr :: Forest_err -> Bool
isValidForestErr err = Language.Forest.Errors.numErrors err == 0

padsError :: Base_md -> Forest_err
padsError (Base_md { Pads.numErrors = n, errInfo = err }) = Forest_err n (fmap PadsError err)

