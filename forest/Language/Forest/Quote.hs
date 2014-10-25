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
    (forest)
    where

import Prelude hiding (exp, init)
import System.IO.Unsafe (unsafePerformIO)

import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Language.Forest.CodeGen
import qualified Language.Forest.Parser as P



parse :: Monad m
      => Loc
      -> P.Parser a
      -> String
      -> m a
parse loc p input = let
  fileName = loc_filename loc
  (line,column) = loc_start loc
  in case P.parse p fileName line column input of
       Left err -> unsafePerformIO $ fail $ show err
       Right x  -> return x


fparse1 p pToQ s
    = do  loc <- location
          x <- Language.Forest.Quote.parse loc p s
          pToQ x

fquasiquote1 p
    = QuasiQuoter  (error "parse expression")
                   (error "parse pattern")
                   (error "parse type")
                   (fparse1 p make_forest_declarations)

forest :: QuasiQuoter
forest  = fquasiquote1 P.forestDecls
    