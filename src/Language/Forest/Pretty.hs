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

module Language.Forest.Pretty where
import Language.Forest.Syntax
import Text.PrettyPrint.Mainland
import qualified Language.Haskell.TH as TH

seplines :: Doc -> [Doc] -> Doc
seplines s = folddoc (\hd tl -> hd <> s </> tl)

embracelines s xs =
    nest 4 (text "{" </>
         seplines s (map (nest 4 . ppr) xs)) 
    </> text "}"

ppr_simple (internal, isForm, external, ty, pred) = 
      text internal
 <+>  (if isForm then text "is" else text "matches")
 <+>  text (litToStr external)
 <+>  text "::"
 <+>  ppr ty 
 <> case pred of 
        Nothing -> text ""
        Just s -> text "predicates not yet implemented"

instance Pretty Field where
  ppr = ppr_field

ppr_field f = case f of
  Simple basic ->   ppr_simple basic
  Comp compfield -> text "compound fields not yet implemented"

instance Pretty DirectoryTy where
  ppr = ppr_directory

ppr_directory (Record name fields) = 
   text "Directory" <+/> embracelines comma fields

ppr_fileTy (str, expEM) = 
     text str 
 <+> case expEM of 
        Nothing -> empty
--        Just expE -> parens (text (Language.Haskell.TH.ppr expE))
        Just expE -> parens (text ("arguments not yet implemented"))

instance Pretty ForestTy where
 ppr = ppr_forestTy

ppr_forestTy ty = case ty of
  Directory dirTy -> ppr dirTy
  File fileTy     -> text "File" <+> ppr_fileTy fileTy
  Named string    -> text string

ppr_declKeyword ty = case ty of 
  Directory dirTy -> text "data"
  otherwise       -> text "type"

ppr_forestDecl (ForestDecl (name, pat, forestTy)) = 
      ppr_declKeyword forestTy
  <+> text name
  <+> text "="
  <+/> ppr forestTy

instance Pretty ForestDecl where
  ppr = ppr_forestDecl

ppr_decls decls = seplines (line <> line) (map ppr decls)


litToStr ( TH.LitE (TH.StringL s)) = "\"" ++ s ++ "\""