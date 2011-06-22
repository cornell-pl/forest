{-# LANGUAGE NamedFieldPuns,RecordWildCards #-}

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


module Language.Pads.Pretty where
import Char (isPrint, ord)
import Numeric (showHex)
import qualified Data.Map as M

import Text.PrettyPrint.Mainland
import Language.Pads.Syntax


instance Pretty Lit where
   ppr (CharL c) | isPrint c   = text $ show c
                 | ord c == 0  = squotes $ text $ "\\0"
                 | otherwise   = squotes $ text $
                                 "\\x" ++ showHex (ord c) ""
   ppr (StringL s) = text $ show s


instance Pretty PadsTy where
    ppr (Ptuple tys) = parens (commasep (map ppr tys))
    ppr (Plit l) = ppr l
    ppr (Pname s) = text s



instance Pretty PadsDecl where
    ppr (PadsDecl (name,pat,padsty)) = ppr name <+>  text (show pat) <+> text "=" <+> ppr padsty



seplines :: Doc -> [Doc] -> Doc
seplines s = folddoc (\hd tl -> hd <> s </> tl)


whitesep = sep
field_ppr field_name ppr = text field_name   <+> equals <+> ppr
record_ppr str pprs  = namedty_ppr str (recordbody_ppr pprs)  
recordbody_ppr docs = 
       text "{" 
  <//> align (seplines comma docs) 
  <//> text "}"
tuple_ppr ds = (text "(" <//>
                    align (commasep ds ) <//>        
                text ")")

maybe_ppr d = case d of 
  Nothing -> text "Nothing"
  Just a -> ppr a




namedty_ppr str ph = hang 2 (text str <+/> ph)
-- host_t_ppr (Host_t h) = namedty_ppr "Host_t" (ppr h)

namedtuple_ppr :: String -> [Doc] -> Doc
namedtuple_ppr name pprls = group $ hang 2 (text name <+/> (tuple_ppr pprls))


list_ppr ds = (text "[---" <//>
                    align (seplines comma ds ) <//>        
                text "]")

--instance (Pretty a, Pretty b)  => Pretty (M.Map a b) where
--  ppr = map_ppr 
map_ppr d = list_ppr (map ppr (M.toList d))

string_ppr :: String -> Doc
string_ppr = ppr


namedlist_ppr :: String -> [Doc] -> Doc
namedlist_ppr name pprls = group $ hang 2 (text name <+/> (list_ppr pprls))










instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e) => Pretty (a, b, c, d, e) where
    ppr (a, b, c, d, e) = parens $ commasep [ppr a, ppr b, ppr c, ppr d, ppr e]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f) => Pretty (a, b, c, d, e, f) where
    ppr (a, b, c, d, e, f) = parens $ commasep [ppr a, ppr b, ppr c, ppr d, ppr e, ppr f]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g) => Pretty (a, b, c, d, e, f,g) where
    ppr (a, b, c, d, e, f, g) = parens $ commasep [ppr a, ppr b, ppr c, ppr d, ppr e, ppr f, ppr g]


