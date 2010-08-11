module Language.Pads.Pretty where
import Char (isPrint, ord)
import Numeric (showHex)

import Text.PrettyPrint.Mainland
import Language.Pads.Syntax
import Language.Pads.Padsc

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

whitesep = sep space

instance Pretty PadsDecl where
    ppr (PadsDecl (name,pat,padsty)) = ppr name <+>  text (show pat) <+> text "=" <+> ppr padsty

instance Pretty Id where
    ppr (Id ident)  = text ident
    ppr (AntiId v)  = ppr "$id:" <> ppr v

instance Pretty a => Pretty (Result a) where
    ppr (Good r) = text "Good:" <+> ppr r
    ppr (Bad  r) = text "Bad:"  <+> ppr r

