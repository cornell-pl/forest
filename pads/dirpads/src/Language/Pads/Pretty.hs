module Language.Pads.Pretty where
import Char (isPrint, ord)
import Numeric (showHex)

import Language.Pads.Syntax
import Text.PrettyPrint.Mainland

instance Pretty PadsTy where
    ppr Pint = text "Pint"
    ppr (Ptuple tys) = parens (commasep (map ppr tys))
    ppr (Plit c) | isPrint c   = text $ show c
                 | ord c == 0  = squotes $ text $ "\\0"
                 | otherwise   = squotes $ text $
                                 "\\x" ++ showHex (ord c) ""
    ppr (Pname s) = text s

instance Pretty PadsDecl where
    ppr (PadsDecl (name,padsty)) = ppr name <+> text "=" <+> ppr padsty

instance Pretty Id where
    ppr (Id ident)  = text ident
    ppr (AntiId v)  = ppr "$id:" <> ppr v

