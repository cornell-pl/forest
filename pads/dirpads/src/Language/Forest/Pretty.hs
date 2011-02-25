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