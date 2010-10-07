{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, NamedFieldPuns, ScopedTypeVariables #-}
module Language.Pads.GenPretty where
-- pattern guards
import Language.Pads.Padsc
import Language.Haskell.TH as TH hiding (ppr)
import Language.Pads.TH
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad

getTyNames :: TH.Type ->  S.Set TH.Name
getTyNames ty  = case ty of
    ConT name -> S.singleton name
    TupleT i -> S.empty
    ArrowT -> S.empty
    ListT -> S.empty
    AppT t1 t2 -> (getTyNames t1) `S.union` (getTyNames t2)

getTyNamesFromCon :: TH.Con -> S.Set TH.Name
getTyNamesFromCon con = case con of
  (NormalC name stys) -> S.unions (map (\(_,ty)   -> getTyNames ty) stys)
  (RecC name vstys)   -> S.unions (map (\(_,_,ty) -> getTyNames ty) vstys)
  (InfixC st1 name st2) -> getTyNames(snd st1) `S.union` getTyNames(snd st2)
  (ForallC tvb cxt con) -> getTyNamesFromCon con

mkPrettyInstance :: TH.Name -> Q ([TH.Dec])
mkPrettyInstance ty_name = do
      if ty_name `elem` [''Pint, ''Pchar, ''Pdigit, ''Ptext, ''Pstring, ''PstringFW, ''PstringME, ''PstringSE]
      then return [] 
      else do 
         let tyBaseName = nameBase ty_name
         let baseStr = strToLower tyBaseName
         let specificPprName = mkName (baseStr ++ "_ppr")
         let funName = mkName (strToLower (tyBaseName ++ "_ppr"))
         info <- reify ty_name
         decls <- case info of 
                   TyConI (NewtypeD [] ty_name' [] (NormalC ty_name'' [(NotStrict, AppT ListT ty)]) derives) -> do -- List
                     { let nestedTyNames = getTyNames ty
                     ; report True ("list case " ++ (nameBase ty_name''))
                     ; nestedDecls <- mapM mkPrettyInstance (S.toList nestedTyNames)
                     ; (itemsE,itemsP) <- doGenPE "list"
                     ; let mapE  = AppE (AppE (VarE 'map) (VarE 'ppr)) itemsE
                     ; let bodyE = AppE (AppE (VarE 'namedlist_ppr) (nameToStrLit ty_name)) mapE
                     ; let argP = ConP (mkName tyBaseName) [itemsP]
                     ; let clause = Clause [argP] (NormalB bodyE) []
                     ; return (concat nestedDecls ++ [FunD specificPprName [clause]] )
                     }
                   TyConI (NewtypeD [] ty_name' [] (NormalC ty_name'' [(NotStrict, ConT core_name)]) derives) -> do
                     { (argP, body) <- mkPatBody tyBaseName
                     ; let clause = Clause [argP] body []	
                     ; return [FunD specificPprName [clause]] 
                     }
                   TyConI (NewtypeD [] ty_name' [] (NormalC ty_name'' [(NotStrict, ty)]) derives) -> do
                     { let nestedTyNames = getTyNames ty
                     ; nestedDecls <- mapM mkPrettyInstance (S.toList nestedTyNames)
                     ; let (len, tys) = tupleTyToListofTys ty
                     ; (exps, pats) <- doGenPEs len "tuple"
                     ; let bodyE = AppE (AppE (VarE 'namedtuple_ppr) (LitE (StringL tyBaseName)))  (pprListEs exps)
                     ; let argP = ConP (mkName tyBaseName) [TupP pats]
                     ; let clause = Clause [argP] (NormalB bodyE) []	
                     ; return (concat nestedDecls ++ [FunD specificPprName [clause]])
                     }
                   TyConI (DataD [] ty_name' [] cons  derives) | isDataType cons -> do
                     { let nestedTyNames = S.unions (map getTyNamesFromCon cons)
                     ; nestedDecls <- mapM mkPrettyInstance (S.toList nestedTyNames)
                     ; (exp, pat) <- doGenPE "case_arg"
                     ; matches <- mapM mkClause cons
                     ; let caseE = CaseE exp matches
                     ; let clause = Clause [pat] (NormalB caseE) []	
                     ; return (concat nestedDecls ++ [FunD specificPprName [clause]] )
                     } 
                   TyConI (DataD [] ty_name' [] cons  derives) | isRecordType cons -> do
                     { report (length cons /= 1) ("GenPretty: record " ++ (nameBase ty_name')  ++ " did not have a single constructor.")
                     ; let nestedTyNames = S.unions (map getTyNamesFromCon cons)
                     ; nestedDecls <- mapM mkPrettyInstance (S.toList nestedTyNames)
                     ; clause <- mkRecord (L.head cons)
                     ; return (concat nestedDecls ++ [FunD specificPprName [clause]])
                     } 
                   otherwise -> do {report True "pattern didn't match"; return []} 
         let inst = AppT (ConT ''Pretty) (ConT ty_name)   
         let genericPprName = mkName "ppr"
         let ppr_method = ValD (VarP genericPprName) (NormalB (VarE specificPprName)) []
         return (decls ++ [InstanceD [] inst [ppr_method]])

isDataType cons = case cons of
  [] -> False
  (NormalC _ _ ) : rest -> True
  otherwise -> False

isRecordType cons = case cons of
  [] -> False
  (RecC _ _ ) : rest -> True
  otherwise -> False

mkPatBody core_name_str = do
  { (exp,pat) <- doGenPE "arg"
  ; let bodyE = AppE (AppE (VarE 'namedty_ppr) (LitE (StringL core_name_str)))  (pprE exp)
  ; let argP = ConP (mkName core_name_str) [pat]
  ; return (argP, (NormalB bodyE))
  }

mkPatBodyNoArg core_name_str = do
  { let bodyE = AppE (VarE 'text) (LitE (StringL core_name_str))  
  ; let argP = ConP (mkName core_name_str) []
  ; return (argP, (NormalB bodyE))
  }

mkClause con = case con of
     NormalC name [] -> do
        { (argP, body) <- mkPatBodyNoArg (nameBase name)
        ; return (Match argP body [])
        }
     NormalC name ty_args -> do
        { (argP, body) <- mkPatBody (nameBase name)
        ; return (Match argP body [])
        }
     otherwise -> error "mkClause not implemented for this kind of constructor."

mkRecord (RecC rec_name fields) = do 
  { fieldInfo <- mapM mkField fields
  ; let (recPs, recEs) = unzip fieldInfo
  ; let recP = RecP rec_name recPs
  ; let bodyE = AppE (AppE (VarE 'record_ppr) (nameToStrLit rec_name)) (ListE recEs)
  ; return (Clause [recP] (NormalB bodyE) [])
  }

mkField (field_name, _, ty) = do 
  { (expE, pat) <- doGenPE (nameBase field_name)
  ; let fieldE = AppE (AppE (VarE 'field_ppr) (nameToStrLit field_name)) (pprE expE)
  ; return ((field_name, pat), fieldE)
  }

nameToStrLit name = LitE (StringL (nameBase name))

--FieldPat = (Name, Pat)
-- RecC Name [VarStrictType]
-- VarStrictType = (Name, Strict, Type)

--source_t_ppr source = case source of
--  IP ip     -> namedty_ppr "IP" (ppr ip) 
--  Host host -> namedty_ppr "Host" (ppr host)




namedty_ppr str ph = hang 2 (text str <+/> ph)
-- host_t_ppr (Host_t h) = namedty_ppr "Host_t" (ppr h)

namedtuple_ppr :: String -> [Doc] -> Doc
namedtuple_ppr name pprls = group $ hang 2 (text name <+/> (tuple_ppr pprls))


list_ppr ds = (text "[" <//>
                    align (sep comma ds ) <//>        
                text "]")

namedlist_ppr :: String -> [Doc] -> Doc
namedlist_ppr name pprls = group $ hang 2 (text name <+/> (list_ppr pprls))

pprE argE = AppE (VarE 'ppr) argE
pprListEs argEs = ListE (map pprE argEs) 


pint_ppr :: Pint -> Doc
pint_ppr (Pint x) = ppr x

instance Pretty Pint where
 ppr = pint_ppr 

pstring_ppr (Pstring s) = ppr s

instance Pretty Pstring where
 ppr = pstring_ppr



tuple_ppr ds = (text "(" <//>
                    align (sep comma ds ) <//>        
                text ")")

recordbody_ppr docs = (text "{" <//>
                    align (sep comma docs) <//>        
                 text "}")
field_ppr field_name ppr = text field_name   <+> equals <+> ppr
record_ppr str pprs  = namedty_ppr str (recordbody_ppr pprs)


{-
time_t_ppr (Time_t{hours, minutes, seconds}) = let
  hours_p   = text "hours"   <+> equals <+> (hours_t_ppr   hours)
  minutes_p = text "minutes" <+> equals <+> (minutes_t_ppr minutes)
  seconds_p = text "seconds" <+> equals <+> (seconds_t_ppr seconds)
  in hang 2 (text "Time_t" <+/>  recordbody_ppr [hours_p, minutes_p, seconds_p])

-}