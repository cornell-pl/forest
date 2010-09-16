module Language.Forest.Parser where

import Language.Forest.Syntax as S

import Control.Monad (msum)
import Text.Parsec
import qualified Text.Parsec.String as PS
import Text.Parsec.Error
import qualified Text.Parsec.Prim as PP
import qualified Text.Parsec.Token as PT
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Language 
import Text.ParserCombinators.Parsec.Pos
import Language.Haskell.Meta as LHM
import Language.Haskell.TH as TH hiding (CharL)
import Data.Char

type Parser = PS.Parser

lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser (haskellStyle { reservedOpNames = ["=", "(:", ":)", "<=>", "{", "}", "::", "<|", "|>", "|", "->", "[:", ":]", "<-"],
                                           reservedNames   = ["is", "File", "Directory", "type" ]})

whiteSpace    = PT.whiteSpace  lexer
identifier    = PT.identifier  lexer
reserved      = PT.reserved    lexer
reservedOp    = PT.reservedOp  lexer
charLiteral   = PT.charLiteral lexer
stringLiteral = PT.stringLiteral  lexer
commaSep1     = PT.commaSep1   lexer
parens        = PT.parens      lexer
braces        = PT.braces      lexer
brackets      = PT.brackets    lexer

forestDecls :: Parser [ForestDecl]
forestDecls = do { decls <- many1 forestDecl
                 ; return decls}
{-
[forest| type Hosts_f  = File Hosts_t        -- Hosts_t is expected to be a PADS type
         type Simple_d = Directory 
                         { local  is "local.txt"  :: File Hosts_t
                         , remote is "remote.txt" :: Hosts_f }    |]
-}

forestDecl :: Parser ForestDecl
forestDecl = do { reserved "type"
                ; id <- identifier
                ; pat <- param
                ; ty <- forestTy
                ; return (ForestDecl(id, pat, replaceName id ty))
                } <?> "Forest Declaration"

forestTy :: Parser ForestTy
forestTy =   directoryTy
         <|> fileTy
         <|> namedTy
         <?> "Forest type"

fileTy :: Parser ForestTy
fileTy = do { reserved "File"
            ; id <- identifier
            ; return (File id)
            } 
          <?> "Forest File type"

namedTy :: Parser ForestTy
namedTy = do { id <- identifier
             ; return (Named id)
             }
          <?> "Forest named type"

directoryTy :: Parser ForestTy
directoryTy = do { reserved "Directory"
                 ; db <- dirBody
                 ; return (Directory db)
                 } 

dirBody :: Parser DirectoryTy
dirBody = recordTy

recordTy :: Parser DirectoryTy
recordTy = do { fields <- braces fieldList
              ; return (Record "temporary" fields)
              } <?> "Forest Record Type"

fieldList :: Parser [Field]
fieldList = commaSep1 field

field :: Parser Field
field = do { internal_name <- identifier
           ; fieldBody internal_name
           } <?> "Forest Field"

fieldBody :: String -> Parser Field
fieldBody internal_name = 
          (try (simpleField internal_name))
      <|> (compField internal_name)
          

simpleField :: String -> Parser Field
simpleField internal_name = do
           { external_exp <- externalName internal_name
           ; forest_ty <- forestTy
           ; predM <- optionMaybe fieldPredicate
           ; return (Simple (internal_name, external_exp, forest_ty, predM))
           } <?> "Simple Forest Field"


oldcompField :: String -> Parser Field
oldcompField internal_name = do
          { reserved "is" 
          ; reservedOp "[:"
          ; str <- manyTill anyChar (reservedOp "::") 
          ; externalE <- case LHM.parseExp str of
                             (Left err)    -> unexpected ("Failed to parse Haskell expression in directory declaration for field " ++ internal_name ++ ":" ++ err)
                             (Right expTH) -> return expTH
          ; forest_ty <- forestTy
          ; reservedOp "|"
          ; strPat <- manyTill anyChar (reservedOp "<-")  
          ; generatorP <- case LHM.parsePat strPat of 
                              Left err    -> unexpected ("Failed to parse Haskell pattern in directory declaration for field "  ++ internal_name ++ ":" ++ err)
                              Right patTH -> return patTH
          ; strGen <- manyTill anyChar (reservedOp ":]")
          ; generatorE <- case LHM.parseExp strGen of
                                (Left err)    -> unexpected ("Failed to parse Haskell generator expression in directory declaration for field " 
                                                            ++ internal_name ++ ":" ++ err)
                                (Right expTH) -> return expTH
          ; return (Comp (internal_name, externalE, forest_ty, generatorP, generatorE, Nothing))
          }

compBodyWhere :: String -> Parser (TH.Exp, Maybe TH.Exp)
compBodyWhere internal_name = do
     { genStr <- manyTill anyChar (reserved "where")
     ; generatorE <- case LHM.parseExp genStr of
                             (Left err)    -> unexpected ("Failed to parse Haskell generator expression in directory declaration for field " 
                                                          ++ internal_name ++ ":" ++ err)
                             (Right expTH) -> return expTH
     ; predStr <- manyTill anyChar eof
     ; predE <- case LHM.parseExp predStr of
                             (Left err)    -> unexpected ("Failed to parse Haskell generator predicate in directory declaration for field " 
                                                          ++ internal_name ++ ":" ++ err)
                             (Right expTH) -> return expTH
     ; return (generatorE, Just predE)
     }

compBodyNoWhere :: String -> Parser (TH.Exp, Maybe TH.Exp)
compBodyNoWhere internal_name = do 
     { genStr <- manyTill anyChar eof
     ; generatorE <- case LHM.parseExp genStr of
                             (Left err)    -> unexpected ("Failed to parse Haskell generator expression in directory declaration for field " 
                                                          ++ internal_name ++ ":" ++ err)
                             (Right expTH) -> return expTH
     ; return (generatorE, Nothing)
     }

compBody :: String -> Parser(TH.Exp, Maybe TH.Exp)
compBody internal_name =  
         (try (compBodyWhere internal_name))
     <|> (compBodyNoWhere internal_name)

compToEnd :: String -> Parser(TH.Exp, Maybe TH.Exp)
compToEnd internal_name = do 
     { compBodyStr <- manyTill anyChar (reservedOp ":]")
     ; (externalE, predOptE) <- case PP.parse (compBody internal_name) "" compBodyStr of
                  Left err -> unexpected ("Failed to parse body for field "++ (internal_name) ++ ". " ++ (show err))
                  Right x -> return x
     ; return (externalE, predOptE)
     }


compField :: String -> Parser Field
compField internal_name = do
          { reserved "is" 
          ; reservedOp "[:"
          ; str <- manyTill anyChar (reservedOp "::") 
          ; externalE <- case LHM.parseExp str of
                             (Left err)    -> unexpected ("Failed to parse Haskell expression in directory declaration for field " ++ internal_name ++ ":" ++ err)
                             (Right expTH) -> return expTH
          ; forest_ty <- forestTy
          ; reservedOp "|"
          ; strPat <- manyTill anyChar (reservedOp "<-")  
          ; generatorP <- case LHM.parsePat strPat of 
                              Left err    -> unexpected ("Failed to parse Haskell pattern in directory declaration for field "  ++ internal_name ++ ":" ++ err)
                              Right patTH -> return patTH
          ; (generatorE, predEOpt) <- compToEnd internal_name
          ; return (Comp (internal_name, externalE, forest_ty, generatorP, generatorE, predEOpt))
          }

newcompFieldUsingHSetComps :: String -> Parser Field
newcompFieldUsingHSetComps internal_name = do
          { reserved "is" 
          ; reservedOp "[:"
          ; str <- manyTill anyChar (reservedOp "::") 
          ; externalE <- case LHM.parseExp str of
                             (Left err)    -> unexpected ("Failed to parse Haskell expression in directory declaration for field " ++ internal_name ++ ":" ++ err)
                             (Right expTH) -> return expTH
          ; forest_ty <- forestTy
          ; reservedOp "|"
          ; strPat <- manyTill anyChar (reservedOp "<-") 
          ; generatorP <- case LHM.parsePat strPat of 
                              Left err    -> unexpected ("Failed to parse Haskell pattern in directory declaration for field "  ++ internal_name ++ ":" ++ err)
                              Right patTH -> return patTH 
          ; strGen <- manyTill anyChar (reservedOp ":]")  
          ; let input = "[" ++ str ++ "|" ++ strPat ++ "<-" ++ strGen ++ "]"
          ; compE <- case LHM.parseExp input of
                              (Left err)    -> unexpected ("Failed to parse Haskell generator expression in directory declaration for field " 
                                                          ++ internal_name ++ ":" ++ err)
                              (Right expTH) -> return expTH
          ; return (Comp (internal_name, externalE, forest_ty, generatorP, compE, Nothing))
          }

{-
*Examples.Simple Language.Haskell.TH Data.Map Monad> runQ [e| [x | x <- ls, True] |]
CompE [BindS (VarP x_0) (VarE ls_1632294373),NoBindS (ConE GHC.Bool.True),NoBindS (VarE x_0)]
-}

{-
[forest | type Nested_d (file_name :: String) = Directory 
               { hostIndex is (file_name++".txt")  :: File Hosts_t 
               , hosts is [: h :: File PtextDocument | h <- getNames (hostIndex) :]
               }  |]
-}

fieldPredicate :: Parser TH.Exp
fieldPredicate = do { reserved   "where"
                    ; reservedOp "<|"
                    ; str <- manyTill anyChar (reservedOp "|>")
                    ; case LHM.parseExp str of
                       Left err    -> unexpected ("Failed to parse Haskell expression: " ++ err ++ ".")
                       Right expTH -> return expTH
                    }
externalName :: String -> Parser TH.Exp
externalName internal = 
       (explicitExternalName internal)
   <|> (implicitExternalName internal)

explicitExternalName :: String -> Parser TH.Exp
explicitExternalName internal = do 
    { reserved "is"
    ; str <- manyTill anyChar (reservedOp "::") 
    ; case LHM.parseExp str of
               (Left err) -> unexpected ("Failed to parse Haskell expression in directory declaration for field " ++ internal ++ ":" ++ err)
               (Right expTH) -> return expTH
    }      
           
implicitExternalName :: String -> Parser TH.Exp
implicitExternalName internal = 
    if isLowerCase internal then do 
        { reservedOp "::"
        ; return (TH.LitE (TH.StringL internal))}
    else unexpected ("Directory label "++ internal ++" is not a valid Haskell record label.  Use an \"is\" clause to give an explicit external name.")     
    

isLowerCase [] = False
isLowerCase (x:xs) = isLower x


param :: Parser (Maybe TH.Pat)
param = do { str <- manyTill anyChar (reservedOp "=")
           ; pat <- if Prelude.null str then return Nothing
                    else case LHM.parsePat str of 
                              Left err    -> unexpected ("Failed to parse Haskell pattern: " ++ err)
                              Right patTH -> return (Just patTH)
           ; return pat                            
           } <?> "Forest parameter"

replaceName :: String -> ForestTy -> ForestTy
replaceName str ty = case ty of
  Directory (Record _ body) -> Directory(Record str body)
  otherwise -> ty



parse :: PS.Parser a -> SourceName -> Line -> Column -> String -> Either ParseError a
parse p fileName line column input 
  = PP.parse (do {  setPosition (newPos fileName line column)
                  ; whiteSpace
                  ; x <- p
                  ; eof
                  ; return x
                  }) fileName input


test = Language.Forest.Parser.parse forestDecls filename line column input 
        where filename = "test"
              line = 0
              column = 0
              input = "type Hosts_f  = File Hosts_t\ntype Simple_d = Directory\n{ local  is \"local.txt\"  :: File Hosts_t\n, remote is \"remote.txt\" :: Hosts_f }"   