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
import Data.Maybe

import qualified Language.Pads.Parser as PadsP
import Language.Pads.Syntax

type Parser = PS.Parser

lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser (haskellStyle { reservedOpNames = ["=", "(:", ":)", "<=>", "{", "}", "::", "<|", "|>", "|", "->", "[:", ":]", "<-"],
                                           reservedNames   = ["is", "File", "Directory", "type", "matches", "Maybe", "as" ]})

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

haskellExp :: Parser (TH.Exp)
haskellExp = do 
   { reservedOp "<|"
   ; str <- manyTill anyChar (reservedOp "|>") 
   ; case LHM.parseExp str of
                 Left err    -> unexpected ("Failed to parse Haskell expression: " ++ err)
                 Right expTH -> return expTH
   } <?> "haskell expression"


forestTy :: Parser ForestTy
forestTy =   directoryTy
         <|> fileTy
         <|> maybeTy
         <|> symLinkTy
         <|> gzipTy
         <|> tarTy
         <|> try fnAppTy
         <|> namedTy
         <|> parenTy
         <?> "Forest type"

symLinkTy :: Parser ForestTy 
symLinkTy = do 
   { reserved "SymLink"
   ; return SymLink
   } <?> "symbolic link type"


fnTy   :: Parser ForestTy
fnTy   =  namedTy

parenTy :: Parser ForestTy
parenTy = parens forestTy

fnAppTy :: Parser ForestTy
fnAppTy = do { ty <- fnTy
             ; exp <- forestArg
             ; return (Fapp ty exp)
             } <?> "type function application"

maybeTy :: Parser ForestTy
maybeTy = do { reserved "Maybe"
             ; ty <- forestTy
             ; return (FMaybe ty)
             } <?> "Forest Maybe type"

gzipTy :: Parser ForestTy
gzipTy = do { reserved "Gzip"
             ; ty <- forestTy
             ; return (Gzip ty)
             } <?> "Forest Gzip type"

tarTy :: Parser ForestTy
tarTy = do { reserved "Tar"
             ; ty <- forestTy
             ; return (Tar ty)
             } <?> "Forest Tar type"

fileTy :: Parser ForestTy
fileTy = do { reserved "File"
            ; fileBodyTyParens
            } 
          <?> "Forest File type"

fileBodyTyParens :: Parser ForestTy
fileBodyTyParens =   parens fileBodyTy
                 <|> fileBodyTy

fileBodyTy :: Parser ForestTy
fileBodyTy = do { id <- identifier
                ; arg <- optionMaybe forestArg
                ; return (File (id, arg))
                }

forestArgR :: Parser TH.Exp
forestArgR = do
   { lit <- PadsP.lit
   ; return (litToExp lit)
   }

forestArg :: Parser TH.Exp
forestArg = forestArgR <|> parens forestArgR


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



compBody :: Parser(Generator, Maybe TH.Exp, Maybe TH.Exp)
compBody =  do
     { isMatch <- optionMaybe (reserved "matches")
     ; generatorE <- forestArg
     ; filterE <- optionMaybe filteredBy
     ; predE <- optionMaybe fieldPredicate
     ; if isJust isMatch 
          then return (Matches generatorE, filterE, predE)
          else return (Explicit generatorE, filterE, predE)
     }



compField :: String -> Parser Field
compField internal_name = do
          { reserved "is" 
          ; repTyConName <- optionMaybe (identifier)
          ; reservedOp "["
          ; explicitFileName <- optionMaybe asPattern
          ; externalE <- forestArg
          ; reservedOp "::"
          ; forest_ty <- forestTy
          ; reservedOp "|"
          ; strPat <- manyTill anyChar (reservedOp "<-")  
          ; generatorP <- case LHM.parsePat strPat of 
                              Left err    -> unexpected ("Failed to parse Haskell pattern in directory declaration for field "  ++ internal_name ++ ":" ++ err)
                              Right patTH -> return patTH
          ; (generatorE, filterEOpt, predEOpt) <- compBody
          ; reservedOp "]"
          ; let compField = CompField internal_name repTyConName explicitFileName externalE forest_ty generatorP generatorE filterEOpt predEOpt
          ; return (Comp compField)
          }

asPattern :: Parser String
asPattern = try (do
 { str <- identifier
 ; reserved "as"
 ; return str
 }
 )

filteredBy :: Parser TH.Exp
filteredBy = do { reserved   "filteredBy"
--                ; haskellExp
                ; forestArg
                }

fieldPredicate :: Parser TH.Exp
fieldPredicate = do { reserved   "where"
                    ; haskellExp
                    }
externalName :: String -> Parser TH.Exp
externalName internal = 
       (explicitExternalName internal)
   <|> (implicitExternalName internal)

explicitExternalName :: String -> Parser TH.Exp
explicitExternalName internal = do 
    { reserved "is"
    ; nameE <- forestArg
    ; reservedOp "::"                 
    ; return nameE
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

test2 = Language.Forest.Parser.parse forestDecls filename line column input 
        where filename = "test"
              line = 0
              column = 0
              input = "type Grads_d = Directory  { classes is  [: aclass :: Class_d (: \"07\"   :)  | aclass <- matches (RE \"classof[0-9][0-9]\") :] }"


