module Language.Pads.Parser where 

import Data.Map as Map

import Language.Pads.Syntax

import Text.Parsec
import qualified Text.Parsec.String as PS
import Text.Parsec.Error
import Text.Parsec.Prim as PP
import qualified Text.Parsec.Token as PT
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Language 
import Text.ParserCombinators.Parsec.Pos
import Language.Haskell.Meta as LHM

type Parser = PS.Parser

lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser (haskellStyle { reservedOpNames = ["=", "(:", ":)"],
                                           reservedNames   = ["Precord"]})

whiteSpace  = PT.whiteSpace  lexer
identifier  = PT.identifier  lexer
reserved    = PT.reserved    lexer
reservedOp  = PT.reservedOp  lexer
charLiteral = PT.charLiteral lexer
commaSep1   = PT.commaSep1   lexer
parens      = PT.parens      lexer

padsDecl :: Parser PadsDecl
padsDecl = do { id <- identifier
--              ; args <- sepBy identifier whiteSpace
              ; str <- manyTill anyChar (reservedOp "=")
              ; pat <- case (Prelude.null str, LHM.parsePat str) of 
                            (True, _) -> return Nothing
                            (_, Left err) -> unexpected ("Failed to parse Haskell pattern: " ++ err)
                            (_, Right patTH) -> return (Just patTH)
              ; ty <- padsTy
              ; return (PadsDecl(Id id,pat,ty))
              }



idTy   :: Parser PadsTy
idTy   = do { base <- identifier
            ; return (Pname base)
            } <?> "named type"

litTy :: Parser PadsTy
litTy = do { c <- charLiteral
           ; return (Plit c)
           } <?> "literal type"

fnTy   :: Parser PadsTy
fnTy   =  idTy

fnAppTy :: Parser PadsTy
fnAppTy = do { ty <- fnTy
             ; reservedOp "(:"
             ; str <- manyTill anyChar (reservedOp ":)") 
             ; case LHM.parseExp str of
                 Left err    -> unexpected ("Failed to parse Haskell expression: " ++ err)
                 Right expTH -> return (Papp ty expTH)
             } <?> "type function application"


recordTy :: Parser PadsTy
recordTy = do { reserved "Precord"
              ; ty <- padsTy
              ; return (Precord ty)
              } <?> "record type"

tupleTy :: Parser PadsTy
tupleTy = do { tys <- parens padsTyList
             ; return (Ptuple tys)
             } <?> "tuple type"


padsTy :: Parser PadsTy
padsTy = recordTy
     <|> tupleTy
     <|> try fnAppTy
     <|> litTy
     <|> idTy
     <?> "pads type"

padsTyList :: Parser [PadsTy]
padsTyList = commaSep1 padsTy


runLex :: Show a => PS.Parser a -> String -> IO()
runLex p input 
  = parseTest (do { whiteSpace
                  ; x <- p
                  ; eof
                  ; return x
                  }) input



parse :: PS.Parser a -> SourceName -> Line -> Column -> String -> Either ParseError a
parse p fileName line column input 
  = PP.parse (do {  setPosition (newPos fileName line column)
                  ; whiteSpace
                  ; x <- p
                  ; eof
                  ; return x
                  }) fileName input



simple :: PS.Parser Char
simple = letter

result = parseTest simple " hello"

