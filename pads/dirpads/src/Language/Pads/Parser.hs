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

type Parser = PS.Parser

baseTypeL = [("Pint", Pint)]
baseTypeM = Map.fromList baseTypeL

lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser (haskellStyle { reservedOpNames = ["="]})

whiteSpace  = PT.whiteSpace  lexer
identifier  = PT.identifier  lexer
reserved    = PT.reserved    lexer
reservedOp  = PT.reservedOp  lexer
charLiteral = PT.charLiteral lexer
commaSep1   = PT.commaSep1   lexer
parens      = PT.parens      lexer

padsDecl :: Parser PadsDecl
padsDecl = do { id <- identifier
              ; reservedOp "="
              ; ty <- padsTy
              ; return (PadsDecl(Id id,ty))
              }

padsTy :: Parser PadsTy
padsTy = do { base <- identifier
            ; case Map.lookup base baseTypeM of
                Nothing -> return (Pname base)
                Just ty -> return ty
            }
     <|> do { c <- charLiteral
            ; return (Plit c)
            }
     <|> do { tys <- parens padsTyList
            ; return (Ptuple tys)
            }

padsTyList :: Parser [PadsTy]
padsTyList = commaSep1 padsTy

runLex :: Show a => PS.Parser a -> String -> IO()
runLex p input 
  = parseTest (do { whiteSpace
                  ; x <- p
                  ; eof
                  ; return x
                  }) input

parse :: PS.Parser a -> SourceName -> String -> Either ParseError a
parse p sourceName input 
  = PP.parse (do { whiteSpace
                  ; x <- p
                  ; eof
                  ; return x
                  }) sourceName input

simple :: PS.Parser Char
simple = letter

result = parseTest simple " hello"

