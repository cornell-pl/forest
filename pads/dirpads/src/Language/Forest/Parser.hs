module Language.Forest.Parser where

import Language.Forest.Syntax as S

import Control.Monad (msum)
import Text.Parsec
import qualified Text.Parsec.String as PS
import Text.Parsec.Error
import Text.Parsec.Prim as PP
import qualified Text.Parsec.Token as PT
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Language 
import Text.ParserCombinators.Parsec.Pos
import Language.Haskell.Meta as LHM
import Language.Haskell.TH as TH hiding (CharL, StringL)

type Parser = PS.Parser

lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser (haskellStyle { reservedOpNames = ["=", "(:", ":)", "<=>", "{", "}", "::", "<|", "|>", "|", "->" ],
                                           reservedNames   = ["Line", "Trans", "using", "where", "data", "type", "Eor", "Void",
                                                              "Eof", "Maybe", "with", "sep", "term", "and", "length", "of",
                                                              "Try", "case", "constrain" ]})

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

forestDecl :: Parser ForestDecl
forestDecl =  undefined




parse :: PS.Parser a -> SourceName -> Line -> Column -> String -> Either ParseError a
parse p fileName line column input 
  = PP.parse (do {  setPosition (newPos fileName line column)
                  ; whiteSpace
                  ; x <- p
                  ; eof
                  ; return x
                  }) fileName input




