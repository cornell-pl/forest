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
import Language.Haskell.TH as TH

type Parser = PS.Parser

lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser (haskellStyle { reservedOpNames = ["=", "(:", ":)", "<=>", "{", "}", "::", "<|", "|>"],
                                           reservedNames   = ["Pline", "Ptrans", "Pusing", "Pwhere"]})

whiteSpace  = PT.whiteSpace  lexer
identifier  = PT.identifier  lexer
reserved    = PT.reserved    lexer
reservedOp  = PT.reservedOp  lexer
charLiteral = PT.charLiteral lexer
commaSep1   = PT.commaSep1   lexer
parens      = PT.parens      lexer
braces      = PT.braces      lexer

replaceName :: String -> PadsTy -> PadsTy
replaceName str ty = case ty of
  Precord _ body -> Precord str body
  otherwise -> ty

padsDecl :: Parser PadsDecl
padsDecl = do { id <- identifier
              ; str <- manyTill anyChar (reservedOp "=")
              ; pat <- case (Prelude.null str, LHM.parsePat str) of 
                            (True, _) -> return Nothing
                            (_, Left err) -> unexpected ("Failed to parse Haskell pattern: " ++ err)
                            (_, Right patTH) -> return (Just patTH)
              ; ty <- padsTy
              ; return (PadsDecl(Id id,pat, replaceName id ty))
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
                 Left err    -> unexpected ("Failed to parse Haskell expression: " ++ err ++ " in Pads application")
                 Right expTH -> return (Papp ty expTH)
             } <?> "type function application"


lineTy :: Parser PadsTy
lineTy = do { reserved "Pline"
              ; ty <- padsTy
              ; return (Pline ty)
              } <?> "line type"

tupleTy :: Parser PadsTy
tupleTy = do { tys <- parens padsTyList
             ; return (Ptuple tys)
             } <?> "tuple type"

padsTyList :: Parser [PadsTy]
padsTyList = commaSep1 padsTy

recordTy :: Parser PadsTy
recordTy = do { fields <- braces fieldList
              ; return (Precord "" fields)   -- empty string is placeholder for record name, which will be filled in at decl level.
              } <?> "record type"

fieldList :: Parser [(Maybe String, PadsTy, Maybe TH.Exp)]
fieldList = commaSep1 field

{- Records 
[pads| Request = { i1 :: Pint, 
                         ',',
                   i2 :: Pint Pwhere <| i1 == i2 } |> |]
-}

field :: Parser (Maybe String, PadsTy, Maybe TH.Exp)
field = do { idM <- optionMaybe $ try fieldLabel
           ; ty  <- padsTy
           ; predM <- optionMaybe fieldPredicate
           ; return (idM, ty, predM)
           }
        

fieldLabel :: Parser String
fieldLabel = do { id <- identifier
                ; reservedOp "::"
                ; return id
                }

fieldPredicate :: Parser TH.Exp
fieldPredicate = do { reserved   "Pwhere"
                    ; reservedOp "<|"
                    ; str <- manyTill anyChar (reservedOp "|>")
                    ; case LHM.parseExp str of
                     Left err    -> unexpected ("Failed to parse Haskell expression: " ++ err ++ " in record declaration.")
                     Right expTH -> return expTH
                    }


transformTy :: Parser PadsTy
transformTy = do { reserved "Ptrans"
                 ; reservedOp "{" 
                 ; srcTy <- padsTy
                 ; reservedOp "<=>" 
                 ; dstTy <- padsTy
                 ; reserved "Pusing"
                 ; str <- manyTill anyChar (reservedOp "}")
                 ; case LHM.parseExp str of
                   Left err    -> unexpected ("Failed to parse Haskell expression: " ++ err ++ " in Ptrans.")
                   Right expTH -> return (Ptrans srcTy dstTy expTH)
                 } <?> "transform"

typedefTy :: Parser PadsTy
typedefTy = do { str1 <- manyTill anyChar (reservedOp "::")
               ; pat <- case LHM.parsePat str1 of
                             (Left err) -> unexpected ("Failed to parse Haskell pattern in Pwhere declaration: " ++ err)
                             (Right patTH) -> return patTH
               ; ty <- padsTy
               ; reserved "Pwhere"
               ; str2 <- manyTill anyChar eof
               ; case LHM.parseExp str2 of
                      Left err    -> unexpected ("Failed to parse Haskell expression: " ++ err ++ " in Pwhere declaration.")
                      Right expTH -> return (Ptypedef pat ty expTH)
               } <?> "where"

padsTy :: Parser PadsTy
padsTy = lineTy
     <|> transformTy 
     <|> tupleTy
     <|> recordTy
     <|> try fnAppTy
     <|> try typedefTy
     <|> litTy
     <|> idTy
     <?> "pads type"



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

