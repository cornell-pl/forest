module Language.Pads.Parser where 


import Language.Pads.Syntax as S

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
lexer = PT.makeTokenParser (haskellStyle { reservedOpNames = ["=", "(:", ":)", "<=>", "{", "}", "::", "<|", "|>", "|" ],
                                           reservedNames   = ["Line", "Trans", "using", "where", "data", "type", "Eor", 
                                                              "Eof", "Maybe", "with", "sep", "term", "and" ]})

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


replaceName :: String -> PadsTy -> PadsTy
replaceName str ty = case ty of
  Precord _ body -> Precord str body
  Punion  _ body -> Punion  str body
  otherwise -> ty

padsDecls :: Parser [PadsDecl]
padsDecls = do { decls <- many1 padsDecl
               ; return decls}

padsDecl :: Parser PadsDecl
padsDecl =   tyDecl
         <|> dataDecl


dataDecl :: Parser PadsDecl
dataDecl = do { reserved "data"
              ; id <- identifier
              ; pat <- param
              ; padsTy <- dataTy id
              ; return (PadsDecl(Id id, pat, padsTy))
              } <?> "Data Declaration"

tyDecl :: Parser PadsDecl
tyDecl = do { reserved "type"
            ; id <- identifier
            ; pat <- param
            ; ty <- padsTy
            ; return (PadsDecl(Id id, pat, replaceName id ty))
            } <?> "Type Declaration"

param :: Parser (Maybe TH.Pat)
param = do { str <- manyTill anyChar (reservedOp "=")
           ; pat <- case (Prelude.null str, LHM.parsePat str) of 
                            (True, _) -> return Nothing
                            (_, Left err) -> unexpected ("Failed to parse Haskell pattern: " ++ err)
                            (_, Right patTH) -> return (Just patTH)
           ; return pat                            
           }

dataTy :: String -> Parser PadsTy
dataTy str = unionTy str 


idTy   :: Parser PadsTy
idTy   = do { base <- identifier
            ; return (Pname base)
            } <?> "named type"

charlitTy :: Parser S.Lit
charlitTy = do { c <- charLiteral
               ; return (S.CharL c)
               } <?> "character literal type"

strlitTy :: Parser S.Lit
strlitTy = do { s <- stringLiteral
               ; return (S.StringL s)
               } <?> "string literal type"

eorlitTy :: Parser S.Lit
eorlitTy = do {reserved "Eor"
              ; return S.EorL
              }

eoflitTy :: Parser S.Lit
eoflitTy = do {reserved "Eof"
              ; return S.EofL
              }

lit :: Parser S.Lit 
lit =   charlitTy
    <|> strlitTy
    <|> eorlitTy
    <|> eoflitTy
    <?> "literal"

litTy :: Parser PadsTy
litTy = do { lit <- lit
           ; return (Plit lit)
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
lineTy = do { reserved "Line"
              ; ty <- padsTy
              ; return (Pline ty)
              } <?> "line type"

tupleTy :: Parser PadsTy
tupleTy = do { tys <- parens padsTyList
             ; return (Ptuple tys)
             } <?> "tuple type"

padsTyList :: Parser [PadsTy]
padsTyList = commaSep1 padsTy

unionTy :: String -> Parser PadsTy
unionTy str = do { branches <- branchList
                 ; return (Punion str branches)
                 } <?> "data type"

branchList :: Parser [(Maybe String, PadsTy, Maybe TH.Exp)]
branchList = sepBy1  branch (reservedOp "|")



branch :: Parser (Maybe String, PadsTy, Maybe TH.Exp)
branch = do { id    <- identifier
            ; tyM   <- optionMaybe padsTy
            ; predM <- optionMaybe fieldPredicate
            ; let ty = case tyM of {Nothing -> Plit (S.StringL id); Just ty' -> ty'}
            ; return (Just id, ty, predM)
            }

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
fieldPredicate = do { reserved   "where"
                    ; reservedOp "<|"
                    ; str <- manyTill anyChar (reservedOp "|>")
                    ; case LHM.parseExp str of
                     Left err    -> unexpected ("Failed to parse Haskell expression: " ++ err ++ ".")
                     Right expTH -> return expTH
                    }


transformTy :: Parser PadsTy
transformTy = do { reserved "Trans"
                 ; reservedOp "{" 
                 ; srcTy <- padsTy
                 ; reservedOp "<=>" 
                 ; dstTy <- padsTy
                 ; reserved "using"
                 ; str <- manyTill anyChar (reservedOp "}")
                 ; case LHM.parseExp str of
                   Left err    -> unexpected ("Failed to parse Haskell expression: " ++ err ++ " in Trans.")
                   Right expTH -> return (Ptrans srcTy dstTy expTH)
                 } <?> "transform type"

typedefTy :: Parser PadsTy
typedefTy = do { str1 <- manyTill anyChar (reservedOp "::")
               ; pat <- case LHM.parsePat str1 of
                             (Left err) -> unexpected ("Failed to parse Haskell pattern in where declaration: " ++ err)
                             (Right patTH) -> return patTH
               ; ty <- padsTy
               ; reserved "where"
               ; reservedOp "<|"
               ; str2 <- manyTill anyChar (reservedOp "|>")
               ; case LHM.parseExp str2 of
                      Left err    -> unexpected ("Failed to parse Haskell expression: " ++ err ++ " in where declaration.")
                      Right expTH -> return (Ptypedef pat ty expTH)
               } <?> "where type"

maybeTy :: Parser PadsTy 
maybeTy = do { reserved "Maybe"
             ; ty <- padsTy
             ; return (Pmaybe ty)
             } <?> "maybe type"

--       | Plist  PadsTy (Maybe PadsTy) (Maybe TermCond)
-- [pads| type Entries = [Pint] with sep (:',':) and term (:eof:)         |]
-- [pads| type Entries = [Pint] with sep (:',':) and term (:noSep:)       |]
-- [pads| type Entries = [Pint] with sep (:',':) and term (:length exp:)  |]
-- [pads| type Entries = [Pint] with sep (:',':) |]    -- keep parsing until get an error in element type

sortModifier (Left sep) =   (Just sep, Nothing)
sortModifier (Right term) = (Nothing, Just term)
sortModifiers mods = 
    let (seps, terms) = unzip $ map sortModifier mods
    in  (msum seps, msum terms)

sep :: Parser PadsTy
sep = do { reserved "sep"
         ; reservedOp "(:"
         ; ty <- padsTy
         ; reservedOp ":)"
         ; return ty 
         } <?> "separator"
     
termKind :: Parser TermCond
termKind = do { reserved "length"
              ; str <- manyTill anyChar (reservedOp ":)")
              ; case LHM.parseExp str of
                      Left err    -> unexpected ("Failed to parse Haskell expression: " ++ err ++ " in list length declaration.")
                      Right expTH -> return (LengthTC expTH)
              }
       <|> do { reserved "noSep"
              ; reservedOp ":)"
              ; return NoSepTC
              }
       <|> do { ty <- padsTy
              ; reservedOp ":)"
              ; return (TyTC ty)
              }

term :: Parser TermCond
term = do { reserved "term"
          ; reservedOp "(:"
          ; termKind
          } <?> "terminator"
  

listMod :: Parser (Either PadsTy TermCond)
listMod =   do { sepMod <- sep
                ; return (Left sepMod) }
         <|> do { termMod <- term
                ; return (Right termMod) }
         <?> "list modifier"

listMods :: Parser (Maybe PadsTy, Maybe TermCond)
listMods = do { reservedOp "with"
               ; modifiers <- sepBy1 listMod (reservedOp "and")
               ;  return (sortModifiers modifiers)
               }
         <|> (return (Nothing, Nothing))
         <?> "list modifiers"

listTy :: Parser PadsTy
listTy = do { elementTy <- brackets padsTy
             ; (sepM, termM) <- listMods
             ; return (Plist elementTy sepM termM)
             } <?> "list type"    

padsTy :: Parser PadsTy
padsTy = lineTy
     <|> transformTy 
     <|> tupleTy
     <|> recordTy
     <|> maybeTy
     <|> listTy
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

