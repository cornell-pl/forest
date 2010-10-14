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
import Text.Parsec.Expr
import Language.Haskell.Meta as LHM
import Language.Haskell.TH as TH hiding (CharL)
import Data.Char
import Language.Pads.RegExp as PRE 
import System.FilePath.Glob

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
integer       = PT.integer     lexer
commaSep1     = PT.commaSep1   lexer
parens        = PT.parens      lexer
braces        = PT.braces      lexer
brackets      = PT.brackets    lexer

lowerId :: Parser String
lowerId = try (do { id <- identifier
                  ; if (isLower . head) id then return id else parserZero })

upperId :: Parser String
upperId = try (do { id <- identifier
                  ; if (isUpper . head) id then return id else parserZero })


replaceName :: String -> PadsTy -> PadsTy
replaceName str ty = case ty of
  Precord _ body -> Precord str body
  Punion  _ body -> Punion  str body
  Pswitch _ cse body -> Pswitch str cse body
  otherwise -> ty

padsDecls :: Parser [PadsDecl]
padsDecls = do { decls <- many1 padsDecl
               ; return decls}

padsDecl :: Parser PadsDecl
padsDecl =   tyDecl
         <|> dataDecl


dataDecl :: Parser PadsDecl
dataDecl = do { reserved "data"
              ; id <- upperId
              ; pat <- param
              ; padsTy <- dataTy id
              ; return (PadsDecl(Id id, pat, padsTy))
              } <?> "Data Declaration"

tyDecl :: Parser PadsDecl
tyDecl = do { reserved "type"
            ; id <- upperId
            ; pat <- param
            ; ty <- padsTy
            ; return (PadsDecl(Id id, pat, replaceName id ty))
            } <?> "Type Declaration"

haskellExp :: Parser (TH.Exp)
haskellExp = do 
   { reservedOp "<|"
   ; str <- manyTill anyChar (reservedOp "|>") 
   ; case LHM.parseExp str of
                 Left err    -> unexpected ("Failed to parse Haskell expression: " ++ err)
                 Right expTH -> return expTH
   } <?> "haskell expression"

param :: Parser (Maybe TH.Pat)
param = do { str <- manyTill anyChar (reservedOp "=")
           ; pat <- if Prelude.null str then return Nothing
                    else case LHM.parsePat str of 
                              Left err    -> unexpected ("Failed to parse Haskell pattern: " ++ err)
                              Right patTH -> return (Just patTH)
           ; return pat                            
           }

dataTy :: String -> Parser PadsTy
dataTy str =   unionTy str 
           <|> switchTy str
           <|> recordTy2 str


idTy   :: Parser PadsTy
idTy   = do { base <- upperId
            ; return (Pname base)
            } <?> "named type"

hidlitTy :: Parser S.Lit
hidlitTy =  do { hid <- lowerId
            ; return (Hid hid)
            } <?> "haskell identifier literal"

hexplitTy :: Parser S.Lit
hexplitTy =  do { hexp <- haskellExp
                ; return (Hexp hexp)
                } <?> "haskell expression literal"

charlitTy :: Parser S.Lit
charlitTy = do { c <- charLiteral
               ; return (S.CharL c)
               } <?> "character literal type"

intlitTy :: Parser S.Lit
intlitTy = do { i <- integer
               ; return (S.IntL i)
               } <?> "integer literal type"

strlitTy :: Parser S.Lit
strlitTy = do { s <- stringLiteral
               ; return (S.StringL s)
               } <?> "string literal type"

eorlitTy :: Parser S.Lit
eorlitTy = do {reserved "Eor"
              ; return S.EorL
              }

eoflitTy :: Parser S.Lit
eoflitTy = do { reserved "Eof"
              ; return S.EofL
              }

voidlitTy :: Parser S.Lit
voidlitTy = do {reserved "Void"
              ; return S.VoidL
              }

globlitTy :: Parser S.Lit
globlitTy = do
 { reserved "GL"
 ; s <- stringLiteral
 ; return (S.GlobL s)
 }

reglitTy :: Parser S.Lit
reglitTy = do { reserved "RE"
              ; s <- stringLiteral
              ; return (S.RegL (RE s))
              }

reglitTy2 :: Parser S.Lit
reglitTy2 = do 
   { reserved "re"
   ; str <- manyTill (noneOf " ") (oneOf " \t\n")
   ; whiteSpace
   ; case Text.Parsec.parse reP "" str of
         Left err -> unexpected ("Failed to parse regular expression: " ++ str)
         Right s  -> return (S.RegL s)
   }

lit :: Parser S.Lit 
lit =   charlitTy
    <|> strlitTy
    <|> eorlitTy
    <|> eoflitTy
    <|> voidlitTy
    <|> reglitTy
--    <|> reglitTy2           Doesn't really work; can't find end of regular expression
    <|> globlitTy
    <|> intlitTy
    <|> hidlitTy
    <|> hexplitTy
    <?> "literal"

litTy :: Parser PadsTy
litTy = do { lit <- lit
           ; return (Plit lit)
           } <?> "literal type"

fnTy   :: Parser PadsTy
fnTy   =  idTy

fnAppTy :: Parser PadsTy
fnAppTy = do { ty <- fnTy
             ; lit <- lit <|> parens lit
             ; return (Papp ty (litToExp lit))
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

switchTy :: String -> Parser PadsTy
switchTy str = do { reservedOp "case"
                  ; reserved "<|"
                  ; str <- manyTill anyChar (reservedOp "|>")
                  ; caseE <- case LHM.parseExp str of
                                Left err    -> unexpected ("Failed to parse Haskell expression: " ++ err ++ ".")
                                Right expTH -> return expTH
                  ; reservedOp "of"
                  ; branches <- switchBranchList
                  ; return (Pswitch str caseE branches)
                  } <?> "switch type"

switchBranchList :: Parser [(TH.Pat, FieldInfo)]
switchBranchList = sepBy1 switchBranch (reservedOp "|")

switchBranch :: Parser (TH.Pat, FieldInfo)
switchBranch = do { str <- manyTill anyChar (reservedOp "->")
                  ; pat <- case LHM.parsePat str of 
                            (Left err)    -> unexpected ("Failed to parse Haskell pattern: " ++ err)
                            (Right patTH) -> return patTH
                  ; br <- branch
                  ; return (pat, br)
                  } <?> "switchBrach"


unionTy :: String -> Parser PadsTy
unionTy str = do { branches <- branchList
                 ; return (Punion str branches)
                 } <?> "data type"


branchList :: Parser [(Maybe String, PadsTy, Maybe TH.Exp)]
branchList = sepBy1  branch (reservedOp "|")



branch :: Parser (Maybe String, PadsTy, Maybe TH.Exp)
branch = do { id    <- upperId
            ; tyM   <- optionMaybe padsTy
            ; predM <- optionMaybe fieldPredicate
            ; let ty = case tyM of {Nothing -> Plit (S.StringL id); Just ty' -> ty'}
            ; return (Just id, ty, predM)
            }

recordTy :: Parser PadsTy
recordTy = do { reservedOp "{"
              ; fields <- fieldList
              ; reservedOp "}"
              ; return (Precord "" fields)   -- empty string is placeholder for record name, which will be filled in at decl level.
              } <?> "record type"

recordTy2 :: String -> Parser PadsTy
recordTy2 str = do { 
                   --id <- identifier ; 
                   reservedOp "{"
--                   ; if str == id then return () else error ("Record name ("++ id ++") must match declared type name: (" ++str ++").")
                   ; fields <- fieldList
                   ; reservedOp "}"
                   ; return (Precord str fields)   -- empty string is placeholder for record name, which will be filled in at decl level.
                   } <?> "record2 type"


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
fieldLabel = do { id <- lowerId
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

typedefTy:: Parser PadsTy
typedefTy= do     { reserved "constrain"
                  ; str1 <- manyTill anyChar (reservedOp "::")
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
-- [pads| type Entries = [Pint] with sep ',' and term eof         |]
-- [pads| type Entries = [Pint] with sep ',' and term length of exp  |]
-- [pads| type Entries = [Pint] with sep ',' |]    -- keep parsing until get an error in element type

sortModifier (Left sep) =   (Just sep, Nothing)
sortModifier (Right term) = (Nothing, Just term)
sortModifiers mods = 
    let (seps, terms) = unzip $ map sortModifier mods
    in  (msum seps, msum terms)

sep :: Parser PadsTy
sep = do { reserved "sep"
         ; ty <- padsTy
         ; return ty 
         } <?> "separator"
     
termKind :: Parser TermCond
termKind = do { reserved "length"
              ; reserved "of"
              ; lit <- lit
              ; return (LengthTC (litToExp lit))
              }
       <|> do { ty <- padsTy
              ; return (TyTC ty)
              } <?> "term kind"

term :: Parser TermCond
term = do { reserved "term"
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

tryTy :: Parser PadsTy
tryTy = do { reservedOp "Try"  
           ; ty <- padsTy
           ; return (Ptry ty)
           } <?> "try type"

reTy :: Parser PadsTy
reTy = do { reserved "re"
          ; s <- stringLiteral
          ; return (Papp (Pname "PstringME") (AppE (ConE (mkName "RE")) (LitE (TH.StringL s))))   
          } <?> "regular expression type"

reTy' :: Parser PadsTy
reTy' = do { reservedOp "/"
          ; s <- stringLiteral
          ; reservedOp "/"
          ; return (Papp (Pname "PstringME") (AppE (ConE (mkName "RE")) (LitE (TH.StringL s))))   
          } <?> "regular expression type"

padsTy :: Parser PadsTy
padsTy = lineTy
     <|> transformTy 
     <|> tupleTy
     <|> recordTy
     <|> maybeTy
     <|> listTy
     <|> tryTy
     <|> try fnAppTy
     <|> typedefTy
     <|> reTy
     <|> reTy'
     <|> litTy
     <|> idTy
     <?> "pads type"

{-

regexp foo = rb compiles to foo = RE (re rb) re :: our syntax ->
 String rep of regular expression

rb ::= <foo> | [^set] | [set] | char | rb+ | rb* |   rb1|rb2  | (rb) | rb1 rb2 | rb?  | .  
set ::= char | set set | char-char |  (escape sequences for ], -, and escape character \

setitem ::= char | char-char 
rawset ::= setitem rawset?  
set ::= ^rawset | rawset

ab*

rel3    :: rel3 + rel2 | rel2
rel2    :: rel2 rel1 | rel1
rel1    :: rel1+ | rel1* | rel1? | primre
primre :: = <foo> | set | char | (re) | .

-}

reP :: Parser RE
reP = buildExpressionParser table primRE

table = [  [ postfix "+" (.+.), postfix "*" (.*.), postfix "?" (.?.) ]
        ,  [ binary'    (.&.) AssocLeft ]
        ,  [ binary "|" (.|.) AssocLeft]
        ]
      where
         postfix  name fun       = Postfix (do{reservedOp name; return fun})
         binary   name fun assoc = Infix   (do{reservedOp name; return fun}) assoc
         binary'       fun assoc = Infix   (do{return fun}) assoc
          

parenRE :: Parser RE
parenRE = do 
    { re <- parens reP
    ; return (PRE.re_parens re)
    }

primRE :: Parser RE
primRE = setRE
--     <|> namedRE
     <|> parenRE
     <|> dotRE
     <|> charRE

dotRE :: Parser RE
dotRE = do
  { char '.'
  ; return (RE ".")
  }

charRE  :: Parser RE
charRE = do
  { c <- noneOf "()+?*[<."
  ; return (RE [c])
  }
    


namedRE :: Parser RE
namedRE = do
   { char '<'
   ; id <- identifier
   ; char '>'
   ; return (ReName id)
   }     

setRE :: Parser RE
setRE = do
   { char '['
   ; (isPos, set) <- set
   ; char ']'
   ; if isPos then return (charClass set)
              else return (opCharClass set)
   }

set :: Parser (Bool, [Char])
set = do 
     { char '^'
     ; rs <- rawset
     ; return (False, rs)
     }
 <|> do 
     { rs <- rawset
     ; return (True, rs)
     }

rawset :: Parser [Char]
rawset = do { sets <- many1 setitem
            ; return (concat sets)
            }


-- XXX this needs to be fixed to handle escaping characters correctly
setitem :: Parser [Char]
setitem = try (do
     { c1 <- alphaNum
     ; char '-'
     ; c2 <- alphaNum
     ; return [c1..c2]
     } )
  <|> do 
     { c <- noneOf "-]\\"
     ; return [c]
     } <?> "setitem"


parse :: PS.Parser a -> SourceName -> Line -> Column -> String -> Either ParseError a
parse p fileName line column input 
  = PP.parse (do {  setPosition (newPos fileName line column)
                  ; whiteSpace
                  ; x <- p
                  ; eof
                  ; return x
                  }) fileName input

