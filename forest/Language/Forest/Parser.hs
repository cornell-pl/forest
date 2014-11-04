{-# LANGUAGE TemplateHaskell #-}

{-
** *********************************************************************
*                                                                      *
*              This software is part of the pads package               *
*           Copyright (c) 2005-2011 AT&T Knowledge Ventures            *
*                      and is licensed under the                       *
*                        Common Public License                         *
*                      by AT&T Knowledge Ventures                      *
*                                                                      *
*                A copy of the License is available at                 *
*                    www.padsproj.org/License.html                     *
*                                                                      *
*  This program contains certain software code or other information    *
*  ("AT&T Software") proprietary to AT&T Corp. ("AT&T").  The AT&T     *
*  Software is provided to you "AS IS". YOU ASSUME TOTAL RESPONSIBILITY*
*  AND RISK FOR USE OF THE AT&T SOFTWARE. AT&T DOES NOT MAKE, AND      *
*  EXPRESSLY DISCLAIMS, ANY EXPRESS OR IMPLIED WARRANTIES OF ANY KIND  *
*  WHATSOEVER, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF*
*  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, WARRANTIES OF  *
*  TITLE OR NON-INFRINGEMENT.  (c) AT&T Corp.  All rights              *
*  reserved.  AT&T is a registered trademark of AT&T Corp.             *
*                                                                      *
*                   Network Services Research Center                   *
*                          AT&T Labs Research                          *
*                           Florham Park NJ                            *
*                                                                      *
*              Kathleen Fisher <kfisher@research.att.com>              *
*                                                                      *
************************************************************************
-}

module Language.Forest.Parser where


import Language.Forest.Syntax as S

import Control.Monad (msum,liftM)
import Control.Monad.State (StateT(..))
import qualified Control.Monad.State as State
import Text.Parsec
import qualified Text.Parsec.String as PS
import Text.Parsec.Error
import qualified Text.Parsec.Prim as PP
import qualified Text.Parsec.Token as PT
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Language 
import Text.ParserCombinators.Parsec.Pos
import qualified Language.Haskell.Meta as LHM
import Language.Haskell.TH as TH hiding (CharL)
import Data.Char
import Data.Maybe

import qualified Language.Pads.Parser as PadsP
import Language.Pads.Syntax

type Parser = PS.Parser

changeParsecState :: (Functor m,Monad m) => (u -> v) -> (v -> u) -> ParsecT s u m a -> ParsecT s v m a
changeParsecState forward backward = mkPT . transform . runParsecT where
	mapState f st = st { PP.stateUser = f (PP.stateUser st) }
	mapReply f (PP.Ok a st err) = PP.Ok a (mapState f st) err
	mapReply _ (PP.Error e) = PP.Error e
	fmap3 = fmap . fmap . fmap
	transform p st = fmap3 (mapReply forward) (p (mapState backward st))

lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser (haskellStyle { reservedOpNames = ["=", "(:", ":)", "<=>", "{", "}", "::", "<|", "|>", "|", "->", "[:", ":]", "<-", ","],
                                           reservedNames   = ["is", "File", "Directory", "type", "matches", "Maybe", "as", "constrain", "where"]})

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
                ; (id,pats) <- params
                ; rawty <- forestTy
                ; predM <- optionMaybe fieldPredicate
                ; let ty = integratePred rawty predM 
                ; return (ForestDecl(id, pats, replaceName id ty))
                } <?> "Forest Declaration"

integratePred :: ForestTy -> Maybe TH.Exp -> ForestTy
integratePred ty predM = case predM of
  Nothing -> ty
  Just predE -> FConstraint (TH.VarP (mkName "this")) ty predE

-- a regular Haskell expression in parenthesis to which we add a return
haskellParenthesisExp :: Parser TH.Exp
haskellParenthesisExp = (liftM (AppE (VarE 'return)) $ do
	str <- parseParentherizedExp
	case LHM.parseExp str of
		Left err    -> unexpected ("Failed to parse Haskell expression: " ++ err)
		Right expTH -> return expTH
	) <?> "haskell expression"

parseParentherizedExp :: Parser String
parseParentherizedExp = changeParsecState (const ()) (const (0::Int)) $ do
	spaces
	char '('
	let go = (char '(' >>= \c -> PP.modifyState succ >> return c) <|> (char ')' >>= \c -> PP.modifyState pred >> return c) <|> anyChar
	let stop = do
		count <- PP.getState
		if count == (0::Int) then char ')' else parserZero
	str <- manyTill go stop
	spaces
	return str

-- a more general monadic Haskell expression
haskellMonadicExp :: Parser TH.Exp
haskellMonadicExp = do 
   { reservedOp "<|"
   ; str <- manyTill anyChar (reservedOp "|>") 
   ; case LHM.parseExp str of
                 Left err    -> unexpected ("Failed to parse Haskell expression: " ++ err)
                 Right expTH -> return expTH
   } <?> "haskell expression"

--parserExp :: Parser TH.Exp
--parserExp = do
--	str <- getInput
--	case LHM.parseExp str of
--		Left err -> parserZero
--		Right expTH -> do
--			setInput ""
--			parserReturn expTH

-- a non-parentherized literal Haskell expression to which we add a return
literalExp :: Parser TH.Exp
literalExp = liftM (AppE (VarE 'return)) PadsP.literal

haskellExp :: Parser TH.Exp
haskellExp = haskellParenthesisExp <|> haskellMonadicExp <|> literalExp

forestTy :: Parser ForestTy
forestTy =   directoryTy
         <|> fileTy
         <|> maybeTy
         <|> symLinkTy
         <|> archiveTy
         <|> constrainTy
         <|> try fnAppTy
         <|> try compTy
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

forestArgs :: Parser [TH.Exp]
forestArgs = many1 forestArg <?> "forest arguments"

fnAppTy :: Parser ForestTy
fnAppTy = do { ty <- fnTy
             ; exps <- forestArgs
             ; return (Fapp ty exps)
             } <?> "type function application"

maybeTy :: Parser ForestTy
maybeTy = do { reserved "Maybe"
             ; ty <- forestTy
             ; return (FMaybe ty)
             } <?> "Forest Maybe type"

archiveTy :: Parser ForestTy
archiveTy = gzipTy <|> tarTy <|> zipTy <|> bzipTy <|> rarTy <?> "Forest Archive Type"

-- for example for an archive.tar.gz file, the AVFS path archive.tar.gz# accesses directly the content of the tar
gzipTy :: Parser ForestTy
gzipTy = (do
	reserved "Gzip"
	ty <- forestTy
	case ty of
		Archive archtype descTy -> return $ Archive (archtype++[Gzip]) descTy
		otherwise -> return (Archive [Gzip] ty)) <?> "Forest Gzip type"

tarTy :: Parser ForestTy
tarTy = do { reserved "Tar"
             ; ty <- forestTy
             ; return (Archive [Tar] ty)
             } <?> "Forest Tar type"

zipTy :: Parser ForestTy
zipTy = do { reserved "Zip"
             ; ty <- forestTy
             ; return (Archive [Zip] ty)
             } <?> "Forest Zip type"

bzipTy :: Parser ForestTy
bzipTy = (do
	reserved "Bzip"
	ty <- forestTy
	case ty of
		Archive archtype descTy -> return $ Archive (archtype++[Bzip]) descTy
		otherwise -> return (Archive [Bzip] ty)) <?> "Forest Bzip type"

rarTy :: Parser ForestTy
rarTy = do { reserved "Rar"
             ; ty <- forestTy
             ; return (Archive [Rar] ty)
             } <?> "Forest Rar type"

constrainTy :: Parser ForestTy
constrainTy = do
  { reserved "constrain"
  ; strPat <- manyTill anyChar (reservedOp "::")
  ; pat <- case LHM.parsePat strPat of
               (Left err) -> unexpected ("Failed to parse Haskell pattern in where declaration: " ++ err)
               (Right patTH) -> return patTH
  ; ty <- forestTy
  ; predE <- fieldPredicate
  ; return (FConstraint pat ty predE)
  } <?> "Forest Constraint type"

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
forestArgR = haskellExp

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
           { (isForm, external_exp) <- externalName internal_name
           ; forest_ty <- forestTy
           ; predM <- optionMaybe fieldPredicate
           ; return (Simple (internal_name, isForm, external_exp, forest_ty, predM))
           } <?> "Simple Forest Field"



compBody :: Parser(Generator, Maybe TH.Exp)
compBody =  do
     { isMatch <- optionMaybe (reserved "matches")
     ; generatorE <- forestArg
     ; predE <- optionMaybe compPredicate
     ; if isJust isMatch 
          then return (Matches  generatorE, predE)
          else return (Explicit generatorE, predE)
     }

compTy :: Parser ForestTy
compTy = do
  { cf <- compForm "this"
  ; return (FComp cf)
  }

compForm :: String -> Parser CompField
compForm internal_name = do
          { repTyConName <- optionMaybe (identifier)
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
          ; (generatorE, predEOpt) <- compBody
          ; reservedOp "]"
          ; return (CompField internal_name repTyConName explicitFileName externalE forest_ty generatorP generatorE predEOpt)
          }

compField :: String -> Parser Field
compField internal_name = do
          { reserved "is" 
          ; cfield <- compForm internal_name
          ; return (Comp cfield)
          }

asPattern :: Parser String
asPattern = try (do
 { str <- identifier
 ; reserved "as"
 ; return str
 }
 )

compPredicate :: Parser TH.Exp
compPredicate = do { reservedOp   ","
                   ; haskellExp
                   }

fieldPredicate :: Parser TH.Exp
fieldPredicate = do { reserved   "where"
                    ; haskellExp
                    }
externalName :: String -> Parser (Bool, TH.Exp)
externalName internal = 
       (explicitExternalName internal)
   <|> (simpleMatches        internal)
   <|> (implicitExternalName internal)

simpleMatches :: String -> Parser (Bool, TH.Exp)
simpleMatches internal = do
  { reserved "matches"
  ; expE <- pathSpec
  ; reservedOp "::"                 
  ; return (False, expE)
  }

explicitExternalName :: String -> Parser (Bool, TH.Exp)
explicitExternalName internal = do 
    { reserved "is"
    ; nameE <- pathSpec
    ; reservedOp "::"                 
    ; return (True, nameE)
    }      
           
implicitExternalName :: String -> Parser (Bool, TH.Exp)
implicitExternalName internal = 
    if isLowerCase internal then do 
        { reservedOp "::"
        ; return (True, TH.LitE (TH.StringL internal))}     
    else unexpected ("Directory label "++ internal ++" is not a valid Haskell record label.  Use an \"is\" clause to give an explicit external name.")     
    
pathSpec :: Parser TH.Exp
pathSpec = forestArg

isLowerCase [] = False
isLowerCase (x:xs) = isLower x

params :: Parser (String,[TH.Pat])
params = do { str <- manyTill anyChar (reservedOp "=")
           ; idpat <- case LHM.parsePat str of 
                              Left err    -> unexpected ("Failed to parse Haskell pattern: " ++ err)
                              Right (ConP id pats) -> return (nameBase id,pats)
           ; return idpat                            
           } <?> "Forest parameter"

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
  Directory (Record _ body) -> Directory (Record str body)
  FConstraint pat (Directory (Record _ body)) pred -> FConstraint pat (Directory (Record str body)) pred
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
