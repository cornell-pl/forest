{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable, TypeSynonymInstances #-}
module Language.Pads.CoreBaseTypes where

import Language.Pads.Generic
import Language.Pads.MetaData
import Language.Pads.PadsParser
import Language.Pads.RegExp

import qualified Language.Pads.Source as S
import qualified Language.Pads.Errors as E

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import qualified Data.Map as M
import Data.Data

import Text.PrettyPrint.Mainland as PP   

import Monad
import Char

{-  Base types 
    Pads type name, underlying representation type name, list of parameter types.
-}

baseTypesList = [
  ("Pint",      (''Int,     [])),
  ("Pchar",     (''Char,    [])),
  ("Pdigit",    (''Int,     [])),
  ("Ptext",     (''String,  [])),
  ("Pbinary",   (''S.RawStream,  [])),
  ("Pre",       (''String,  [''String])),
  ("Pstring",   (''String,  [''Char])),
  ("PstringFW", (''String,  [''Int])),
  ("PstringME", (''String,  [''RE])),
  ("PstringSE", (''String,  [''RE]))
 ]



baseTypesMap :: M.Map String (Name, [Name]) = M.fromList baseTypesList

{- XXX: These declarations should be generated from table above . -}
{- XXX: Default values should be defined to use parameter, so for example, we can generate a string of the proper length. -}
newtype Pint = Pint Int
  deriving (Eq, Show, Data, Typeable, Num, Ord, Integral, Real, Enum)
newtype Pchar = Pchar Char
  deriving (Eq, Show, Data, Typeable, Ord)
newtype Pdigit = Pdigit Int
  deriving (Eq, Show, Data, Typeable, Num, Ord, Integral, Real, Enum)
newtype Ptext    = Ptext String
  deriving (Eq, Show, Data, Typeable)
newtype Pbinary  = Pbinary S.RawStream
  deriving (Eq, Show, Data, Typeable)

type Pint_md = Base_md
type Pchar_md = Base_md
type Pdigit_md = Base_md
type Ptext_md = Base_md
type Pbinary_md = Base_md

instance Pretty Pchar where
  ppr (Pchar c) = text (show c)

instance Pretty Ptext where
  ppr (Ptext str) = string ("\"" ++ str++ "\"")
              

instance Pretty Pbinary where
  ppr (Pbinary str) = text "Binary"

instance Pads Pint Base_md where
  parsePP = pint_parseM

instance Pads Pchar Base_md where
  parsePP = pchar_parseM

instance Pads Pdigit Base_md where
  parsePP = pdigit_parseM

instance Pads Ptext Base_md where
  parsePP = ptext_parseM

instance Pads Pbinary Base_md where
  parsePP = pbinary_parseM

newtype Pre = Pre String
  deriving (Eq, Show, Data, Typeable)
newtype Pstring    = Pstring    String
  deriving (Eq, Show, Data, Typeable)
newtype PstringFW = PstringFW String
  deriving (Eq, Show, Data, Typeable)
newtype PstringME = PstringME String
  deriving (Eq, Show, Data, Typeable)
newtype PstringSE = PstringSE String
  deriving (Eq, Show, Data, Typeable)

type Pre_md     = Base_md
type Pstring_md = Base_md
type PstringFW_md = Base_md
type PstringME_md = Base_md
type PstringSE_md = Base_md

instance Pads1 String Pre Base_md where 
  parsePP1 = pre_parseM 

instance Pads1 Char Pstring Base_md where 
  parsePP1 = pstring_parseM 

instance Pads1 Int PstringFW Base_md where 
  parsePP1  = pstringFW_parseM 

instance Pads1 RE PstringME Base_md where
  parsePP1 = pstringME_parseM

instance Pads1 RE PstringSE Base_md where
  parsePP1 = pstringSE_parseM

class ToString a where
  toString :: a -> String

instance ToString Pre where
  toString (Pre s) = s

instance ToString Pstring where
  toString (Pstring s) = s

instance ToString PstringFW where
  toString (PstringFW s) = s
 
instance ToString PstringME where
  toString (PstringME s) = s

instance ToString PstringSE where
  toString (PstringSE s) = s


pstringFW_parseM :: Int -> PadsParser (PstringFW, Base_md)
pstringFW_parseM n = do 
  initPos <- getPos
  isEof <- isEofP 
  if isEof then badReturn (def1 n, mkErrBasePD (E.FoundWhenExpecting "EOF" "PstringFW") (Just initPos))
   else do 
     isEor <- isEorP
     if isEor && n /= 0 then badReturn (def1 n, mkErrBasePD (E.FoundWhenExpecting "EOR" "PstringFW") (Just initPos))
      else do  
          str <- takeP n 
          if (length str) /= n then badReturn (def1 n, mkErrBasePD (E.Insufficient (length str) n) (Just initPos))
            else goodReturn (PstringFW str, cleanBasePD)


pstringME_parseM :: RE -> PadsParser (PstringME, Base_md)
pstringME_parseM re = do 
  initPos <- getPos
  isEof <- isEofP 
  if isEof then badReturn (def1 re, mkErrBasePD (E.FoundWhenExpecting "EOF" "PstringME") (Just initPos))
   else do 
      match <- regexMatchP re
      case match of 
        Nothing  -> badReturn  (def1 re, mkErrBasePD (E.RegexMatchFail (show re)) (Just initPos))
        Just str -> goodReturn (PstringME str, cleanBasePD)

pre_parseM :: String -> PadsParser (Pre, Base_md)
pre_parseM sre = do 
  initPos <- getPos
  isEof <- isEofP 
  if isEof then badReturn (def1 sre, mkErrBasePD (E.FoundWhenExpecting "EOF" "Pre") (Just initPos))
   else do 
      match <- regexMatchP (RE sre)
      case match of 
        Nothing  -> badReturn  (def1 sre, mkErrBasePD (E.RegexMatchFail sre) (Just initPos))
        Just str -> goodReturn (Pre str, cleanBasePD)


pstringSE_parseM :: RE -> PadsParser (PstringSE, Base_md)
pstringSE_parseM re = do 
  initPos <- getPos
  isEof <- isEofP 
  if isEof then badReturn (def1 re, mkErrBasePD (E.FoundWhenExpecting "EOF" "PstringSE") (Just initPos))
   else do 
      match <- regexStopP re
      case match of 
        Nothing  -> badReturn  (def1 re, mkErrBasePD (E.RegexMatchFail (show re)) (Just initPos))
        Just str -> goodReturn (PstringSE str, cleanBasePD)

pstring_parseM :: Char -> PadsParser (Pstring, Base_md)
pstring_parseM c = do 
  initPos <- getPos
  isEof <- isEofP 
  if isEof then badReturn (def1 c, mkErrBasePD (E.FoundWhenExpecting "EOF" "Pstring") (Just initPos))
   else do 
     isEor <- isEorP
     if isEor then badReturn (def1 c, mkErrBasePD (E.FoundWhenExpecting "EOR" "Pstring") (Just initPos))
      else do  
          str <- satisfy (\c'-> c /= c')
          goodReturn (Pstring str, cleanBasePD)

ptext_parseM :: PadsParser (Ptext, Base_md)
ptext_parseM = do
  document <- getAllP
  goodReturn (Ptext document, cleanBasePD)

pbinary_parseM :: PadsParser (Pbinary, Base_md)
pbinary_parseM = do
  document <- getAllBinP
  goodReturn (Pbinary document, cleanBasePD)


pchar_parseM :: PadsParser (Pchar, Base_md)
pchar_parseM  = do 
  initPos <- getPos
  isEof <- isEofP 
  if isEof then badReturn (def, mkErrBasePD (E.FoundWhenExpecting "EOF" "Pchar") (Just initPos))
   else do 
     isEor <- isEorP
     if isEor then badReturn (def, mkErrBasePD (E.FoundWhenExpecting "EOR" "Pchar") (Just initPos))
      else do  
          c <- takeHeadP
          goodReturn (Pchar c, cleanBasePD)

pdigit_parseM :: PadsParser (Pdigit, Base_md)
pdigit_parseM  = do 
  initPos <- getPos
  isEof <- isEofP 
  if isEof then badReturn (def, mkErrBasePD (E.FoundWhenExpecting "EOF" "Pdigit") (Just initPos))
   else do 
     isEor <- isEorP
     if isEor then badReturn (def, mkErrBasePD (E.FoundWhenExpecting "EOR" "Pdigit") (Just initPos))
      else do  
          c <- takeHeadP
          if isDigit c then goodReturn (Pdigit (digitToInt c), cleanBasePD)
                       else badReturn  (def, mkErrBasePD (E.FoundWhenExpecting [c] "Pdigit") (Just initPos))


pint_parseM :: PadsParser (Pint,Base_md)
pint_parseM = do
  initPos <- getPos
  isEof <- isEofP 
  if isEof then badReturn (def, mkErrBasePD (E.FoundWhenExpecting "EOF" "Pint") (Just initPos))
   else do 
     isEor <- isEorP
     if isEor then badReturn (def, mkErrBasePD (E.FoundWhenExpecting "EOR" "Pint") (Just initPos))
      else do  
          c <- peakHeadP 
          let isNeg = c == '-'
          when isNeg (takeHeadP >> return ())
          digits <- satisfy Char.isDigit
          if null digits then badReturn (def, mkErrBasePD (E.FoundWhenExpecting (mkStr c) "Pint") (Just initPos))
            else goodReturn (Pint $ digitListToInt isNeg digits, cleanBasePD)

class LitParse a where
  litParse :: a -> PadsParser ((), Base_md)

instance LitParse Char where
  litParse = pcharLit_parseM

instance LitParse String where
  litParse = pstrLit_parseM

instance LitParse RE where
  litParse = preLit_parseM


preLit_parseM :: RE -> PadsParser ((), Base_md)
preLit_parseM re = do { (match, md) <- pstringME_parseM re
                      ; if numErrors md == 0 then goodReturn ((), md) else badReturn ((), md)
                      }


pcharLit_parseM :: Char -> PadsParser ((), Base_md)
pcharLit_parseM c = do 
  let cStr = mkStr c
  initPos <- getPos
  isEof <- isEofP
  if isEof then badReturn ((), mkErrBasePD (E.FoundWhenExpecting "EOF" cStr) (Just initPos))
   else do 
     isEor <- isEorP
     if isEor then badReturn ((), mkErrBasePD (E.FoundWhenExpecting "EOR" cStr) (Just initPos))
      else do
         c' <- takeHeadP 
         if c == c' then goodReturn ((),cleanBasePD)
          else do
           foundIt <- scanP c
           errPos <- getPos
           if foundIt then badReturn ((), mkErrBasePD (E.ExtraBeforeLiteral cStr) (Just errPos))
                      else badReturn ((), mkErrBasePD (E.MissingLiteral     cStr) (Just errPos))


pstrLit_parseM :: String -> PadsParser ((), Base_md)
pstrLit_parseM s = do 
  initPos <- getPos
  isEof <- isEofP
  if isEof then badReturn ((), mkErrBasePD (E.FoundWhenExpecting "EOF" s) (Just initPos))
   else do 
     isEor <- isEorP
     if isEor then badReturn ((), mkErrBasePD (E.FoundWhenExpecting "EOR" s) (Just initPos))
      else do
         match <- scanStrP s
         errPos <- getPos
         case match of
           Nothing   -> badReturn  ((), mkErrBasePD (E.MissingLiteral     s) (Just errPos))
           Just []   -> goodReturn ((), cleanBasePD)
           Just junk -> badReturn  ((), mkErrBasePD (E.ExtraBeforeLiteral s) (Just errPos))

peorLit_parseM :: PadsParser ((), Base_md)
peorLit_parseM = do 
  initPos <- getPos
  isEof <- isEofP
  if isEof then badReturn ((), mkErrBasePD (E.FoundWhenExpecting "EOF" "EOR") (Just initPos))
   else do 
     isEor <- isEorP
     if isEor then doLineEnd
              else badReturn ((), mkErrBasePD (E.LineError "Expecting EOR") (Just initPos))

peofLit_parseM :: PadsParser ((), Base_md)
peofLit_parseM = do
  initPos <- getPos
  isEof <- isEofP
  if isEof then goodReturn ((), cleanBasePD)
           else badReturn  ((), (mkErrBasePD ( E.ExtraBeforeLiteral "Eof")(Just initPos)))

pvoidLit_parseM :: PadsParser ((), Base_md)
pvoidLit_parseM = goodReturn ((), cleanBasePD)


{- Helper functions -}
mkStr c = "'" ++ [c] ++ "'"

