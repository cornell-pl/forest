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
  deriving (Eq, Show, Data, Typeable, Ord)
newtype Pbinary  = Pbinary S.RawStream
  deriving (Eq, Show, Data, Typeable, Ord)

type Pint_md = Base_md
type Pchar_md = Base_md
type Pdigit_md = Base_md
type Ptext_md = Base_md
type Pbinary_md = Base_md

instance Pretty Pchar where
  ppr (Pchar c) = text (show c)

instance Pretty Ptext where
  ppr (Ptext str) = string ("\"" ++ str++ "\"")

instance Pretty Pre where
  ppr (Pre s) = string s
              

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
  deriving (Eq, Data, Typeable, Ord)
newtype Pstring    = Pstring    String
  deriving (Eq, Show, Data, Typeable, Ord)
newtype PstringFW = PstringFW String
  deriving (Eq, Show, Data, Typeable, Ord)
newtype PstringME = PstringME String
  deriving (Eq, Show, Data, Typeable, Ord)
newtype PstringSE = PstringSE String
  deriving (Eq, Show, Data, Typeable, Ord)

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


handleEOF val str loc p 
  = do { isEof <- isEOFP 
       ; if isEof then
           badReturn (val, mkErrBasePDfromLoc (E.FoundWhenExpecting "EOF" str) loc )
         else p}


pstringFW_parseM :: Int -> PadsParser (PstringFW, Base_md)
pstringFW_parseM n = do 
  initLoc <- getLoc
  handleEOF (def1 n) "PstringFW" initLoc $ do
     isEor <- isEORP
     if isEor && n /= 0 then badReturn (def1 n, mkErrBasePDfromLoc (E.FoundWhenExpecting "EOR" "PstringFW") initLoc)
      else do  
          str <- takeP n 
          if (length str) /= n then badReturn (def1 n, mkErrBasePDfromLoc (E.Insufficient (length str) n) initLoc)
            else return (PstringFW str, cleanBasePD)


pstringME_parseM :: RE -> PadsParser (PstringME, Base_md)
pstringME_parseM re = do 
  initLoc <- getLoc
  handleEOF (def1 re) "PstringME" initLoc $ do
      match <- regexMatchP re
      case match of 
        Nothing  -> badReturn  (def1 re, mkErrBasePDfromLoc (E.RegexMatchFail (show re)) initLoc)
        Just str -> return (PstringME str, cleanBasePD)

pre_parseM :: String -> PadsParser (Pre, Base_md)
pre_parseM sre = do 
  initLoc <- getLoc
  handleEOF (def1 sre) "Pre" initLoc $ do
      match <- regexMatchP (RE sre)
      case match of 
        Nothing  -> badReturn  (def1 sre, mkErrBasePDfromLoc (E.RegexMatchFail sre) initLoc)
        Just str -> return (Pre str, cleanBasePD)


pstringSE_parseM :: RE -> PadsParser (PstringSE, Base_md)
pstringSE_parseM re = do 
  initLoc <- getLoc
  handleEOF (def1 re) "PstringSE" initLoc $ do
      match <- regexStopP re
      case match of 
        Nothing  -> badReturn  (def1 re, mkErrBasePDfromLoc (E.RegexMatchFail (show re)) initLoc)
        Just str -> return (PstringSE str, cleanBasePD)

pstring_parseM :: Char -> PadsParser (Pstring, Base_md)
pstring_parseM c = do 
  initLoc <- getLoc
  handleEOF (def1 c) "Pstring" initLoc $ do 
     isEor <- isEORP
     if isEor then badReturn (def1 c, mkErrBasePDfromLoc (E.FoundWhenExpecting "EOR" "Pstring") ( initLoc))
      else do  
          str <- satisfy (\c'-> c /= c')
          return (Pstring str, cleanBasePD)

ptext_parseM :: PadsParser (Ptext, Base_md)
ptext_parseM = do
  document <- getAllP
  return (Ptext document, cleanBasePD)

pbinary_parseM :: PadsParser (Pbinary, Base_md)
pbinary_parseM = do
  document <- getAllBinP
  return (Pbinary document, cleanBasePD)


pchar_parseM :: PadsParser (Pchar, Base_md)
pchar_parseM  = do 
  initLoc <- getLoc
  handleEOF def "Pchar" initLoc $ do
     isEor <- isEORP
     if isEor then badReturn (def, mkErrBasePDfromLoc (E.FoundWhenExpecting "EOR" "Pchar") ( initLoc))
      else do  
          c <- takeHeadP
          return (Pchar c, cleanBasePD)

pdigit_parseM :: PadsParser (Pdigit, Base_md)
pdigit_parseM  = do 
  initLoc <- getLoc
  handleEOF def "Pdigit" initLoc $ do
     isEor <- isEORP
     if isEor then badReturn (def, mkErrBasePDfromLoc (E.FoundWhenExpecting "EOR" "Pdigit") ( initLoc))
      else do  
          c <- takeHeadP
          if isDigit c then return (Pdigit (digitToInt c), cleanBasePD)
                       else badReturn  (def, mkErrBasePDfromLoc (E.FoundWhenExpecting [c] "Pdigit") ( initLoc))


pint_parseM :: PadsParser (Pint,Base_md)
pint_parseM = do
  initLoc <- getLoc
  isEof <- isEOFP 
  if isEof then badReturn (def, mkErrBasePDfromLoc (E.FoundWhenExpecting "EOF" "Pint") ( initLoc))
   else do 
     isEor <- isEORP
     if isEor then badReturn (def, mkErrBasePDfromLoc (E.FoundWhenExpecting "EOR" "Pint") ( initLoc))
      else do  
          c <- peakHeadP 
          let isNeg = c == '-'
          when isNeg (takeHeadP >> return ())
          digits <- satisfy Char.isDigit
          if null digits then badReturn (def, mkErrBasePDfromLoc (E.FoundWhenExpecting (mkStr c) "Pint") ( initLoc))
            else return (Pint $ digitListToInt isNeg digits, cleanBasePD)

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
                      ; if numErrors md == 0 then return ((), md) else badReturn ((), md)
                      }


pcharLit_parseM :: Char -> PadsParser ((), Base_md)
pcharLit_parseM c = do 
  let cStr = mkStr c
  initLoc <- getLoc
  isEof <- isEOFP
  if isEof then badReturn ((), mkErrBasePDfromLoc (E.FoundWhenExpecting "EOF" cStr) ( initLoc))
   else do 
     isEor <- isEORP
     if isEor then badReturn ((), mkErrBasePDfromLoc (E.FoundWhenExpecting "EOR" cStr) ( initLoc))
      else do
         c' <- takeHeadP 
         if c == c' then return ((),cleanBasePD)
          else do
           foundIt <- scanP c
           errLoc <- getLoc
           if foundIt then badReturn ((), mkErrBasePDfromLoc (E.ExtraBeforeLiteral cStr) ( errLoc))
                      else badReturn ((), mkErrBasePDfromLoc (E.MissingLiteral     cStr) ( errLoc))


pstrLit_parseM :: String -> PadsParser ((), Base_md)
pstrLit_parseM s = do 
  initLoc <- getLoc
  isEof <- isEOFP
  if isEof then badReturn ((), mkErrBasePDfromLoc (E.FoundWhenExpecting "EOF" s) ( initLoc))
   else do 
     isEor <- isEORP
     if isEor then badReturn ((), mkErrBasePDfromLoc (E.FoundWhenExpecting "EOR" s) ( initLoc))
      else do
         match <- scanStrP s
         errLoc <- getLoc
         case match of
           Nothing   -> badReturn  ((), mkErrBasePDfromLoc (E.MissingLiteral     s) ( errLoc))
           Just []   -> return ((), cleanBasePD)
           Just junk -> badReturn  ((), mkErrBasePDfromLoc (E.ExtraBeforeLiteral s) ( errLoc))

peorLit_parseM :: PadsParser ((), Base_md)
peorLit_parseM = do 
  initLoc <- getLoc
  isEof <- isEOFP
  if isEof then badReturn ((), mkErrBasePDfromLoc (E.FoundWhenExpecting "EOF" "EOR") ( initLoc))
   else do 
     isEor <- isEORP
     if isEor then doLineEnd
              else badReturn ((), mkErrBasePDfromLoc (E.LineError "Expecting EOR") ( initLoc))

peofLit_parseM :: PadsParser ((), Base_md)
peofLit_parseM = do
  initLoc <- getLoc
  isEof <- isEOFP
  if isEof then return ((), cleanBasePD)
           else badReturn  ((), (mkErrBasePDfromLoc ( E.ExtraBeforeLiteral "Eof")( initLoc)))

pvoidLit_parseM :: PadsParser ((), Base_md)
pvoidLit_parseM = return ((), cleanBasePD)


{- Helper functions -}
mkStr c = "'" ++ [c] ++ "'"

