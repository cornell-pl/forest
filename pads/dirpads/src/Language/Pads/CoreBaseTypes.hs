{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable, TypeSynonymInstances #-}
module Language.Pads.CoreBaseTypes where

import Language.Pads.Generic
import Language.Pads.MetaData
import Language.Pads.PadsParser
import Language.Pads.RegExp

import qualified Language.Pads.Source as S
import qualified Language.Pads.Errors as E
import qualified Data.ByteString.Lazy.Char8 as B   -- abstraction for output data

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import qualified Data.Map as M
import Data.Data
import qualified Data.List as List

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
  printAcc = pint_printAcc

instance Pads Pchar Base_md where
  parsePP = pchar_parseM
  printAcc = pchar_printAcc

instance Pads Pdigit Base_md where
  parsePP = pdigit_parseM
  printAcc = pdigit_printAcc

instance Pads Ptext Base_md where
  parsePP = ptext_parseM
  printAcc = ptext_printAcc

instance Pads Pbinary Base_md where
  parsePP = pbinary_parseM
  printAcc = pbinary_printAcc

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
  printAcc1 = pre_printAcc

instance Pads1 Char Pstring Base_md where 
  parsePP1 = pstring_parseM 
  printAcc1 = pstring_printAcc

instance Pads1 Int PstringFW Base_md where 
  parsePP1 = pstringFW_parseM 
  printAcc1 = pstringFW_printAcc

instance Pads1 RE PstringME Base_md where
  parsePP1 = pstringME_parseM
  printAcc1 = pstringME_printAcc

instance Pads1 RE PstringSE Base_md where
  parsePP1 = pstringSE_parseM
  printAcc1 = pstringSE_printAcc

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


----------------------------------

handleEOF val str p 
  = do { isEof <- isEOFP 
       ; if isEof then
           returnError val (E.FoundWhenExpecting "EOF" str) 
         else p}

handleEOR val str p 
  = do { isEor <- isEORP 
       ; if isEor then
           returnError val (E.FoundWhenExpecting "EOR" str)
         else p}

----------------------------------



pstringFW_parseM :: Int -> PadsParser (PstringFW, Base_md)
pstringFW_parseM 0 = returnClean (PstringFW "")
pstringFW_parseM n =
  handleEOF (def1 n) "PstringFW" $
  handleEOR (def1 n) "PstringFW" $ do
    str <- takeP n 
    if (length str) == n 
      then returnClean (PstringFW str)
      else returnError (def1 n) (E.Insufficient (length str) n)

pstringME_parseM :: RE -> PadsParser (PstringME, Base_md)
pstringME_parseM re = 
  handleEOF (def1 re) "PstringME" $ do
    match <- regexMatchP re
    case match of 
      Just str -> returnClean (PstringME str)
      Nothing  -> returnError (def1 re) (E.RegexMatchFail (show re))

pre_parseM :: String -> PadsParser (Pre, Base_md)
pre_parseM sre =
  handleEOF (def1 sre) "Pre" $ do
    match <- regexMatchP (RE sre)
    case match of 
      Just str -> returnClean (Pre str)
      Nothing  -> returnError (def1 sre) (E.RegexMatchFail sre)


pstringSE_parseM :: RE -> PadsParser (PstringSE, Base_md)
pstringSE_parseM re =
  handleEOF (def1 re) "PstringSE" $ do
    match <- regexStopP re
    case match of 
      Just str -> returnClean (PstringSE str)
      Nothing  -> returnError (def1 re) (E.RegexMatchFail (show re))

pstring_parseM :: Char -> PadsParser (Pstring, Base_md)
pstring_parseM c =
  handleEOF (def1 c) "Pstring" $
  handleEOR (def1 c) "Pstring" $ do
    str <- satisfy (\c'-> c /= c')
    returnClean (Pstring str)

ptext_parseM :: PadsParser (Ptext, Base_md)
ptext_parseM = do
  document <- getAllP
  returnClean (Ptext document)

pbinary_parseM :: PadsParser (Pbinary, Base_md)
pbinary_parseM = do
  document <- getAllBinP
  returnClean (Pbinary document)


pchar_parseM :: PadsParser (Pchar, Base_md)
pchar_parseM  =
  handleEOF def "Pchar" $
  handleEOR def "Pchar" $ do
    c <- takeHeadP
    returnClean (Pchar c)

pdigit_parseM :: PadsParser (Pdigit, Base_md)
pdigit_parseM  =
  handleEOF def "Pdigit" $
  handleEOR def "Pdigit" $ do
    c <- takeHeadP
    if isDigit c 
      then returnClean (Pdigit (digitToInt c))
      else returnError def (E.FoundWhenExpecting [c] "Pdigit")


pint_parseM :: PadsParser (Pint,Base_md)
pint_parseM =
  handleEOF def "Pint" $
  handleEOR def "Pint" $ do
    c <- peekHeadP 
    let isNeg = (c == '-')
    when isNeg (takeHeadP >> return ())
    digits <- satisfy Char.isDigit
    if not (null digits)
      then returnClean (Pint $ digitListToInt isNeg digits)
      else returnError def (E.FoundWhenExpecting (mkStr c) "Pint")

class LitParse a where
  litParse :: a -> PadsParser ((), Base_md)

instance LitParse Char where
  litParse = pcharLit_parseM

instance LitParse String where
  litParse = pstrLit_parseM

instance LitParse RE where
  litParse = preLit_parseM


preLit_parseM :: RE -> PadsParser ((), Base_md)
preLit_parseM re = do
  (match, md) <- pstringME_parseM re
  if numErrors md == 0 
    then return ((), md) 
    else badReturn ((), md)


pcharLit_parseM :: Char -> PadsParser ((), Base_md)
pcharLit_parseM c =
  handleEOF () (mkStr c) $
  handleEOR () (mkStr c) $ do
    c' <- takeHeadP 
    if c == c' then returnClean () else do
      foundIt <- scanP c
      returnError () (if foundIt 
                      then E.ExtraBeforeLiteral (mkStr c)
                      else E.MissingLiteral     (mkStr c)) 


pstrLit_parseM :: String -> PadsParser ((), Base_md)
pstrLit_parseM s =
  handleEOF () s $ 
  handleEOR () s $ do
    match <- scanStrP s
    case match of
      Just []   -> returnClean ()
      Just junk -> returnError () (E.ExtraBeforeLiteral s)
      Nothing   -> returnError () (E.MissingLiteral     s)

peorLit_parseM :: PadsParser ((), Base_md)
peorLit_parseM =
  handleEOF () "EOR" $ do
   isEor <- isEORP
   if isEor then doLineEnd
     else returnError () (E.LineError "Expecting EOR")

peofLit_parseM :: PadsParser ((), Base_md)
peofLit_parseM = do
  isEof <- isEOFP
  if isEof then returnClean ()
           else returnError () (E.ExtraBeforeLiteral "Eof")

pvoidLit_parseM :: PadsParser ((), Base_md)
pvoidLit_parseM = returnClean ()

{- Printing functions -}

pstringFW_printAcc :: Int -> (PstringFW, Base_md) -> B.ByteString -> B.ByteString
pstringFW_printAcc n (PstringFW str, bmd) orig = B.append orig (B.pack str)             -- Should we check that str has length n?

pstringME_printAcc :: RE -> (PstringME, Base_md) -> B.ByteString -> B.ByteString
pstringME_printAcc re (PstringME str, bmd) orig = B.append orig (B.pack str)        -- We're not likely to check that str matches re

pre_printAcc :: String -> (Pre, Base_md) -> B.ByteString -> B.ByteString
pre_printAcc s (Pre str, bmd) orig = B.append orig (B.pack str)

pstringSE_printAcc :: RE -> (PstringSE, Base_md) -> B.ByteString -> B.ByteString
pstringSE_printAcc s (PstringSE str, bmd) orig = B.append orig (B.pack str)

pstring_printAcc :: Char -> (Pstring, Base_md) -> B.ByteString -> B.ByteString
pstring_printAcc c (Pstring str, bmd) orig = B.append orig (B.pack str)

ptext_printAcc :: (Ptext, Base_md) -> B.ByteString -> B.ByteString
ptext_printAcc (Ptext str, bmd) orig = B.append orig (B.pack str)

pbinary_printAcc :: (Pbinary, Base_md) -> B.ByteString -> B.ByteString
pbinary_printAcc (Pbinary bstr, bmd) orig = B.append orig (bstr)

pchar_printAcc :: (Pchar, Base_md) -> B.ByteString -> B.ByteString
pchar_printAcc (Pchar c,bmd) orig = B.append orig (B.pack [c])

pdigit_printAcc :: (Pdigit, Base_md) -> B.ByteString -> B.ByteString
pdigit_printAcc (Pdigit i, bmd) orig = B.append orig (B.pack [intToDigit i])

pint_printAcc :: (Pint, Base_md) -> B.ByteString -> B.ByteString
pint_printAcc (Pint i, bmd) orig = B.append orig (B.pack (show i))

preLit_printAcc :: RE -> ((), Base_md) -> B.ByteString -> B.ByteString
preLit_printAcc re ((),bmd) orig = B.append orig (B.pack "--REGEXP LITERAL-- ")

pcharLit_printAcc :: Char -> ((),Base_md) -> B.ByteString -> B.ByteString
pcharLit_printAcc c _ orig = B.append orig (B.pack [c])

pstrLit_printAcc :: String -> ((),Base_md) -> B.ByteString -> B.ByteString
pstrLit_printAcc str _ orig = B.append orig (B.pack str)

peorLit_printAcc :: ((),Base_md) -> B.ByteString -> B.ByteString
peorLit_printAcc r orig = B.append orig (S.printEOR)

peofLit_printAcc :: ((),Base_md) -> B.ByteString -> B.ByteString
peofLit_printAcc r orig  = B.append orig (S.printEOF)

pvoidLit_printAcc :: ((),Base_md) -> B.ByteString -> B.ByteString
pvoidLit_printAcc r orig = B.append orig (B.pack [])


-- ty_print_acc :: (pads,md) -> B.ByteString -> B.ByteString
-- ty_print_acc ((a,b,c),(mda,mdb,mdc)) acc = print_acc (c, mdc) (print_acc (b, mdb) (print_acc (a, mba)))

{- 
  first list is head of queue in correct order
  second list is tail of queue, in reverse order, so front of list is last entry in queue.
  current is the record in the process of being added to the queue.
  Example: the queue [abc, def, ghi] could be represented:
    PQueue []               [def, abc]      [i,h,g]
    PQueue []               [ghi, def, abc] []
    PQueue [abc]            [ghi, def]      []
    PQueue [abc, def]       [ghi]           []
    PQueue [abc, def,ghi]   []              []

-}
-- Assume concat :: [t] -> s
{-
data PQueue s t = PQueue [s] [s] [t]


emptyPQ :: ConcatTo s t => PQueue t s
emptyPQ = PQueue [] [] []

eorPQ :: ConcatTo s t => PQueue t s -> PQueue t s
eorPQ (PQueue front kcab tnerruc) = PQueue front ((concatTo(List.reverse tnerruc)):kcab) []

insertPQ :: ConcatTo s t => s -> PQueue t s -> PQueue t s
insertPQ new (PQueue front kcab tnerruc) = PQueue front kcab (new : tnerruc)

shiftPQ :: ConcatTo s t => PQueue t s -> PQueue t s
shiftPQ (PQueue front kcab tnerruc) = PQueue (front ++ (List.reverse kcab)) [] tnerruc

nullPQ :: PQueue t s -> Bool
nullPQ (PQueue front kcab tnerruc) = List.null front && List.null kcab && List.null tnerruc

removePQ :: ConcatTo s t => PQueue t s -> (t, PQueue t s)
removePQ q @ (PQueue front kcab tnerruc)  = 
  case front of 
    (f:rest) -> (f, PQueue rest kcab tnerruc)                  -- return head of queue
    [] -> let (PQueue front' [] tnerruc') = shiftPQ q          -- if head is empty, normalize queue
          in case front' of 
             (f':ront') -> (f', PQueue ront' [] tnerruc')      -- return head of queue
             [] -> (concatTo (List.reverse tnerruc'), PQueue [] [] [])    -- if head is still empty, return pending entry.

-}
              

pstrLit_printQ :: String -> PQueue B.ByteString String -> PQueue B.ByteString String
pstrLit_printQ str pq = insertPQ str pq

tuple_printQ :: (String, String, String) -> PQueue B.ByteString String -> PQueue B.ByteString String
tuple_printQ (s1,s2,s3) pq = pstrLit_printQ s3 (pstrLit_printQ s2 (pstrLit_printQ s1 pq))

rtuple_printQ :: (String, String, String) -> PQueue B.ByteString String -> PQueue B.ByteString String
rtuple_printQ ss pq = eorPQ (tuple_printQ ss pq)

list_printQ :: [(String,String,String)] -> PQueue B.ByteString String -> PQueue B.ByteString String
list_printQ items pq = 
   if List.null items then pq else
     let (first,rest) = List.splitAt 1 items
         pq' = list_printQ' first pq
     in addContPQ pq' (list_printQ rest emptyPQ)

list_printQ' :: [(String,String,String)] -> PQueue B.ByteString String -> PQueue B.ByteString String
list_printQ' [] pq0 =  pq0
list_printQ' (item:items) pq0 = list_printQ' items (rtuple_printQ item pq0)


data PQueue t s = PQueue [t] [t] [s] (Maybe (PQueue t s))

emptyPQ :: ConcatTo s t => PQueue t s
emptyPQ = PQueue [] [] [] Nothing

eorPQ :: ConcatTo s t => PQueue t s -> PQueue t s
eorPQ (PQueue front kcab tnerruc Nothing)     = PQueue front ((concatTo(List.reverse tnerruc)):kcab) [] Nothing
eorPQ (PQueue front kcab tnerruc (Just rest)) = PQueue front kcab tnerruc (Just (eorPQ rest))

insertPQ :: ConcatTo s t => s -> PQueue t s -> PQueue t s
insertPQ new (PQueue front kcab tnerruc Nothing)     = PQueue front kcab (new : tnerruc) Nothing
insertPQ new (PQueue front kcab tnerruc (Just rest)) = PQueue front kcab tnerruc (Just (insertPQ new rest))

addContPQ :: ConcatTo s t => PQueue t s -> PQueue t s -> PQueue t s
addContPQ (PQueue front kcab tnerruc Nothing)     pq2 = PQueue front kcab tnerruc (Just pq2)
addContPQ (PQueue front kcab tnerruc (Just rest)) pq2 = PQueue front kcab tnerruc (Just (addContPQ rest pq2))

lshiftPQ :: ConcatTo s t => PQueue t s -> PQueue t s
lshiftPQ (PQueue front1 kcab1 tnerruc1 next) = 
    PQueue (front1 ++ (List.reverse kcab1)) [] tnerruc1 next

addToFrontPQ :: ConcatTo s t => PQueue t s -> [s] -> PQueue t s
addToFrontPQ (PQueue front kcab tnerruc' rest)  tnerruc =
   case front of 
     (f:ront) -> PQueue ((cappend tnerruc f) : ront) kcab tnerruc' rest
     [] -> if not (List.null kcab) then
             let b:ack = List.reverse kcab
             in PQueue ((cappend tnerruc b) : ack) [] tnerruc' rest
           else if not (List.null tnerruc') then
                   PQueue [] [] (tnerruc' ++ tnerruc) rest
                else case rest of 
                   Nothing -> PQueue [] [] tnerruc Nothing
                   Just pq' -> addToFrontPQ pq' tnerruc

nullPQ :: PQueue t s -> Bool
nullPQ (PQueue front kcab tnerruc Nothing) = List.null front && List.null kcab && List.null tnerruc 
nullPQ (PQueue front kcab tnerruc (Just tail)) = List.null front && List.null kcab && List.null tnerruc && nullPQ tail

removePQ :: ConcatTo s t => PQueue t s -> (Maybe t, PQueue t s)
removePQ q @ (PQueue front kcab tnerruc tail)  = 
  case front of 
    (f:rest) -> (Just f, PQueue rest kcab tnerruc tail)                  -- return head of queue
    [] -> let (PQueue front' [] tnerruc' tail') = lshiftPQ q             -- if head is empty, normalize queue
          in case front' of 
             (f':ront') -> (Just f', PQueue ront' [] tnerruc' tail')      -- return head of queue
             [] -> case (tnerruc', tail') of      
                     ([], Nothing) -> (Nothing, emptyPQ)
                     (s,  Nothing) -> (Just (concatTo (List.reverse s)), emptyPQ)
                     ([], Just pq') -> removePQ pq'
                     (s,  Just pq') -> removePQ (addToFrontPQ pq' s)

class ConcatTo s t where
  concatTo :: [s] -> t
  cappend :: [s] -> t -> t

instance ConcatTo String B.ByteString where
  concatTo ss  = B.pack(List.concat ss)
  cappend ss b = B.append (concatTo ss) b


              
input = [("abc", "def", "ghi"),
         ("jkl", "mno", "pqr"),
         ("stu", "vwx", "yz0"),
         ("Kat", "hle", "en"),
         ("Fi",  "sh",  "er"),
         ("Sta", "nfo", "rd "),
         ("Uni", "ver", "sity")
         , undefined]

toStringPQ :: ConcatTo s t => PQueue t s -> [t]
toStringPQ pq = 
  let (item, pq') = removePQ pq
  in case item of 
      Nothing -> []
      Just i -> i : (toStringPQ pq')        



{- Helper functions -}
mkStr c = "'" ++ [c] ++ "'"

