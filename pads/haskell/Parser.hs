-- Author: Michel Blockelet (supervised by David Walker)
-- Institution: Princeton University
-- Date: July 08

module Parser where

import Data.Typeable

import Types
import Aux
import qualified Regexp


data ParserAnnot where
  ScanLimit :: Int -> ParserAnnot
  SyncChar  :: Char -> ParserAnnot
  ScanMode  :: Int -> ParserAnnot
  deriving Typeable


{--- ParserAnnot lookup functions ---}
{- pa_scanlimit :: scan limit when searching for a String -}
pa_scanlimit :: [Annot] -> Int
pa_scanlimit [] = 0
pa_scanlimit (x:xs) = case x of
                        Annot y -> case (cast x) of
                                     Just (ScanLimit n) -> n
                                     _ -> pa_scanlimit xs

{- pa_syncchar :: synchronization char -}
pa_syncchar :: [Annot] -> Char
pa_syncchar [] = '\n'
pa_syncchar (x:xs) = case x of
                       Annot y -> case (cast x) of
                                    Just (SyncChar c) -> c
                                    _ -> pa_syncchar xs

{- pa_scanmode :: scan mode -}
pa_scanmode :: [Annot] -> Int
pa_scanmode [] = 1
pa_scanmode (x:xs) = case x of
                       Annot y -> case (cast x) of
                                    Just (ScanMode n) -> n
                                    _ -> pa_scanmode xs



{--- Helper functions ---}
{- parse_pdata :: Data search -}
parse_pdata :: [Annot] -> [Branch a] -> String -> (a, PD, String)
parse_pdata env [] = error "parse_pdata :: no branches !"
parse_pdata env [(Branch ((_, f, _), p))] = \x -> let (y, pd, s) = parser env p x in (f y, pd, s)
parse_pdata env ((Branch ((_, f, _), p)):xs) = \x -> let (y, pd, s) = parser env p x in
                                                     case pd of PDbase Good -> (f y, pd, s)
                                                                PDlist Good _ -> (f y, pd, s)
                                                                PDdata Good _ -> (f y, pd, s)
                                                                _ -> parse_pdata env xs x



{--- The parser itself ---}
parser :: [Annot] -> PADS x -> String -> (x, PD, String)
parser env Pint = \x -> let (intl, reml) = (map_and_take (\c -> (fromEnum c) - 48) (\y -> and [y >= 0, y <= 9]) x) in
                        if null intl
                          then (0, PDbase (Bad "parser :: empty int" True), reml)
                          else (foldl (\x -> \y -> x*10 + y) 0 intl, PDbase Good, reml)
parser env Pchar = \x -> case x of [] -> (' ', PDbase (Bad "parser :: empty string" True), "")
                                   (x:xs) -> (x, PDbase Good, xs)
parser env Pfloat = \x -> let (intl1, reml1) = (map_and_take (\c -> (fromEnum c) - 48) (\y -> and [y >= 0, y <= 9]) x) in
                          case reml1 of
                            ('.':nl1) -> let (intl2, reml2) = (map_and_take (\c -> (fromEnum c) - 48) (\y -> and [y >= 0, y <= 9]) x) in
                                         if null (concat [intl1, intl2])
                                           then (0, PDbase (Bad "parser :: empty float" True), reml2)
                                           else ((int_to_float (foldl (\x -> \y -> x*10 + y) 0 intl1))
                                                 +(foldr (\x -> \y -> ((int_to_float x) + y)/10) 0 intl2), PDbase Good, reml2)
                            _ -> if null intl1
                                   then (0, PDbase (Bad "parser :: empty float" True), reml1)
                                   else (int_to_float (foldl (\x -> \y -> x*10 + y) 0 intl1), PDbase Good, reml1)
parser env Punit = \x -> ((), PDbase Good, x)
parser env (Pbefore s p) = \x -> let (spd, pl, reml) = (splitAtList s x (pa_scanlimit env)) in
                                 let (x, pd, reml2) = (parser env p reml) in
                                 case pl of
                                   [] -> (x, PDdata spd pd, reml2)
                                   _ -> (x, PDdata (Bad "parser :: remaining string" True) pd, reml2)
parser env (Pafter p s) = \x -> case (pa_scanmode env) of
                                  1 -> let (spd, pl, reml) = (splitAtList s x (pa_scanlimit env)) in
                                       let (val, pd, ereml) = (parser env p pl) in
                                       case ereml of
                                         [] -> case pd of
                                                 PDbase Good -> (val, PDdata spd pd, reml)
                                                 _ -> (val, PDdata (case spd of (Bad _ _) -> spd
                                                                                _ -> (Nested True)) pd, reml)
                                         _  -> case pd of
                                                 PDbase Good -> (val, PDdata (Bad "parser :: remaining string" True) pd, reml)
                                                 _ -> (val, PDdata (case spd of (Bad _ _) -> spd
                                                                                _ -> (Nested True)) pd, reml)
                                  _ -> let (val, pd, ereml) = (parser env p x) in
                                       let (spd, pl, reml) = (splitAtList s ereml (pa_scanlimit env)) in
                                       case pl of
                                         [] -> case pd of
                                                 PDbase Good -> (val, PDdata spd pd, reml)
                                                 _ -> (val, PDdata (case spd of (Bad _ _) -> spd
                                                                                _ -> (Nested True)) pd, reml)
                                         _  -> case pd of
                                                 PDbase Good -> (val, PDdata (Bad "parser :: remaining string" True) pd, reml)
                                                 _ -> (val, PDdata (case spd of (Bad _ _) -> spd
                                                                                _ -> (Nested True)) pd, reml)
parser env (Ppair p1 p2) = \x -> let (val1, pd1, reml1) = (parser env p1 x) in
                                 let (val2, pd2, reml2) = (parser env p2 reml1) in
                                 ((val1, val2), mkPDlist [pd1,pd2], reml2)
parser env (Pdpair id p fp) = \x -> let (val1, pd1, reml1) = (parser env p x) in 
                                    let (val2, pd2, reml2) = (parser env (fp val1) reml1) in
                                    ((val1, val2), mkPDlist [pd1,pd2], reml2)
parser env (Plist p s) = \x -> case (pa_scanmode env) of
                                 1 -> case (takeOut s x) of
                                        (Good, ns) -> ([], PDlist Good [], ns)
                                        (Bad _ _, _) -> let (y, pd, reml) = (parser env p x) in
                                                        case (parser env (Plist p s) reml) of
                                                         (ry, PDlist _ ll, rreml) -> (y:ry, mkPDlist (pd:ll), rreml)
                                 _ -> (\(la, lb, lc) -> if null lc
                                                        then let (spd, pl, reml) = (splitAtList s x (pa_scanlimit env)) in
                                                             ([], PDlist spd [], reml)
                                                        else let (spd, pl, reml) = (splitAtList s (last lc) (pa_scanlimit env)) in
                                                             case spd of Good -> (la, mkPDlist lb, reml)
                                                                         Bad _ _ -> (la, PDlist spd lb, reml))
                                      (unzip3 (takeWhile (\(_, pd, _) -> case getPDErrorCode pd of Good -> True
                                                                                                   _ -> False)
                                                         (iterate (\(_, _, y) -> parser env p y) (parser env p x))))
parser env (PlistFW p n) = \x -> let (reml, ly) = takeLast n (unfold (\y -> let (cx, cpd, cs) = parser env p y in (cs,(cx, cpd))) x) in
                                 (map (\(a,b) -> a) ly, mkPDlist (map (\(a,b) -> b) ly), reml)
parser env (Pnamed p s) = parser env p
parser env (Pannot p a) = parser (a:env) p
parser env (Pdata l) = \x -> parse_pdata env l x
