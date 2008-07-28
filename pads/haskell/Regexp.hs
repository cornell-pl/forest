-- Author: Michel Blockelet (supervised by David Walker)
-- Institution: Princeton University
-- Date: July 08

module Regexp where

import Data.Array.IArray

import Types
import Aux



{--- Helper functions ---}
{- state_shift :: shift states numbers -}
state_shift :: Int -> AutomTrList -> AutomTrList
state_shift n = map (\(a, tag, tr, b) -> (a+n, tag, tr, b+n))



{--- Regexp to automaton ---}
{- re_to_automl :: generates (end state, a list of transitions from RE)
 - start state is always 0 -}
re_to_automl :: RE -> (Int, AutomTrList)
re_to_automl (REChar c) = (1, [(0, Nothing, TrSChar c, 1)])
re_to_automl (REFchar f) = (1, [(0, Nothing, TrFChar f, 1)])
re_to_automl (REStar re) = let (end, ltr) = re_to_automl re in (end, (end, Nothing, TrEpsilon, 0):ltr) -- only loop the end state with the start state (0)
re_to_automl (REOr re1 re2) = let (end1, ltr1) = re_to_automl re1
                                  (end2, ltr2) = re_to_automl re2 in
                              (end1+end2, concat [[(0, Nothing, TrEpsilon, 1),
                                                   (0, Nothing, TrEpsilon, end1+2),
                                                   (end1+1, Nothing, TrEpsilon, end1+end2+3),
                                                   (end1+end2+2, Nothing, TrEpsilon, end1+end2+3)],
                                                  state_shift 1 ltr1, state_shift (end1+2) ltr2])
re_to_automl (REConcat lre) = foldl (\(end, ltr) -> \re -> let (nend, ntr) = (re_to_automl re) in
                                                           (nend+end, concat [state_shift end ntr, ltr])) (0, []) lre
re_to_automl (RETag tag) = (1, [(0, Just (TagSimple tag), TrEpsilon, 1)])
re_to_automl (RENamed name re) = let (end, ltr) = re_to_automl re in
                                 (end+2, (0, Just (TagName name True), TrEpsilon, 1):(end+1, Just (TagName name False), TrEpsilon, end+2):(state_shift 1 ltr))



{- processTr :: transition predicate -}
processTr :: AutomTr -> Char -> Bool
processTr (TrSChar c) = \x -> x == c
processTr (TrFChar f) = \x -> f x


{- accessible :: lists all accessible states with epsilon-transitions -}
accessible :: AutomTrList -> [([Maybe AutomTag], AutomState)] -> [([Maybe AutomTag], AutomState)]
accessible l tsl = let sl = map (\(_, x) -> x) tsl in
                   let (lok, lnok) = spanAll (\x -> case x of (a, _, TrEpsilon, b) -> elem a sl
                                                              _ -> False) l in
                   let lnew = filter (\(_, _, x) -> notElem x sl) (map (\(a, tag, _, b) -> (a, tag, b)) lok) in
                   if null lnew
                     then tsl
                     else accessible lnok
                       (concat [map (\(a, tag, b) -> let (otag, _) = head (filter (\(_, oa) -> a == oa) tsl) in (tag:otag, b)) lnew, tsl])


{- tr_from :: lists all transitions from a state -}
tr_from :: AutomTrList -> [([Maybe AutomTag], AutomState)] -> [([Maybe AutomTag], AutomTr, AutomState)]
tr_from [] nl = []
tr_from ((s, t, tr, e):xs) nl = if elem s (map (\(_, x) -> x) nl)
                                then case tr of TrEpsilon -> tr_from xs nl
                                                _ -> (t:((\(tagl, _) -> tagl) (head (filter (\(_, x) -> x == s) nl))), tr, e):(tr_from xs nl)
                                else tr_from xs nl


{- automl_to_autom :: generates the automaton from a transition list -}
automl_to_autom :: (Int, AutomTrList) -> Autom
automl_to_autom (end, l) = (end, (array (0, end)
                            (map (\n -> (n,
                                         (\c -> map (\(a, _, b) -> (a,b))
                                                    (filter (\(_, tr, _) -> processTr tr c)
                                                            (tr_from l (accessible l [([], n)]))))))
                                 [0..end])))



{- re_to_autom :: everything combined ! -}
re_to_autom :: RE -> Autom
re_to_autom = automl_to_autom . re_to_automl



{--- The Regexp matcher ---}
{- processTags :: processes the tags on transitions -}
processTags :: Int -> String -> AutomState -> [(RETag, Int)] -> [(REName, String)] -> [Maybe AutomTag] -> (AutomState, [(RETag, Int)], [(REName, String)])
processTags pos xs ns tagl namel [] = (ns, tagl, namel)
processTags pos xs ns tagl namel (ntag:otag) = case ntag of
                                        Nothing -> processTags pos xs ns tagl namel otag
                                        Just (TagSimple nstag) -> processTags pos xs ns ((nstag, pos):tagl) namel otag
                                        Just (TagName nntag begin) -> if begin
                                                                      then processTags pos xs ns (("__begin_" ++ nntag, pos):tagl) ((nntag, xs):namel) otag
                                                                      else processTags pos xs ns (("__end_" ++ nntag, pos):tagl) namel otag


{- match_main :: does the main part of matching -}
match_main :: Autom -> [(AutomState, [(RETag, Int)], [(REName, String)])] -> Int -> String -> (Bool, [(RETag, Int)], [(REName, String)])
match_main (end, trarray) sl pos [] = if any (\(st, _, _) -> st == end) sl
                                      then let (_, tagl, namel) = head (filter (\(x, _, _) -> x == end) sl) in (True, tagl, namel)
                                      else (False, [], [])
match_main (end, trarray) sl pos (x:xs) = let nl = concat (map (\(n, tagl, namel) ->
                                                            map (\(ntag, ns) -> processTags pos xs ns tagl namel ntag)
                                                             ((trarray!n) x))
                                                       sl) in
                                          match_main (end, trarray) nl (pos+1) xs


{- match :: does some post-processing of match_main results
 - match :: the automaton -> (string matches the RE, [tag name, position where we encountered the tag], [substring name, associated string]) -}
match :: Autom -> String -> (Bool, [(RETag, Int)], [(REName, String)])
match autom xs = let (b, tagl, namel) = match_main autom [(0, [], [])] 1 xs in
                 (b, tagl, map (\(name, str) -> (name, take (((\(_, n) -> n) (head (filter (\(tag, _) -> tag == "__end_" ++ name) tagl)))
                                                             -((\(_, n) -> n) (head (filter (\(tag, _) -> tag == "__begin_" ++ name) tagl)))) str)) namel)



{--- The Regexp-compliant string generator ---}
{- bruteforce :: find a char accepted by f ...
 - function -> random int list -> number of times we tried -> result -}
bruteforce :: (Char -> Bool) -> [Int] -> Int -> Char
bruteforce f (x:rl) n = if n > 127
                        then 'a'
                        else let c = toEnum (mod x 128) in
                             if (f c) then c else (bruteforce f rl (n+1))


{- gen :: generate a string that matches a RE -}
gen :: [Annot] -> [Int] -> RE -> String
gen env rl (REChar c) = show c
gen env rl (REFchar f) = showChar (bruteforce f rl 0) ""
gen env (x:rl) (REStar re) = let n = (mod x 10) in (concat (map (\m -> gen env (takeEvery n rl m) re) [1..n]))
gen env (x:rl) (REOr re1 re2) = if (mod x 2) == 0 then gen env rl re1 else gen env rl re2
gen env rl (REConcat lre) = let n = length lre in (concat (map (\(m, re) -> gen env (takeEvery n rl m) re) (zip [1..n] lre)))
gen env rl (RETag tag) = ""
gen env rl (RENamed name re) = gen env rl re
