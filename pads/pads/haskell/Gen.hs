-- Author: Michel Blockelet (supervised by David Walker)
-- Institution: Princeton University
-- Date: July 08

module Gen where

import Data.Typeable
import System.Random

import Types
import Utilities
import Parser



data GenAnnot where
  IntLimit :: Int -> GenAnnot
  UserFunc :: ([Int] -> String) -> GenAnnot
  deriving Typeable


{--- GenAnnot lookup functions ---}
{- ga_intlimit :: limit for Pints -}
ga_intlimit :: [Annot] -> Int
ga_intlimit [] = 2^30
ga_intlimit (x:xs) = case x of Annot y -> case (cast y) of Just (IntLimit n) -> n
                                                           _ -> ga_intlimit xs

{- ga_userfunc :: user function for generating some part of the description -}
ga_userfunc :: Annot -> [Int] -> String -> String
ga_userfunc x rl val = case x of
                         Annot y -> case (cast x) of
                                      Just (UserFunc f) -> f rl
                                      _ -> val



{--- The generator itself ---}
do_gen :: [Annot] -> [Int] -> PADS x -> String
do_gen env (x:rl) Pint = show (mod x (ga_intlimit env))
do_gen env (x:rl) Pchar = showChar (toEnum (mod x 127)) ""
do_gen env (x:rl) Pfloat = show ((int_to_float x)/100000)
do_gen env rl Punit = ""
do_gen env rl (Pbefore s p) = s ++ (do_gen env rl p)
do_gen env rl (Pafter p s) = (do_gen env rl p) ++ s
do_gen env rl (Ppair pa pb) = let (rla, rlb) = splitoae rl in
                              (do_gen env rla pa) ++ (do_gen env rlb pb)
do_gen env rl (Pdpair id p f) = let (rla, rlb) = splitoae rl in
                            let sy = (do_gen env rla p) in
                            let (py, _, _) = (parser env p sy) in
                            sy ++ (do_gen env rlb (f py))
do_gen env (n:rl) (Plist p s) = (foldl (++) "" (map (\x -> do_gen env (takeEvery (mod n 20) rl x) p) [0..((mod n 20)+1)])) ++ s
do_gen env rl (PlistFW p n) = foldl (++) "" (map (\x -> do_gen env (takeEvery n rl x) p) [0..(n-1)])
do_gen env rl (Pnamed p n) = do_gen env rl p
do_gen env rl (Pannot p a) = ga_userfunc a rl (do_gen env rl p)
do_gen env rl (Pdata l) = case (head l) of Branch ((_, f, _), p) -> do_gen env rl p



{--- Mix up a string ---}
str_mixup :: [Int] -> String -> String
str_mixup (m:n:rl) [] = if (mod m 25) == 0 then showChar (toEnum (mod n 127)) "" else ""
str_mixup (m:n:rl) (x:xs) = case (mod m 25) of
                              0 -> (toEnum (mod n 127)):(str_mixup rl xs)
                              1 -> (toEnum (mod n 127)):x:(str_mixup rl xs)
                              2 -> str_mixup rl xs
                              _ -> x:(str_mixup rl xs)
