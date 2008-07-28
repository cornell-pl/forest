-- Author: Michel Blockelet (supervised by David Walker)
-- Institution: Princeton University
-- Date: July 08

module Printer where

import Types
import Aux



{- Printer -}
printer :: [Annot] -> PADS x -> x -> String
printer env Pint = show
printer env Pchar = \x -> showChar x ""
printer env Pfloat = show
printer env Punit = \x -> ""
printer env (Pbefore s p) = \x -> s ++ (printer env p x)
printer env (Pafter p s) = \x -> (printer env p x) ++ s
printer env (Ppair a b) = \(x,y) -> printer env a x ++ printer env b y
printer env (Pdpair s p f) = \(x,y) -> printer env p x ++ printer env (f x) y
printer env (Plist p s) = \x -> case x of [] -> ""
                                          (a:r) -> (printer env p a) ++ printer env (Plist p s) r
printer env (PlistFW p n) = \x -> case x of [] -> ""
                                            (a:r) -> (printer env p a) ++ printer env (PlistFW p n) r
printer env (Pnamed p s) = printer env p
printer env (Pannot p a) = printer (a:env) p
printer env (Pdata l) = \x -> case (brancher l x) of (BranchedValue (p, y)) -> (printer env p y)
