-- Author: Michel Blockelet (supervised by David Walker)
-- Institution: Princeton University
-- Date: July 08

import System.Random
import Data.Typeable

import Types
import Utilities
import Parser
import Printer
import Gen
import Xml



{--- Test functions ---}
{- smallseed :: Take a random list and a seed (a small seed), and gives an unique list -}
smallseed :: Int -> [a] -> [a]
smallseed 0 l = let (la, lb) = splitoae l in la
smallseed n l = let (la, lb) = splitoae l in smallseed (n-1) lb

{- test :: Tests generation, parsing and XML generation -}
test :: Int -> [Int] -> Bool -> PADS x -> (String, String)
test n rl mix desc = let (la, lb) = splitoae rl in
                     let s = (let gs = do_gen [] (smallseed n la) desc in if mix then str_mixup lb gs else gs) in
                     let (a,b,c) = (parser [] desc s) in
                     (s, showXML (toXML [] desc (a,b)))
