{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}

module Examples.Syn where
import Language.Pads.Padsc
import Language.Pads.Parser
import Text.Parsec.Prim
import Text.Parsec.Pos
import Control.Monad.Identity
import Language.Haskell.Meta as LHM

import System.IO.Unsafe (unsafePerformIO)

--       type Hours_t = h :: Pint 
--       type Hours_t = Pint


[pads| data Date_t = {day::Pint}  
       type Hours_t = constrain h :: Pint where <| 0 <= h && h < 24 |>    |]



parser = dataDecl
input = "data Date_t = {day::Pint}\n-- type Hours_t = h :: Pint where <| True |>\n"
res = Text.Parsec.Prim.runP parser () "test" input


parser2 = recordTy2 "Date_t"
input2 = "{day::Pint}\n-- type Hours_t = h :: Pint where <| True |>\n"
res2 = Text.Parsec.Prim.runP parser2 () "test" input2

{-
parser3 = recordTest 
input3 = "day::Pint}\n-- type Hours_t = h :: Pint where <| True |>\n"
res3 = Text.Parsec.Prim.runP parser3 () "test" input3

-}
-- res2 = runIdentity $ runPT parser () "test" input
-- res3 = Control.Monad.Identity.runIdentity $ Text.Parsec.Prim.runParsecT parser (Text.Parsec.Prim.State input (Text.Parsec.Pos.initialPos "test") ())