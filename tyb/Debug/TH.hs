{-# LANGUAGE TemplateHaskell #-}
module Debug.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

--------------------------------------------------------------------------------
-- Debugging Helpers
--------------------------------------------------------------------------------

{-|
Debugging Template Haskell stuff at the GHCi REPL is hard because everything
ends up in the "Q" monad and there is no way to print the "Q" monad.  These functions
call pprint and show but return a "Q Exp" so that the following expressions work
to print a given value (e.g. "x"):
  $(pprintQ x)
  $(showQ x)

NOTE: GHCi likes to run the contents of splices twice or more, so the results
may be printed multiple times.
-}

pprintQ :: (Ppr a) => Q a -> Q Exp
pprintQ x = x >>= (qRunIO . putStrLn . pprint) >> [|return ()|]

showQ :: (Show a) => Q a -> Q Exp
showQ x = x >>= (qRunIO . putStrLn . show) >> [|return ()|]
