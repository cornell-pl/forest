{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-type-defaults #-} -- See Note [Type defaults]

-- Note that this file takes a little bit longer to compile than the
-- others (e.g. ~150 seconds vs <1 second).
--
-- This file contains a large number transformations over a large
-- number of types (i.e. all the types in Language.Haskell.Syntax).
-- This seems to trigger poor compile-time performance.
--
-- We are looking into fixing this.

module Syntax where

import Data.Generics.TH
import Control.Monad.Reader
import Language.Haskell.Syntax

----------------------------------------
-- Id
----------------------------------------
id_All :: HsModule -> HsModule
id_All = $(everywhere (const [|id|]) [t|HsModule|])

id_HsName :: HsModule -> HsModule
id_HsName = $(everywhere (const [|id|] `extE` (eqType [t|HsName|], [|id|])) [t|HsModule|])

----------------------------------------
-- Const
----------------------------------------

const_HsName :: HsName -> HsModule -> HsModule
const_HsName c = $(everywhere (const [|id|] `extE` (eqType [t|HsName|], [|const c|])) [t|HsModule|])

const_HsName_But :: HsName -> HsModule -> HsModule
const_HsName_But c = $(everywhereBut (liftM not . inType [t|HsName|])
                       (const [|id|] `extE` (eqType [t|HsName|], [|const c|])) [t|HsModule|])

----------------------------------------
-- Counting
----------------------------------------

count_HsModule_All :: HsModule -> Int
count_HsModule_All = $(everything [|(+)|] (const [|const 1|]) [t|HsModule|])

count_HsName :: HsModule -> Int
count_HsName = $(everything [|(+)|] (mkQ [|0|] 'f) [t|HsModule|])
  where f :: HsName -> Int
        f _ = 1

count_HsName_Acc :: HsModule -> Int
count_HsName_Acc x = $(everythingAccR (mkQ [|id|] 'f) [t|HsModule|]) x 0
  where f :: HsName -> Int -> Int
        f _ = (+1)

count_HsName_ForR :: HsModule -> Int
count_HsName_ForR x = $(everythingForR 'f [t|HsModule|]) x 0
  where f :: HsName -> Int -> Int
        f _ = (+1)

count_HsName_ForL :: HsModule -> Int
count_HsName_ForL x = $(everythingForL 'f [t|HsModule|]) x 0
  where f :: HsName -> Int -> Int
        f _ = (+1)

count_HsName_ForL' :: HsModule -> Int
count_HsName_ForL' x = $(everythingForL' 'f [t|HsModule|]) x 0
  where f :: HsName -> Int -> Int
        f _ = (+1)

-- 148us if we only check eqType, 117us if we check both eqType and inType
count_HsName_But :: HsModule -> Int
count_HsName_But = $(let g t = do b <- eqType [t|HsName|] t
                                  c <- inType [t|HsName|] t
                                  if b then do h <- [|const 1|]
                                               return (h, True)
                                       else do h <- [|const 0|]
                                               return (h, not c)
                     in everythingBut [|(+)|] g [t|HsModule|])

count_HsName_But_Acc :: HsModule -> Int
count_HsName_But_Acc x = $(let g t = do b <- eqType [t|HsName|] t
                                        c <- inType [t|HsName|] t
                                        if b then do h <- [|const (+1)|]
                                                     return (h, True)
                                             else do h <- [|const id|]
                                                     return (h, not c)
                  in everythingButAccR g [t|HsModule|]) x 0

count_HsName_For :: HsModule -> Int
count_HsName_For x = let f = (\_ -> (+1)) :: HsName -> Int -> Int
                     in $(everythingForL' 'f [t|HsModule|]) x 0

----------------------------------------
-- Listing
----------------------------------------

list_HsName_List :: HsModule -> [HsName]
list_HsName_List = $(everything [|(++)|] (mkQ [|[]|] 'f) [t|HsModule|])
  where f :: HsName -> [HsName]
        f x = [x]

list_HsName_But :: HsModule -> [HsName]
list_HsName_But = $(everythingBut [|(++)|]  (\t -> do
                                                e <- mkQ [|[]|] 'f t
                                                b   <- liftM not (inType [t|HsName|] t) 
                                                return (e,b))
                                     [t|HsModule|])
  where f :: HsName -> [HsName]
        f x = [x]

list_HsName_Comp :: HsModule -> [HsName]
list_HsName_Comp x = $(everything [|(.)|] (mkQ [|id|] 'f) [t|HsModule|]) x []
  where f :: HsName -> [HsName] -> [HsName]
        f = (:)

list_HsName_Comp_But :: HsModule -> [HsName]
list_HsName_Comp_But x = $(let g t = do b <- eqType [t|HsName|] t
                                        c <- inType [t|HsName|] t
                                        if b then do h <- [|(:)|]
                                                     return (h, True)
                                             else do h <- [|const id|]
                                                     return (h, not c)
                           in everythingBut [|(.)|] g [t|HsModule|]) x []
  where f :: HsName -> [HsName] -> [HsName]
        f = (:)

list_HsName_AccR :: HsModule -> [HsName]
list_HsName_AccR x = $(everythingAccR (mkQ [|id|] 'f) [t|HsModule|]) x []
 where
 f :: HsName -> [HsName] -> [HsName]
 f = (:)


list_HsName_AccL :: HsModule -> [HsName]
list_HsName_AccL x = $(everythingAccL (mkQ [|id|] 'f) [t|HsModule|]) x []
 where
 f :: HsName -> [HsName] -> [HsName]
 f = (:)

list_HsName_AccL' :: HsModule -> [HsName]
list_HsName_AccL' x = $(everythingAccL' (mkQ [|id|] 'f) [t|HsModule|]) x []
 where
 f :: HsName -> [HsName] -> [HsName]
 f = (:)

list_HsName_Acc_But :: HsModule -> [HsName]
list_HsName_Acc_But x =  $(let rec memoF t = do
                                 is <- eqType [t|HsName|] t
                                 contains <- inType [t|HsName|] t
                                 case (is, contains) of
                                   (True, _) -> [|\acc name -> name : acc|]
                                   (_, True) -> [|\acc -> $(thcase (g [|acc|]) (return t))|]
                                   (_, _) -> [|\acc _ -> acc|]
                                 where g acc _ctor [] = acc
                                       g acc ctor ((t, v):vs) = [|$(memoF t) $(g acc ctor vs) $v|]
                           in [t|HsModule|] >>= memoizeExp rec) [] x


list_HsName_AccR_But :: HsModule -> [HsName]
list_HsName_AccR_But x = $(let h t = do b <- eqType [t|HsName|] t
                                        c <- inType [t|HsName|] t
                                        if b then do h <- [|(:)|]
                                                     return (h, True)
                                             else do h <- [|const id|]
                                                     return (h, not c)
                           in everythingButAccR h [t|HsModule|]) x []

list_HsName_ForR :: HsModule -> [HsName]
list_HsName_ForR x = $(everythingForR 'f [t|HsModule|]) x []
  where f :: HsName -> [HsName] -> [HsName]
        f = (:)

list_HsName_AccL_But :: HsModule -> [HsName]
list_HsName_AccL_But x = $(let h t = do b <- eqType [t|HsName|] t
                                        c <- inType [t|HsName|] t
                                        if b then do h <- [|(:)|]
                                                     return (h, True)
                                             else do h <- [|const id|]
                                                     return (h, not c)
                           in everythingButAccL h [t|HsModule|]) x []

list_HsName_ForL :: HsModule -> [HsName]
list_HsName_ForL x = $(everythingForL 'f [t|HsModule|]) x []
  where f :: HsName -> [HsName] -> [HsName]
        f = (:)

list_HsName_AccL'_But :: HsModule -> [HsName]
list_HsName_AccL'_But x = $(let h t = do b <- eqType [t|HsName|] t
                                         c <- inType [t|HsName|] t
                                         if b then do h <- [|(:)|]
                                                      return (h, True)
                                              else do h <- [|const id|]
                                                      return (h, not c)
                            in everythingButAccL' h [t|HsModule|]) x []

list_HsName_ForL' :: HsModule -> [HsName]
list_HsName_ForL' x = $(everythingForL' 'f [t|HsModule|]) x []
  where f :: HsName -> [HsName] -> [HsName]
        f = (:)

-- Slower than list_HsName_Comp
list_HsName_Comp2 :: HsModule -> [HsName]
list_HsName_Comp2 x = $(everything [|flip (.)|] (mkQ [|id|] 'f) [t|HsModule|]) x []
  where f :: HsName -> [HsName] -> [HsName]
        f = (:)

----------------------------------------
-- Prefixing
----------------------------------------

prefix :: HsName -> HsName
prefix (HsIdent s) = HsIdent ('x' : s)
prefix x = x

prefix_HsName :: HsModule -> HsModule
prefix_HsName = $(everywhere (const [|id|] `extN` 'prefix) [t|HsModule|])

prefix_HsName_But :: HsModule -> HsModule
prefix_HsName_But = $(everywhereBut (liftM not . inType [t|HsName|]) (const [|id|] `extN` 'prefix) [t|HsModule|])

----------------------------------------
-- A useful helper for benchmarking
----------------------------------------

deepseq_HsModule = 
  $(let --rec :: (TH.Type -> Q TH.Exp) -> TH.Type -> Q TH.Exp
        rec k t = thcase f (return t) where
          --f :: Q TH.Exp -> [(TH.Type, Q TH.Exp)] -> Q TH.Exp
          f _ [] = [| () |]
          f _ ((t, x) : xs) = [| $(k t) $x `seq` $(f undefined xs) |]
    in [t|HsModule|] >>= memoizeExp rec)

----------------------------------------
-- Notes
----------------------------------------

{-
Note [Type defaults]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC 7.0.3 has (we think) a bug where it reports type default warnings
in Template Haskell code even though no type defaulting is actually
happening.  Until it is fixed, we just have to tell GHC to be quiet
about it.
-}
