module Ex where
	
import Language.Haskell.TH
import Data.Generics.TH
import GHC.Prim
import GHC.Exts
	
data Foo = A Int | Foo Int Int#
as :: Foo -> Foo
as = $(everywhere' (mkT 'f) [t| Foo |])


--as :: Foo -> Foo
--as = \ x -> (\ y -> case y of { (Foo a b) -> Foo (g a) (g b) }) (f x)
--g = \ x -> \ y -> y x

f :: Foo -> Foo
f = id

test :: (Foo) -> Foo
test x = let !(Foo a b) = x in Foo a b

showUnboxedInt :: Int# -> String
showUnboxedInt n = (show $ I# n) ++ "#"

--as :: Foo -> Foo
--as (Foo a b) = Foo (id a) (id b)




