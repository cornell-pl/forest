module Ex where
	
import Data.Generics.TH
	
data Foo = Foo Int Int
as :: Foo -> Foo
as = $(everywhere' (mkT 'f) [t| Foo |])
	where
	f :: Foo -> Foo
	f = id