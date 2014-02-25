-- Author: Michel Blockelet (supervised by David Walker)
-- Institution: Princeton University
-- Date: July 08

module Types where

{--- Data types ---}
import Data.Array
import Data.Typeable



{--- Main GADT ---}
data PADS x where
  Pint    :: PADS Int
  Pchar   :: PADS Char
  Pfloat  :: PADS Float
  Punit   :: PADS ()
  Pbefore :: String -> PADS a -> PADS a
  Pafter  :: PADS a -> String -> PADS a
  Ppair   :: PADS a -> PADS b -> PADS (a,b)
  Pdpair  :: Id -> PADS a -> (a -> PADS b) -> PADS (a,b)
  Plist   :: PADS a -> String -> PADS ([a])
  PlistFW :: PADS a -> Int -> PADS ([a])
  Pnamed  :: PADS a -> Id -> PADS a
  Pannot  :: PADS a -> Annot -> PADS a
  Pdata   :: [Branch a] -> PADS a

instance Show (PADS x) where
  show Pint = "Pint"
  show Pchar = "Pchar"
  show Pfloat = "Pfloat"
  show Punit = "Punit"
  show (Pbefore s p) = "Pbefore(\""++s++"\", "++(show p)++")"
  show (Pafter p s) = "Pafter("++(show p)++", \""++s++"\")"
  show (Ppair pa pb) = "Ppair("++(show pa)++", "++(show pb)++")"
  show (Pdpair id p pf) = "Pdpair("++id++" :: "++(show p)++", ...)"
  show (Plist p s) = "Plist("++(show p)++", \""++s++"\")"
  show (PlistFW p n) = "PlistFW("++(show n)++"x "++(show p)++")"
  show (Pnamed p name) = "Pnamed("++name++" :: "++(show p)++")"
  show (Pannot p a) = "Pannot("++(show p)++", ...)"
  show (Pdata l) = "Pdata(...)"

type Id = String

data Annot = forall a. (Typeable a) => Annot a
  deriving Typeable

type Pcons a b = (String, a -> b, b -> Maybe a)
data Branch a = forall b. Branch (Pcons b a, PADS b)
data BranchedValue = forall b. BranchedValue (PADS b, b)



{--- Parsing error codes ---}
data PDErrorCode where
  Good   :: PDErrorCode
  Bad    :: String -> Bool -> PDErrorCode -- String :: error message
  Nested :: Bool -> PDErrorCode
  deriving Show

data PD where
  PDbase :: PDErrorCode -> PD
  PDlist :: PDErrorCode -> [PD] -> PD
  PDdata :: PDErrorCode -> PD -> PD
  deriving Show



{--- Regular Expressions ---}
type RETag = String
type REName = String

data RE where
  REChar :: Char -> RE
  REFchar :: (Char -> Bool) -> RE -- for ranges of characters
  REStar :: RE -> RE
  REOr :: RE -> RE -> RE
  REConcat :: [RE] -> RE
  RETag :: RETag -> RE
  RENamed :: REName -> RE -> RE

instance Show RE where
  show (REChar c) = "REChar("++(c:")")
  show (REFchar f) = "REFchar(...)"
  show (REStar re) = "REStar("++(show re)++")"
  show (REOr rea reb) = "REOr("++(show rea)++", "++(show reb)++")"
  show (REConcat rel) = "REConcat("++(show rel)++")"
  show (RETag tag) = "RETag("++(show tag)++")"
  show (RENamed name re) = "RENamed("++(show name)++" :: "++(show re)++")"



{--- Automatons ---}
-- Autom = (end state number, array of transitions starting from state n for each n in the array)
type Autom = (AutomState, Array AutomState AutomTransition)
-- AutomTransition = character -> list of possible end states and tags we got through
-- [Maybe AutomTag] could be replaced by [AutomTag] (but it was easier to program)
type AutomTransition = (Char -> [([Maybe AutomTag], AutomState)])
-- AutomTrList :: list of transitions with tags : (start state, tag, transition function, end state)
type AutomTrList = [(AutomState, Maybe AutomTag, AutomTr, AutomState)]
type AutomState = Int

data AutomTag where
  TagSimple :: RETag -> AutomTag
  TagName :: REName -> Bool -> AutomTag -- Bool :: True if beginning, False if ending
  deriving Show

data AutomTr where
  TrSChar :: Char -> AutomTr -- accept a specific characters
  TrFChar :: (Char -> Bool) -> AutomTr -- accept a range of characters
  TrEpsilon :: AutomTr -- epsilon-transition

instance Show AutomTr where
  show (TrSChar c) = "TrSChar("++(c:")")
  show (TrFChar f) = "TrFChar(...)"
  show (TrEpsilon) = "TrEpsilon"
