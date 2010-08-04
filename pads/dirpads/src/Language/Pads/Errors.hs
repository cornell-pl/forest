module Language.Pads.Errors where
import Text.PrettyPrint.Mainland as PP
import qualified Language.Pads.Source as S
import Data.Data

data ErrMsg = 
   FoundWhenExpecting String String
 | MissingLiteral String
 | ExtraBeforeLiteral String
 | LineError String
 | Insufficient Int Int
 | RegexMatchFail String
 | TransformToDstFail String String String
 | TransformToSrcFail String String String
 | UnderlyingTypedefFail
 | PredicateFailure
   deriving (Typeable, Data, Eq)

{- XXX-KSF: fix pretty printing to use pretty printing combinators rather than string ++ -}
instance Pretty ErrMsg where
  ppr (FoundWhenExpecting str1 str2) = text ("Encountered " ++ str1 ++ " when expecting " ++ str2 ++ ".")
  ppr (MissingLiteral s)     = text ("Missing Literal: " ++ s ++ ".")
  ppr (ExtraBeforeLiteral s) = text ("Extra bytes before literal: " ++ s ++ ".")
  ppr (Insufficient found expected) = text("Found " ++ (show found) ++ " bytes when looking for " ++ (show expected) ++ "bytes.")
  ppr (RegexMatchFail s) = text ("Failed to match regular expression: " ++ s ++ ".")
  ppr (TransformToDstFail s1 s2 s3) = text ("Parsing transform " ++ s1 ++ " failed on input: " ++ s2 ++ s3)
  ppr (TransformToSrcFail s1 s2 s3) = text ("Printing transform "++ s1 ++ " failed on input: " ++ s2 ++ s3)
  ppr (LineError s)        = text s
  ppr UnderlyingTypedefFail  = text "Predicate is true, but underlying type had an error."
  ppr PredicateFailure       = text "Predicate is false."


data ErrInfo = ErrInfo { msg      :: ErrMsg,
                         position :: Maybe S.Pos }
   deriving (Typeable, Data, Eq)

instance Pretty ErrInfo where
  ppr (ErrInfo {msg,position}) = ppr msg <+> 
       case position of 
         Nothing -> empty
         Just pos -> (text "at:") <+>  ppr pos

