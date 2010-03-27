module Language.Pads.Source where
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Int
import Data.Data
import Text.PrettyPrint.Mainland as PP

{- Input source abstraction -}
data Loc = Loc { lineNumber :: Int64,
                 byteOffset :: Int64 }
     deriving (Typeable, Data,Eq)

instance Pretty Data.Int.Int64 where
  ppr i = text(show i)

instance Pretty Loc where
 ppr (Loc{lineNumber,byteOffset}) = text "Line:" <+> ppr lineNumber <> text ", Offset:" <+> ppr byteOffset 

data Source = Source {current  :: B.ByteString,
                      rest     :: B.ByteString,
                      atEOF    :: Bool,
                      loc      :: Loc }

data Pos = Pos { line       :: B.ByteString,
                 begin      :: Loc,
                 end        :: Maybe Loc}
  deriving (Typeable, Data, Eq)
instance Pretty Pos where 
  ppr (Pos{line,begin,end}) = case end of
                                Nothing -> ppr begin
                                Just end_loc ->  text "from:" <+> ppr begin <+> text "to:" <+> ppr end_loc
                                


{- Called when current is empty.
   Should not be called when atEOF is already set. -}
getNextRecord_newline (s @ Source {current, rest, atEOF, loc = Loc{lineNumber, byteOffset}}) = 
      if atEOF then s
      else if B.null rest then
            (Source {current = B.empty, rest = rest, atEOF = True, loc = Loc{lineNumber, byteOffset=0}})
      else  (Source {current = nextRecord, rest=residual, atEOF = False, loc = Loc{lineNumber=lineNumber+1, byteOffset=0}})
        where (nextRecord, raw_residual) = B.break (\c->c == '\n') rest
              residual = B.drop 1 raw_residual

padsSourceFromString str = getNextRecord_newline (Source{current = B.empty,
                                                         rest    = B.pack str,
                                                         atEOF   = False,   -- if string is empty, will be made True by getNextRecord_newline 
                                                         loc     = Loc{ lineNumber = -1,   -- will be incremeneted to 0 by getNextRecord_newline
                                                                        byteOffset = 0}})
padsSourceToString (Source {current, rest, ..}) = B.unpack (B.concat [current,rest])
isEOF (Source{atEOF,..}) = atEOF
isEOR (Source{current,..}) = B.null current
head  (Source{current,..}) = B.head current
tail  (Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
       (Source{current=B.tail current,rest,atEOF,loc=Loc{byteOffset=byteOffset+1,lineNumber}})
getPos (Source {current, rest, atEOF, loc}) = 
   Pos {line=current, begin = loc, end = Nothing }
scanTo chr (src @ Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
     let Pos {line,begin,..} = getPos src
         (skipped, residual) = B.break (\c->c==chr) current
         (found,remaining,newByteOffset) =   
            if B.null residual then   -- Reached EOR w/o finding chr
                 (False, residual,        byteOffset + (B.length skipped))
            else (True,  B.tail residual, byteOffset + (B.length skipped) + 1)
         newLoc    = Loc{lineNumber, byteOffset = newByteOffset}
         endErrLoc = Loc{lineNumber, byteOffset = byteOffset + (B.length skipped)}
      in (found, 
          Source {current = remaining, rest, atEOF, loc=newLoc},
          Pos    {line, begin, end=Just endErrLoc})
