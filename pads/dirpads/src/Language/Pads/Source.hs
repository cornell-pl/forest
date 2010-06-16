module Language.Pads.Source where
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Int
import Data.Data
import Text.PrettyPrint.Mainland as PP
import qualified Text.Regex.ByteString as BRE

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

instance Pretty Source where 
    ppr (Source{current, rest, ..}) = text "Current:" <+> text (show current)
                                

{- Regular expression support -}
data RE = RE String
  deriving (Eq, Data, Typeable, Show)

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
takeHead (Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
       (B.head current, Source{current=B.tail current,rest,atEOF,loc=Loc{byteOffset=byteOffset+1,lineNumber}})

take n (Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
     let (head, tail) = B.splitAt n current
         newOffset    = byteOffset + (B.length head)
     in (B.unpack head, Source{current=tail,rest,atEOF,loc=Loc{byteOffset=byteOffset+newOffset,lineNumber}})


regexMatch (RE re_str_raw) (s @ Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
     let re = BRE.mkRegexWithOpts (B.pack('^' : re_str_raw)) True True    -- append ^ to indicate we want to match at the beginning of the string.
     in case BRE.matchRegexAll re current of
        Nothing -> (Nothing, s)       -- match failed, return input unchanged
        Just (before,match,after,_) ->  -- we ignore subexpression matches.
           if not (B.null before) then (Nothing, s)   -- only looking for matches at the beginning of the string
           else  (Just (B.unpack match), Source{current= after,rest,atEOF,loc=Loc{byteOffset=byteOffset+(fromIntegral (B.length match)),lineNumber}})

regexStop (RE re_str_raw) (s @ Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
     let re = BRE.mkRegexWithOpts (B.pack re_str_raw) True True 
     in case BRE.matchRegexAll re current of
        Nothing -> (Nothing, s)       -- match failed, return input unchanged
        Just (before,match,after,_) ->  -- we ignore subexpression matches.
             (Just (B.unpack before), 
              Source{current= B.append match after,rest,atEOF,loc=Loc{byteOffset=byteOffset+(fromIntegral (B.length before)),lineNumber}})

span p (Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
     let (head, tail) = B.span p current
         newOffset    = byteOffset + (B.length head)
     in (B.unpack head, Source{current=tail,rest,atEOF,loc=Loc{byteOffset=byteOffset+newOffset,lineNumber}})

tail  (Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
       (Source{current=B.tail current,rest,atEOF,loc=Loc{byteOffset=byteOffset+1,lineNumber}})

getPos (Source {current, rest, atEOF, loc}) = 
   Pos {line=current, begin = loc, end = Nothing }
scanTo chr (src @ Source{current,rest,atEOF,loc=Loc{byteOffset,lineNumber}}) = 
     let (Pos {line,begin,..}) = getPos src
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

eqCurrent :: Source -> Source -> Bool
eqCurrent s s'= current s == current s'

recordBegin :: Source -> (Maybe String, Source)
recordBegin s = (Nothing, s)

recordEnd :: Source -> (Maybe String, Source)
recordEnd s = 
  if atEOF s then (Just "Found EOF when looking for EOR", s)
             else (Nothing, getNextRecord_newline s)
  