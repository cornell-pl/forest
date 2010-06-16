module Text.Regex.ByteString (
    Regex,
    mkRegex,
    mkRegexWithOpts,
    matchRegex,
    matchRegexAll,
--    subRegex,
--    splitRegex
) where

import Data.Array((!))
import Data.Bits((.|.))
import Text.Regex.Base(RegexMaker(makeRegexOpts),defaultExecOpt,RegexLike(matchAll,matchAllText),RegexContext(matchM),MatchText)
import Text.Regex.Posix(Regex,compNewline,compIgnoreCase,compExtended)
import Data.ByteString.Lazy.Char8

mkRegex :: ByteString -> Regex
mkRegex s = makeRegexOpts opt defaultExecOpt s
  where opt = compExtended .|. compNewline

-- | Makes a regular expression, where the multi-line and
-- case-sensitve options can be changed from the default settings.

mkRegexWithOpts 
  :: ByteString  -- ^ The regular expression to compile
  -> Bool        -- ^ 'True' @\<=>@ @\'^\'@ and @\'$\'@ match the beginning and
                 -- end of individual lines, respecitively, and @\'.\'@ does /not/
                 -- match the newline character.
  -> Bool        -- ^ 'True' @\<=>@ matching is case sensitive
  -> Regex       -- ^ Returns: the compiled regular expression

mkRegexWithOpts s single_line case_sensitive 
  = let opt = (if single_line then (compNewline .|.) else id) .
              (if case_sensitive then id else (compIgnoreCase .|.)) $ 
              compExtended
    in makeRegexOpts opt defaultExecOpt s

-- | Match a regular expression against a bytestring
matchRegex
  :: Regex       -- ^ The regular expressions
  -> ByteString  -- ^ The Bytestring to match against
  -> Maybe [ByteString]  -- ^ Returns @'Just' strs@ if the match succeeded
                         -- (and @strs@ is the list of subexpression matches),
                         -- or 'Nothing' otherwise.
matchRegex p str = fmap (\(_,_,_,str) -> str) (matchRegexAll p str)

-- | Match a regular expression against a bytestring, returning more information
-- about the match.
matchRegexAll
   :: Regex             -- ^ The regular expression
   -> ByteString	-- ^ The bytestring to match against
   -> Maybe ( ByteString, ByteString, ByteString, [ByteString] )
		-- ^ Returns: 'Nothing' if the match failed, or:
		-- 
		-- >  Just ( everything before match,
		-- >         portion matched,
		-- >         everything after the match,
		-- >         subexpression matches )

matchRegexAll p str = matchM p str

{-
{- | Replaces every occurance of the given regexp with the replacement string.

In the replacement string, @\"\\1\"@ refers to the first substring;
@\"\\2\"@ to the second, etc; and @\"\\0\"@ to the entire match.
@\"\\\\\\\\\"@ will insert a literal backslash.

This does not advance if the regex matches an empty string.  This
misfeature is here to match the behavior of the the original
Text.Regex API.
-}

subRegex :: Regex                          -- ^ Search pattern
         -> ByteString                         -- ^ Input string
         -> ByteString                         -- ^ Replacement text
         -> ByteString                         -- ^ Output string
subRegex regexp inp repl =
 if Data.ByteString.length inp == 0 then Data.ByteString.empty
 else 
  let compile _i str [] = \ _m ->  (str++)
      compile i str (("\\",(off,len)):rest) =
        let i' = off+len
            pre =  Data.ByteString.take (off-i) str
            str' = Data.ByteString.drop (i'-i) str
        in if Data.ByteString.null str' then \ _m -> (pre ++) . ('\\':)
             else \  m -> (pre ++) . ('\\' :) . compile i' str' rest m
      compile i str ((xstr,(off,len)):rest) =
        let i' = off+len
            pre = Data.ByteString.take (off-i) str
            str' = Data.ByteString.drop (i'-i) str
            x = read xstr
        in if Data.ByteString.null str' then \ m -> (pre++) . ((fst (m!x))++)
             else \ m -> (pre++) . ((fst (m!x))++) . compile i' str' rest m
      compiled :: MatchText ByteString -> ByteString -> ByteString
      compiled = compile 0 repl findrefs where
        -- bre matches a backslash then capture either a backslash or some digits
        bre = mkRegex "\\\\(\\\\|[0-9]+)"
        findrefs = Data.ByteString.map (\m -> (fst (m!1),snd (m!0))) (matchAllText bre repl)
      go _i str [] = str
      go i str (m:ms) =
        let (_,(off,len)) = m!0
            i' = off+len
            pre = Data.ByteString.take (off-i) str
            str' = Data.ByteString.drop (i'-i) str
        in if Data.ByteString.null str' then pre ++ (compiled m "")
             else pre ++ (compiled m (go i' str' ms))
  in go 0 inp (matchAllText regexp inp)
-}

{- | Splits a string based on a regular expression.  The regular expression
should identify one delimiter.

This does not advance and produces an infinite list of [] if the regex
matches an empty string.  This misfeature is here to match the
behavior of the the original Text.Regex API.


splitRegex :: Regex -> ByteString -> [ByteString]
splitRegex delim strIn = 
 if Data.ByteString.length strIn == 0 then [] 
 else
  let matches = Data.ByteString.map (!0) (matchAll delim strIn)
      go _i str [] = str : []
      go i str ((off,len):rest) =
        let i' = off+len
            firstline = Data.ByteString.take (off-i) str
            remainder = Data.ByteString.drop (i'-i) str
        in seq i' $
           if Data.ByteString.null remainder then [firstline,Data.ByteString.empty]
             else firstline : go i' remainder rest
  in go 0 strIn matches
-}