module Text.Regex.ByteString (
    Regex,
    mkRegex,
    mkRegexS,
    mkRegexWithOpts,
    mkRegexWithOptsS,
    matchRegex,
    matchRegexS,
    matchRegexAll,
    matchRegexAllS,
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

mkRegexS :: String -> Regex
mkRegexS s = mkRegex (pack s)

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

mkRegexWithOptsS 
  :: String      -- ^ The regular expression to compile
  -> Bool        -- ^ 'True' @\<=>@ @\'^\'@ and @\'$\'@ match the beginning and
                 -- end of individual lines, respecitively, and @\'.\'@ does /not/
                 -- match the newline character.
  -> Bool        -- ^ 'True' @\<=>@ matching is case sensitive
  -> Regex       -- ^ Returns: the compiled regular expression

mkRegexWithOptsS s = mkRegexWithOpts (pack s) 

-- | Match a regular expression against a bytestring
matchRegex
  :: Regex       -- ^ The regular expressions
  -> ByteString  -- ^ The Bytestring to match against
  -> Maybe [ByteString]  -- ^ Returns @'Just' strs@ if the match succeeded
                         -- (and @strs@ is the list of subexpression matches),
                         -- or 'Nothing' otherwise.
matchRegex p str = fmap (\(_,_,_,str) -> str) (matchRegexAll p str)

-- | Match a regular expression against a bytestring
matchRegexS
  :: Regex       -- ^ The regular expressions
  -> String  -- ^ The Bytestring to match against
  -> Maybe [String]  -- ^ Returns @'Just' strs@ if the match succeeded
                         -- (and @strs@ is the list of subexpression matches),
                         -- or 'Nothing' otherwise.
matchRegexS p str = fmap (\(_,_,_,str) -> str) (matchRegexAllS p str)

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


matchRegexAllS
   :: Regex             -- ^ The regular expression
   -> String	        -- ^ The string to match against
   -> Maybe ( String, String, String, [String] )
		-- ^ Returns: 'Nothing' if the match failed, or:
		-- 
		-- >  Just ( everything before match,
		-- >         portion matched,
		-- >         everything after the match,
		-- >         subexpression matches )

matchRegexAllS p str = fmap unp (matchM p (pack str))
  where unp (b1,b2,b3,bs) = (unpack b1, unpack b2, unpack b3, Prelude.map unpack bs)
