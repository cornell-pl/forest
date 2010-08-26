module Examples.RegExpTest where

import qualified Text.Regex as RE

re = RE.mkRegexWithOpts "^a+" True True
re_results1 = RE.matchRegexAll re "aaaab"

