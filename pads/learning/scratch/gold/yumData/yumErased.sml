structure YumErased = struct
    open Tokens

    fun mkPhonyLoc ( ln : int ) : location = mkLoc 0 0 1 ln
    val erasedTokens : LToken list =
    [
      ( Pstring "Erased", mkPhonyLoc 263 )
    , ( Pstring "Erased", mkPhonyLoc 264 )
    , ( Pstring "Erased", mkPhonyLoc 265 )
    , ( Pstring "Erased", mkPhonyLoc 266 )
    , ( Pstring "Erased", mkPhonyLoc 267 )
    , ( Pstring "Erased", mkPhonyLoc 268 )
    , ( Pstring "Erased", mkPhonyLoc 316 )
    , ( Pstring "Erased", mkPhonyLoc 317 )
    , ( Pstring "Erased", mkPhonyLoc 318 )
    , ( Pstring "Erased", mkPhonyLoc 319 )
    ]
    val erasedTokensFreq : int = 10

    val erasedpkgTokens : LToken list =
    [
      ( Pstring "ruby-tcltk", mkPhonyLoc 262)
    , ( Pstring "ruby-devel", mkPhonyLoc 263)
    , ( Pstring "ruby-libs", mkPhonyLoc 264)
    , ( Pstring "ruby-docs", mkPhonyLoc 265)
    , ( Pstring "ruby-libs", mkPhonyLoc 266)
    , ( Pstring "ruby-mode", mkPhonyLoc 267)
    , ( Pstring "ruby", mkPhonyLoc 268)
    , ( Pstring "dhcp-devel", mkPhonyLoc 316)
    , ( Pstring "dhcpv6_client", mkPhonyLoc 317)
    , ( Pstring "dhcpv6", mkPhonyLoc 318)
    , ( Pstring "dhcp", mkPhonyLoc 319)
    ]
    val erasedpkgTokensFreq : int = 11

end
