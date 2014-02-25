structure YumTokens = struct
    open Tokens

    fun mkPhonyLoc ( ln : int ) : location = mkLoc 0 0 1 ln
    val sp1tok : LToken list = repeatLToken 329 (Pstring " ") 6 6
    val sp2tok : LToken list = repeatLToken 329 (Pstring " ") 15 15
    val colsptok : LToken list = repeatLToken 329 (Pstring ": ") 0 0
    val dottok : LToken list = repeatLToken 317 (Pstring ".") 0 0
    val dashtok : LToken list = repeatLToken 317 (Pstring "-") 0 0
    val eortok : LToken list = repeatLToken 10 (Pstring " ") 0 0
    val insupdsptok : LToken list = repeatLToken 317 (Pstring " ") 0 0
end
