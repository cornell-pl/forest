fun eof () = NONE
fun getLoc (yypos, yytext) = {beginloc=yypos, endloc=yypos + size(yytext) -1}

fun getFirst s isBegin = 
    let val ss = Substring.full s
        val beginTrim = if isBegin then 1 else 2
        val stripped = Substring.trimr 1 (Substring.triml beginTrim ss) 
        val (lss, rss) =  Substring.splitl Char.isSpace stripped
        val result = if Substring.isEmpty lss then (Substring.string rss, "")
                     else (Substring.string lss, Substring.string rss)
    in
       result
    end
%%

%structure TokenLex

triplet = [0-9]{1,3};
ipAddr  = {triplet}\.{triplet}\.{triplet}\.{triplet};

doublet = [0-9]{1,2};
time    = {doublet}:{doublet}((:{doublet})?);

month   = Jan | jan | Feb | feb | Mar | mar | Apr | apr | May | may | Jun | jun | 
          Jul | jul | Aug | aug | Sep | sep | Oct | oct | Nov | nov | Dec | dec;

str     = [A-Za-z][A-Za-z0-9_\-]*;
xmlb    = \<([a-zA-Z])+\>;
oxmlb   = \<[^\>]+\>;
xmle    = \<\/[^\>]+\>;

%%

{ipAddr}  => (SOME (Tokens.Pip yytext,                    getLoc(yypos, yytext) ));
{xmlb}    => (SOME (Tokens.PbXML (getFirst yytext true),  getLoc(yypos, yytext) ));
{xmle}    => (SOME (Tokens.PeXML (getFirst yytext false), getLoc(yypos, yytext) ));
{time}    => (SOME (Tokens.Ptime yytext,                  getLoc(yypos, yytext) ));
{str}     => (SOME (Tokens.Pstring yytext,                getLoc(yypos, yytext) ));
-?[0-9]+  => (SOME (Tokens.Pint (Option.valOf(LargeInt.fromString yytext)), getLoc(yypos, yytext) ));
[ \t]+    => (SOME (Tokens.Pwhite yytext,                 getLoc(yypos, yytext) ));
.         => (SOME (Tokens.Other (String.sub(yytext,0)),  getLoc(yypos, yytext) )); 
\n        => (continue());
