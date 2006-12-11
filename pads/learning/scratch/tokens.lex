fun eof() = NONE

%%

%structure TokenLex

triplet = [0-9]{1,3};
ipAddr  = {triplet}\.{triplet}\.{triplet}\.{triplet};

doublet = [0-9]{1,2};
time    = {doublet}:{doublet}((:{doublet})?);

month   = Jan | jan | Feb | feb | Mar | mar | Apr | apr | May | may | Jun | jun | 
          Jul | jul | Aug | aug | Sep | sep | Oct | oct | Nov | nov | Dec | dec;

str     = [A-Za-z][A-Za-z0-9_\-]*;

%%

{ipAddr}  => (SOME (Tokens.Pip yytext, {offset=yypos, span=size(yytext) - 1}));

{time}    => (SOME (Tokens.Ptime yytext, {offset=yypos, span=size(yytext) - 1}));
{str}     => (SOME (Tokens.Pstring yytext, {offset=yypos, span=size(yytext) - 1}));
-?[0-9]+  => (SOME (Tokens.Pint (Option.valOf(LargeInt.fromString yytext)), {offset=yypos, span=size(yytext) - 1}));
[ \t]+    => (SOME (Tokens.Pwhite yytext, {offset=yypos, span=size(yytext) - 1}));
.         => (SOME (Tokens.Other (String.sub(yytext,0)), {offset=yypos, span=size(yytext) - 1}));
\n        => (continue());
