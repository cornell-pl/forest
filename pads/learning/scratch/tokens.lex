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
timezone = [+-][0-1][0-9]00;
ampm = am | AM | pm | PM;
time    = {doublet}:{doublet}((:{doublet})?)(([ ]*{ampm})?)([ \t]+{timezone})?;
port = [1-9][0-9]*;
filename = [^\\\/?*:<>\"]+;
filepath =  \/{filename}(\/{filename})*\/? | 
	([a-zA-Z]:)?\\{filename}(\\{filename})*\\? |
	\\\\{filename}(\\{filename})*\\?;
day	= [1-9] | [1-2][0-9] | 0[1-9] | 3[0-1];
weekday = Mon | Monday | Tue | Tuesday | Wed | Wednesday | Thu | Thursday | Fri | Friday | 
	  Sat | Saturday | Sun | Sunday | mon | tue | wed | thu | fri | sat | sun;
month   = Jan | jan | Feb | feb | Mar | mar | Apr | apr | May | may | Jun | jun | 
          Jul | jul | Aug | aug | Sep | sep | Oct | oct | Nov | nov | Dec | dec |
	  January | February | March | April | May | June | July | August | September |
	  October | November | December;
year = [0-2][0-9]{3};
date =  {month}\/{day}\/{year} | {day}\/{month}\/{year} | {year}\/{month}\/{day} |
	{month}\-{day}\-{year} | {day}\-{month}\-{year} | {year}\-{month}\-{day} |
	({weekday},[ \t]+)?{month}[ \t]+{day}(,[ \t]+{year})? | 
	({weekday},[ \t]+)?{day}[ \t]+{month}(,[ \t]+{year})?;
str     = [A-Za-z][A-Za-z0-9_\-]*;
str1     = [0-9A-Za-z][A-Za-z0-9_\-]*;
query = [^\&=]+=[^\&]*(\&[^\&=]+=[^\&]*)*;
url = http:\/\/{str1}(\.{str1})*(:{port})?\/?({filepath})?(\?)?\&?{query}?(#{str1})? |
	{filepath}\?{query}(#{str1})?;
xmlb    = \<([a-zA-Z])+\>;
oxmlb   = \<[^\>]+\>;
xmle    = \<\/[^\>]+\>;

%%

{ipAddr}  => (SOME (Types.Pip yytext,                    getLoc(yypos, yytext) ));
{xmlb}    => (SOME (Types.PbXML (getFirst yytext true),  getLoc(yypos, yytext) ));
{xmle}    => (SOME (Types.PeXML (getFirst yytext false), getLoc(yypos, yytext) ));
{time}    => (SOME (Types.Ptime yytext,                  getLoc(yypos, yytext) ));
{date}    => (SOME (Types.Pdate yytext,                  getLoc(yypos, yytext) ));
{filepath}    => (SOME (Types.Ppath yytext,                  getLoc(yypos, yytext) ));
{url}    => (SOME (Types.Purl yytext,                  getLoc(yypos, yytext) ));
{str}     => (SOME (Types.Pstring yytext,                getLoc(yypos, yytext) ));
-?[0-9]+  => (SOME (Types.Pint (Option.valOf(LargeInt.fromString yytext)), getLoc(yypos, yytext) ));
[ \t]+    => (SOME (Types.Pwhite yytext,                 getLoc(yypos, yytext) ));
.         => (SOME (Types.Other (String.sub(yytext,0)),  getLoc(yypos, yytext) )); 
\n        => (continue());
