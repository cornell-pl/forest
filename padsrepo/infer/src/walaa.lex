type lexresult = (Tokens.Token * {beginloc:int, endloc:int}) option
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

%structure WalaaLex

triplet = [0-9]{1,3};
doublet = [0-9]{2};
hexdoublet = [0-9a-fA-F]{2};
timezone = [+-][0-1][0-9]00;
ampm = AM|PM;
port = [1-9][0-9]*;
day = [1-9]|[1-2][0-9]|0[1-9]|3[0-1];
weekday = Mon|Monday|Tue|Tuesday|Wed|Wednesday|Thu|Thursday|Fri|Friday|Sat|Saturday|Sun|Sunday|mon|tue|wed|thu|fri|sat|sun;
month = Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December;
nummonth = 0?[1-9]|1[0-2];
genmonth = {month}|{nummonth};
domainsuffix = com|net|edu|org|gov|co|ac|mil|info|int;
year = [0-2][0-9]{3};
quote = ["'];
str = [A-Za-z][A-Za-z0-9]*;
str1 = [0-9A-Za-z][A-Za-z0-9]*;
query = {str1}=[0-9a-zA-Z_\-%;.+/:]*(\&{str1}=[0-9a-zA-Z_\-%;.+/:]*)*\&?;
username = [0-9a-zA-Z\-_+][0-9a-zA-Z\-_+.]*;
sysname = ({str1}\.)+{str1};
protocol = http|ftp|https;
safeurlchar = [^<>{}|\^\[\] \t\n\r"()'];
Ptime = {doublet}:{doublet}:{doublet}([ ]*{ampm})?([ \t]+{timezone})?;
Pip = {triplet}\.{triplet}\.{triplet}\.{triplet};
Pemail = {username}@{sysname};
Pmac = ({hexdoublet}(:|\-)){5}{hexdoublet};
Pdate = {genmonth}\/{day}\/{year}|{day}\/{genmonth}\/{year}|{year}\/{genmonth}\/{day}|{genmonth}\-{day}\-{year}|{day}\-{genmonth}\-{year}|{year}\-{genmonth}\-{day}|{genmonth}\.{day}\.{year}|{day}\.{genmonth}\.{year}|{year}\.{genmonth}\.{day}|({weekday},?[ \t]+)?{month}[ \t]+{day}(,[ \t]+{year})?|({weekday},?[ \t]+)?{day}[ \t]+{month}(,[ \t]+{year})?;
PbXML = \<([a-zA-Z])+\>;
oxmlb = \<[^>]+\>;
PeXML = \<\/[^>]+\>;
Pwhite = [ \t\r\n]+;
Pint = [0-9]+;
Pstring = {str};

%%

{Ptime}	=> (SOME (Types.Ptime yytext, getLoc(yypos, yytext) ));
{Pip}	=> (SOME (Types.Pip yytext, getLoc(yypos, yytext) ));
{Pemail}	=> (SOME (Types.Pemail yytext, getLoc(yypos, yytext) ));
{Pmac}	=> (SOME (Types.Pmac yytext, getLoc(yypos, yytext) ));
{Pdate}	=> (SOME (Types.Pdate yytext, getLoc(yypos, yytext) ));
{PbXML}	=> (SOME (Types.PbXML (getFirst yytext true), getLoc(yypos, yytext) ));
{PeXML}	=> (SOME (Types.PeXML (getFirst yytext false), getLoc(yypos, yytext) ));
{Pwhite}	=> (SOME (Types.Pwhite yytext, getLoc(yypos, yytext) ));
{Pint}	=> (SOME (Types.Pint (Option.valOf(LargeInt.fromString yytext), yytext), getLoc(yypos, yytext) ));
{Pstring}	=> (SOME (Types.Pstring yytext, getLoc(yypos, yytext) ));

.         => (SOME (Types.Other (String.sub(yytext,0)),  getLoc(yypos, yytext) )); 
\n        => (continue());
