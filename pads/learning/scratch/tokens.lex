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
filename = [^\\/?*:<>"]+;
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
nummonth = 0?[1-9] | 1[0-2];
genmonth = {month} | {nummonth};
domainsuffix = ac | ad | ae | aero | af | ag | ai | al | am | an | ao | aq | ar | arpa | as | at | au | aw | az | ba | bb | bd | be | bf | bg | bh | bi | biz | bj | bm | bn | bo | br | bs | bt | bv | bw | by | bz | ca | cc | cf | cg | ch | ci | ck | cl | cm | cn | co | com | coop | cr | cs | cu | cv | cx | cy | cz | de | dj | dk | dm | do | dz | ec | edu | ee | eg | eh | er | es | et | eu | fi | firm | fj | fk | fm | fo | fr | fx | ga | gb | gd | ge | gf | gh | gi | gl | gm | gn | gov | gp | gq | gr | gs | gt | gu | gw | gy | hk | hm | hn | hr | ht | hu | id | ie | il | in | info | int | io | iq | ir | is | it | jm | jo | jobs | jp | ke | kg | kh | ki | km | kn | kp | kr | kw | ky | kz | la | lb | lc | li | lk | lr | ls | lt | lu | lv | ly | ma | mc | md | mg | mh | mil | mk | ml | mm | mn | mo | mp | mq | mr | ms | mt | mu | museum | mv | mw | mx | my | mz | na | name | nato | nc | ne | net | nf | ng | ni | nl | no | nom | np | nr | nt | nu | nz | om | org | pa | pe | pf | pg | ph | pk | pl | pm | pn | pr | pro | pt | pw | py | qa | re | ro | ru | rw | sa | sb | sc | sd | se | sg | sh | si | sj | sk | sl | sm | sn | so | sr | st | store | su | sv | sy | sz | tc | td | tf | tg | th | tj | tk | tm | tn | to | tp | tr | travel | tt | tv | tw | tz | ua | ug | uk | um | us | uy | va | vc | ve | vg | vi | vn | vu | web | wf | ws | ye | yt | yu | za | zm | zr | zw; 

year = [0-2][0-9]{3};
date =  {genmonth}\/{day}\/{year} | {day}\/{genmonth}\/{year} | {year}\/{genmonth}\/{day} |
	{genmonth}\-{day}\-{year} | {day}\-{genmonth}\-{year} | {year}\-{genmonth}\-{day} |
	{genmonth}\.{day}\.{year} | {day}\.{genmonth}\.{year} | {year}\.{genmonth}\.{day} |
	({weekday},[ \t]+)?{month}[ \t]+{day}(,[ \t]+{year})? | 
	({weekday},[ \t]+)?{day}[ \t]+{month}(,[ \t]+{year})?;
str     = [A-Za-z][A-Za-z0-9_\-]*;
str1     = [0-9A-Za-z][A-Za-z0-9_\-]*;
query = [^&=]+=[^&]*(\&[^&=]+=[^&]*)*\&?;
hostname = ({str1}\.)+{domainsuffix};
url = http:\/\/{hostname}(:{port})?\/?({filepath})?(\?)?\&?{query}?(#{str1})? |
      http:\/\/{ipAddr}(:{port})?\/?({filepath})?(\?)?\&?{query}?(#{str1})? |
      {filepath}\?{query}(#{str1})?;
xmlb    = \<([a-zA-Z])+\>;
oxmlb   = \<[^>]+\>;
xmle    = \<\/[^>]+\>;

%%

{ipAddr}  => (SOME (Types.Pip yytext,                    getLoc(yypos, yytext) ));
{xmlb}    => (SOME (Types.PbXML (getFirst yytext true),  getLoc(yypos, yytext) ));
{xmle}    => (SOME (Types.PeXML (getFirst yytext false), getLoc(yypos, yytext) ));
{time}    => (SOME (Types.Ptime yytext,                  getLoc(yypos, yytext) ));
{date}    => (SOME (Types.Pdate yytext,                  getLoc(yypos, yytext) ));
{filepath}    => (SOME (Types.Ppath yytext,                  getLoc(yypos, yytext) ));
{url}    => (SOME (Types.Purl yytext,                  getLoc(yypos, yytext) ));
{hostname}    => (SOME (Types.Phostname yytext,                  getLoc(yypos, yytext) ));
{str}     => (SOME (Types.Pstring yytext,                getLoc(yypos, yytext) ));
-?[0-9]+  => (SOME (Types.Pint (Option.valOf(LargeInt.fromString yytext)), getLoc(yypos, yytext) ));
[ \t]+    => (SOME (Types.Pwhite yytext,                 getLoc(yypos, yytext) ));
.         => (SOME (Types.Other (String.sub(yytext,0)),  getLoc(yypos, yytext) )); 
\n        => (continue());
