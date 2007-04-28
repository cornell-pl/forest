#!/usr/bin/perl
sub printUsage()
{
 print "Usage: config.pl CONFIG_FILE\n";
}

%pairs = ();
@exports = ();

sub escape 
{
 ($str) = @_;
 $str =~ s{\\}{\\\\}g;
# $str =~ s{\/}{\\\/}g;
 $str =~ s{"}{\\"}g;
 return $str
}

sub expand1
{
 ($str) = @_;
 if ($str =~ /(.*?)\{([A-Za-z]\w*)\}(.*)/) 
 {
   local $pre = $1;
   local $name = $2;
   local $rest = $3;
   if ($pairs{$name} ne "") 
   {
    local $expanded1 = expand($name);
    local $expanded2 = expand1($rest);
    return "$pre($expanded1)$expanded2";
   }
   else {return "$pre{$name}".expand($rest);}
 }
 else {
   return $str;
 }
}

sub expand 
{
 local ($name) = @_;
 if ($pairs{$name} ne "") 
 {
  expand1 ($pairs{$name});
 }
}

($configfile) = @ARGV;
if (!$configfile) {
  printUsage();
  exit;
}

($prefix, $suffix)  = split (/\./, $configfile);
$structurename = ucfirst($prefix."Lex");
$lexfile = $prefix.".lex";
$includefile = $prefix.".p";
print "Writing to $lexfile and $includefile\n";
open (FILE, "<$configfile") or die "Can't open config file for read!\n";
open (LEX, ">$lexfile") or die "Cannot open lex file $lexfile for write!\n";
print LEX 
"fun eof () = NONE
fun getLoc (yypos, yytext) = {beginloc=yypos, endloc=yypos + size(yytext) -1}

fun getFirst s isBegin = 
    let val ss = Substring.full s
        val beginTrim = if isBegin then 1 else 2
        val stripped = Substring.trimr 1 (Substring.triml beginTrim ss) 
        val (lss, rss) =  Substring.splitl Char.isSpace stripped
        val result = if Substring.isEmpty lss then (Substring.string rss, \"\")
                     else (Substring.string lss, Substring.string rss)
    in
       result
    end

%%

%structure $structurename

";

while (<FILE>)
{
 chomp;
 s/\s*$//;
 s/\s+\|\s+/\|/g;
 next if (/^#.*/);
 if (/^def\s*(\w+)\s*(.*)/)
 {
   $pairs{$1} = $2;
   print LEX "$1 = $2;\n"
 }
 elsif (/^exp\s*(\w+)\s*(.*)/)
 {
  $pairs{$1} = $2;
  print LEX "$1 = $2;\n";
  push (@exports, $1);
 }
}
print LEX "\n%%\n\n";
foreach $name (@exports)
{
 if ($name eq "PbXML") {
   print LEX "{$name}\t=> (SOME (Types.$name (getFirst yytext true), getLoc(yypos, yytext) ));\n"
 }
 elsif ($name eq "PeXML") {
   print LEX "{$name}\t=> (SOME (Types.$name (getFirst yytext false), getLoc(yypos, yytext) ));\n"
 }
 else {	
   print LEX "{$name}\t=> (SOME (Types.$name yytext, getLoc(yypos, yytext) ));\n"
 }
}
 print LEX "
-?[0-9]{1,9}  => (SOME (Types.Pint (Option.valOf(LargeInt.fromString yytext), yytext), getLoc(yypos, yytext) ));
[A-Za-z0-9][A-Za-z0-9_\\-]* => (SOME (Types.Pstring yytext,                getLoc(yypos, yytext) ));
.         => (SOME (Types.Other (String.sub(yytext,0)),  getLoc(yypos, yytext) )); 
\\n        => (continue());
";
close LEX;

open (INCLUDE, ">$includefile") or die "Cannot open PADS include file $includefile for write!\n";
foreach my $name (@exports)
{
 $re = escape (expand($name));
 print INCLUDE "Ptypedef Pstring_ME(:\"/$re/\":) P$name;\n\n";
}
print INCLUDE "Ptypedef Pstring_ME(:\"/[A-Za-z0-9][A-Za-z0-9_\\\\-]*/\":) PPstring;\n\n";
close INCLUDE;

