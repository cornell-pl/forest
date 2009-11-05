#!/usr/bin/perl
sub printUsage()
{
 print "Usage: config.pl CONFIG_FILE OUTPUT_PATH\n";
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

sub mod_left_paren
{
 local ($re) = @_;
 local $output_re = "";
 local $in_char_class = 0;
 local $num_bs = 0;
 for ($i = 0; $i<length $re; $i++)
 {
  $ch = substr $re, $i, 1;
  if ($ch eq "[" && $num_bs % 2 == 0)
  {
   $in_char_class = 1;
   $num_bs = 0;
   $output_re = $output_re . $ch;
  } 
  elsif ($ch eq "]" && $num_bs % 2 == 0)
  {
   $in_char_class = 0;
   $num_bs = 0;
   $output_re = $output_re . $ch;
  } 
  elsif ($ch eq "\\") 
  {
   $num_bs++;
   $output_re = $output_re . $ch;
  }
  elsif ($ch eq "(" && $in_char_class==0 && $num_bs %2 == 0)
  {
   $num_bs =0;
   $output_re = $output_re . "(?:";
  }
  else
  {
   $num_bs =0;
   $output_re = $output_re . $ch;
  }
  # print ("char=" . $ch . "  in_char_class=" . $in_char_class . "   num_bs=" . $num_bs . "\n") 
 }
 # print ("The output re = " . $output_re . "\n");
 return $output_re;   
}


($configpath, $outpath) = @ARGV;
if (!$configpath) {
  printUsage();
  exit;
}
if (!$outpath) {$outpath = ".";}
if ($configpath =~ /.*\/(.*)\.config/)
{
 $prefix = $1
}
else {
($prefix, $suffix)  = split (/\./, $configpath);
}
$structurename = ucfirst($prefix."Lex");
$lexfile = $outpath."/".$prefix.".lex";
$includefile = $outpath."/".$prefix.".p";
print "Writing to $lexfile and $includefile\n";
open (FILE, "<$configpath") or die "Can't open config file for read!\n";
open (LEX, ">$lexfile") or die "Cannot open lex file $lexfile for write!\n";
print LEX 
"type lexresult = (Tokens.Token * {beginloc:int, endloc:int}) option
fun eof () = NONE
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
 elsif ($name eq "Pint") {
   print LEX "{$name}\t=> (SOME (Types.$name (Option.valOf(LargeInt.fromString yytext), yytext), getLoc(yypos, yytext) ));\n"
 }	
 else {	
   print LEX "{$name}\t=> (SOME (Types.$name yytext, getLoc(yypos, yytext) ));\n"
 }
}
 print LEX "
.         => (SOME (Types.Other (String.sub(yytext,0)),  getLoc(yypos, yytext) )); 
\\n        => (continue());
";
close LEX;

open (INCLUDE, ">$includefile") or die "Cannot open PADS include file $includefile for write!\n";
foreach my $name (@exports)
{
 $orig_re = expand($name);
 $re = escape (mod_left_paren($orig_re));
 print INCLUDE "Ptypedef Pstring_ME(:\"/$re/\":) P$name;\n\n";
}
#Finally add definition for PPchar
print INCLUDE "Ptypedef Pstring_ME(:\"/[^0-9A-Za-z \\\\t\\\\r\\\\n]/\":) PPchar;\n\n";
close INCLUDE;
