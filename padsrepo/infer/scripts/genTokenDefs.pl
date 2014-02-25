#!/usr/bin/perl
sub printUsage()
{
 print "Usage: genTokenDefs.pl CONFIG_FILE OUTPUT_PATH\n
This is a program to generate the tokenDefs.sml, required by the increment program.\n";
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
$tokenfile = $outpath."/tokenDefs.sml";
print "Writing to $tokenfile\n";
open (FILE, "<$configpath") or die "Can't open config file for read!\n";
while (<FILE>)
{
 chomp;
 s/\s*$//;
 s/\s+\|\s+/\|/g;
 next if (/^#.*/);
 if (/^def\s*(\w+)\s*(.*)/)
 {
   $pairs{$1} = $2;
 }
 elsif (/^exp\s*(\w+)\s*(.*)/)
 {
  $pairs{$1} = $2;
  push (@exports, $1);
 }
}
close FILE;
open (TOKENFILE, ">$tokenfile") or die "Cannot open PADS tokenDefs.sml for write!\n";
print TOKENFILE 
"structure TokenDefs =
struct
open Tokens
val tokenDefList =\n";

@strs = (); 
foreach my $name (@exports)
{
 $re = escape (expand($name));
 if ($name eq "Pint") {
	push @strs, "( Pint (0, \"\"), \"[-~]?$re\" )";
 }
 elsif ($name eq "PbXML" || $name eq "PeXML") {}
 else {
   push @strs, "( $name \"\", \"$re\" )";
 }
}
push (@strs, "( Pfloat (\"\", \"\"), \"([-~]?([0-9]+))|[-~]?([0-9]+)\\\\.([0-9]+)|[-~]?[1-9]\\\\.([0-9]+)(E|e)[-~]?([0-9]+)\" )");

print TOKENFILE "[\n" . join (",\n", @strs) . "\n]\n\nend\n\n";
close TOKENFILE;
