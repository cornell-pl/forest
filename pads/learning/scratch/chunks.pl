#!/usr/bin/perl
#This script takes a data sample file and produce a number of randomly selected chunks of the file.
#These random parts can be used to form a population of input for the learning system
#Usage: chunks.pl [-h] [-m <random>|<contiguous>] [-s <size of chunk>] [-n <number of chunks to produce>] -f datafile
#Mode random picks random lines to form a chunk whereas mode contiguous picks contiguous lines to form a chunk

$DEFAULT_MODE = "random";
$DEFAULT_SIZE = 10;
$DEFAULT_NUM = 10;
sub printusage()
{
 print "\nUsage: chunks.pl [-h] [-m <random>|<contiguous>] [-s <size of chunk>] 
		[-n <number of chunks to produce>] -f datafile\n";
 print "Note: datafile must be a line-based record file.\n";
}

$arg=join (':', @ARGV);
if ($arg =~ /-h/ || $arg !~ /-f/)
{ printusage(); exit;}

if ($arg =~ /-f:([^:]*)/)
{
 $datafile = $1;
}
else { printusage(); exit;}

if ($arg =~ /-m:([^:]*)/)
{
 $mode = $1;
}
else {
 $mode = $DEFAULT_MODE;
}
if ($arg =~ /-s:([^:]*)/)
{
 $size = $1;
}
else {
 $size = $DEFAULT_SIZE;
}
if ($arg =~ /-n:([^:]*)/)
{
 $num = $1;
}
else {
 $num = $DEFAULT_NUM;
}

@lines = ();
open (FILE, "<$datafile") or die "Can't open data file for reading";
while (<FILE>)
{
 chomp;
 push @lines, $_;
}
close FILE;

$numrecs = $#lines+1;

if ($num*$size>$numrecs) 
{
 print "Input file size too small for given chunk size and number of chunks!\n";
 exit;
}

for ($i=0; $i<$num; $i++)
{
 $out = $datafile.".chunk".$i;
 open (OUT, ">$out") or die "Can't open $out for output!";
 if ($mode eq "random") #randomly pick $size lines from the input file
 {
  for ($j=0; $j<$size; $j++)
  {
   print OUT $lines[int(rand($numrecs))]."\n";
  }
  print "Written random chunk to $out.\n";
 }
 else #contiguous mode
 {
  $startline = int(rand($numrecs-$size+1));
  for ($j=0; $j<$size; $j++)
  {
   print OUT $lines[$startline+$j]."\n";
  }
  print "Written contiguous chunk to $out.\n";
 }
 close OUT
}
