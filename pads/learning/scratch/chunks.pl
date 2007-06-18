#!/usr/bin/perl

##########################################################################################
#
# This script takes a data sample file and produce a number of
# randomly selected chunks of the file.
# These random parts can be used to form a population
# of input for the learning system
# Usage: chunks.pl [-h] [-m <random>|<contiguous>] [-s <size of chunk>] [-n <number of chunks to produce>] [-p <percentage>] -f datafile
#
# Mode random picks random lines to form a chunk whereas mode
# contiguous picks contiguous lines to form a chunk
#
# Notes:
#    Add percentage (-p <percentage>)
#    Get number of lines in file (linecnt)
#    For -n <n>, compute ( linecnt / n )
#
##########################################################################################

$DEFAULT_MODE    = "random";
$DEFAULT_SIZE    = 10;
$DEFAULT_NUM     = 1;
$DEFAULT_PERCENT = 0;

sub printusage()
{
 print "\nUsage: chunks.pl [-h] [-m <random>|<contiguous>] [-s <size of chunk>] 
		[-n <number of chunks to produce>]  [-p <percentage>] -f datafile\n";
 print "Note: datafile must be a line-based record file.\n";
}

$arg=join (':', @ARGV);
if ($arg =~ /-h/ || $arg !~ /-f/)
{ printusage(); exit;}

# Parse out the name of the datafile
if ($arg =~ /-f:([^:]*)/)
{
 $datafile = $1;
}
else { printusage(); exit;}

# Parse out the mode parameter
if ($arg =~ /-m:([^:]*)/)
{
 $mode = $1;
}
else {
 $mode = $DEFAULT_MODE;
}

# Parse out the size parameter
if ($arg =~ /-s:([^:]*)/)
{
 $size = $1;
}
else {
 $size = $DEFAULT_SIZE;
}

# Parse out the number of records parameter
$num = $DEFAULT_NUM;
if ($arg =~ /-n:([^:]*)/) { $num = $1 }

# Parse out the percentage parameter
$percent = $DEFAULT_PERCENT;
if ($arg =~ /-p:([^:]*)/) { $percent = $1 };
if ( ($arg =~ /-s:([^:]*)/) && ($arg =~ /-p:([^:]*)/) ) { print "-p overides -s\n" }

# Validate the percentage number
die "Negative percentage ($percent)\n" if ($percent < 0 );
die "Percentage too big ($percent)\n" if ($percent > 100 );

# Slurp the data file
open(INF,$datafile) or die "Can't open $datafile for reading\n";
@lines = <INF>;  #Read the file into an array
chomp(@lines);
close(INF);

# Calculate the number of lines in the file
$numrecs = $#lines+1;

# If percentage was specified, recalculate the size
if ( $percent != 0 ) { $size = int ( $numrecs * $percent / 100 ) }

print "numrecs = $numrecs, num = $num, size = $size\n";

die "Input file size too small for given chunk size and number of chunks!\n" if ($num*$size>$numrecs);

for ( $i=0; $i < $num; $i++ )
{
 $out = $datafile.".chunk".$i;
 open (OUT, ">$out") or die "Can't open $out for output!";
 if ($mode eq "random") #randomly pick $size lines from the input file
 #notice that random records have to be unique
 {
  local %usedindices=();
  for ($j=0; $j<$size; $j++)
  {
   do {
     $newindex=int(rand($numrecs));
   }
   while ($usedindices{$newindex}); 
   $usedindices{$newindex}=1;
   print OUT $lines[$newindex]."\n";
  }
  print "Written random chunk to $out\n";
 }
 else #contiguous mode
 {
  $startline = int(rand($numrecs-$size+1));
  for ($j=0; $j<$size; $j++)
  {
   print OUT $lines[$startline+$j]."\n";
  }
  print "Written contiguous chunk to $out\n";
 }
 close OUT
}
