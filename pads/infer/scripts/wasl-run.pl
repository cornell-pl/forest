#!/usr/bin/perl
# usage: wasl-run.pl LARGE_FILE
# this script first creates a number of smaller files from
# the large file of size 200k, 400k, 600k, 800k and 1M lines each
# and then incrementally learn scriptions from these files using
# various settings of optimizations. The learn size is 100 and the
# chunk size is also 1000.

($filename) = @ARGV;
if (!$filename) {
  print "Usage: wasl-run.pl LARGE_FILE_NAME\n";
  exit;
}
for ($i=200000; $i<=1000000; $i+=200000)
{
  system("head -n $i $filename > $filename.$i");
  print "\nLearning $filename.$i: opt_level = 0\n";
  system ("increment $filename.$i 500 100 0");
  print "\nLearning $filename.$i: opt_level = 1\n";
  system ("increment $filename.$i 500 100 1");
  print "\nLearning $filename.$i: opt_level = 2\n";
  system ("increment $filename.$i 500 100 2");
  print "\nLearning $filename.$i: opt_level = 3\n";
  system ("increment $filename.$i 500 100 3");
}

