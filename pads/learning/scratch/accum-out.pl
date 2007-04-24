#!/usr/bin/perl

$accumFile = $ARGV[0];
open  (ACCUM, $accumFile) || die "Can't open $accumFile";

while ($record = <ACCUM>) {
  if ($record =~ /good vals: *([0-9]+) *bad vals: *([0-9]+)/) {
     local ($good, $bad) = ($1, $2);
     print "\n\nnumber of correctly parsed records = $good\n";
     print "number of incorrectly parsed records = $bad\n";
     last;
  }
}

close (ACCUM);
