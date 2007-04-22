#!/usr/bin/perl

$accumFile = $ARGV[0];
open  (ACCUM, $accumFile) || die "Can't open $accumFile";

while ($record = <ACCUM>) {
  if ($record =~ /good vals: *([0-9]+) *bad vals: *([0-9]+)/) {
     local ($good, $bad) = ($1, $2);
     print "overall report: good = $good, bad = $bad\n";
     last;
  }
}

close (ACCUM);
