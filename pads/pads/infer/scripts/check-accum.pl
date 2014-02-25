#!/usr/bin/perl

($accumFile) = @ARGV;
open  (ACCUM, $accumFile) || die "Can't open $accumFile";
  while ($record = <ACCUM>) {
    if ($record =~ /good vals: *([0-9]+) *bad vals: *([0-9]+)/) {
     if ($2==0) {
	print "$accumFile ...... pass\n"
     }
     else {
	print "$accumFile ...... fail\n"
     }
     last;
    }
  }
close (ACCUM);
