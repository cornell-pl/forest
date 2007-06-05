#!/usr/bin/perl

@accumFiles = @ARGV;
$numfiles = $#accumFiles + 1;
$errrate=0;
foreach my $accumFile  (@accumFiles)
{
  open  (ACCUM, $accumFile) || die "Can't open $accumFile";

  while ($record = <ACCUM>) {
    if ($record =~ /good vals: *([0-9]+) *bad vals: *([0-9]+)/) {
     local ($good, $bad) = ($1, $2);
     $newrate = $bad/($good+$bad);
     print "$accumFile: error rate = $newrate\n";
     $errrate += $newrate;
#     print "\n\nnumber of correctly parsed records = $good\n";
#     print "number of incorrectly parsed records = $bad\n";
     last;
    }
  }
  close (ACCUM);
}
$errrate = $errrate /$numfiles;
print "The average error rate = $errrate \n";
