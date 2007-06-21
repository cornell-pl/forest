#!/usr/bin/perl

@accumFiles = @ARGV;
$numfiles = $#accumFiles + 1;
$succrate=0;
$time = 0;
foreach my $accumFile  (@accumFiles)
{
  open  (ACCUM, $accumFile) || die "Can't open $accumFile";

  while ($record = <ACCUM>) {
    if ($record =~ /Total time = ([0-9.]+)/)
    {
#     printf ("$accumFile: total time = %.2f\n", $1);
     $time += $1;
    }
    elsif ($record =~ /good vals: *([0-9]+) *bad vals: *([0-9]+)/) {
     local ($good, $bad) = ($1, $2);
     $newrate = (100*$good)/($good+$bad);
     printf ("$accumFile: success rate = %.2f\%\n", $newrate);
     $succrate += $newrate;
#     print "\n\nnumber of correctly parsed records = $good\n";
#     print "number of incorrectly parsed records = $bad\n";
     last;
    }
  }
  close (ACCUM);
}
$succrate = $succrate /$numfiles;
$time = $time/$numfiles;
printf ("The average time = %.2f\n", $time);
printf ("The average success rate = %.2f\%\n", $succrate);
