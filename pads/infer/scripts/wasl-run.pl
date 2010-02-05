#!/usr/bin/perl
# usage: wasl-run.pl LARGE_FILES
# this script first creates a number of smaller files from
# the large file of size 200k, 400k, 600k, 800k and 1M lines each
# and then incrementally learn scriptions from these files using
# various settings of optimizations. The learn size is 100 and the
# chunk size is also 1000.
use Time::HiRes qw ( time alarm sleep );

@testfiles = (  "ai.3000", "asl.log",
#		"ai.big",
#		"pws", "interface.loop", 
#		"coblitz.access.20090618_000", 
#		"access.exlog.20090621_000",
#		"debug_getbig.20090624_000",
#		"dbg_req_redirect.20090624_000"
	     );
$numTimes = 3;
$timeout = 10;
$initsize = 500;
$incsize = 100;
$pads_home = `echo \$PADS_HOME`;
chomp $pads_home;
$arch = `$pads_home/ast-ast/bin/package.cvs`;
chomp $arch;

sub getFileName
{
 #getting the file name of the largefile
 my ($file) = @_;
 @parts = split(/\//, $file);
 return $parts[$#parts];
}

sub output
{
  my ($hdr, $tc, $adc, $score, $dist, $rate, $time, $rptime, $padstime, $blobtime, $try) = @_;
  if ($rptime>0) {my $rptime_s = sprintf("%.2f", $rptime);} else {my $rptime_s = "NA";}
  if ($padstime >0) {my $padstime_s = sprintf("%.2f", $padstime);} else {my $padstime_s = "NA";}
  if ($blobtime>0) {my $blobtime_s = sprintf("%.2f", $blobtime);} else {my $blobtime_s = "NA";}
  if ($dist>=0) {my $dist_s = sprintf("%.2f", $dist);} else {my $dist_s = "NA";}
  printf ("$hdr: lntime = %.2f  rptime = %s  padstime = %s  blobtime = %s  tc = %.2f  adc = %.2f  score = %.2f  dist = %s  ",  
		$time, $rptime_s, $padstime_s, $blobtime_s, $tc, $adc, $score, $dist_s);
  if ($try == 0) {
    printf("accuracy = %.2f\%\n", $rate);
  }
  else {printf("(try #%d)\n", $try);}
}

sub scan
{
  (my $file) = @_;
  my $score=0;
  my $time=0;
  my $rptime = 0;
  my $tc = 0;
  my $adc = 0;
  my $dist = -1;
  if (-e $file)
  { 
  open (FILE, "<$file") or die "Can't open $file!";
  while (<FILE>)
  {
   if (/Final comps = \(([0-9.]+)b, ([0-9.]+)b, ([0-9.]+)\)/) 
   {$tc = $1; $adc= $2; $score = $3;}
   elsif (/Total time = ([0-9.]+)/) 
   {$time = $1;}
   elsif (/Reparse time = ([0-9.]+)/)
   {$rptime = $1;}
   elsif (/.*ty2 = ([0-9.]+) nodes.*Edit distance = ([0-9.]+)/) {$dist = $2/$1;}
   elsif (/.*uncaught exception.*/)
    {$score = -1;
     last;
    }
  }
  return ($tc, $adc, $score, $dist, $time, $rptime)
  }
  else {
   print "$file doesn't exist!\n";
   {return (0, 0, 0, -1, 0, 0);}
  }
}

sub getGoldDist 
{
 (my $filename) = @_;
 my $goldxml = "gold/$arch/$filename.pxml";
 system ("cd gen; make $filename-parse>&/dev/null");
 if (-e $goldxml)
 {
   my $line = `descdist gen/$arch/$filename.pxml $goldxml`; 
   chomp $line;
   if ($line =~ /.*ty2 = ([0-9.]+) nodes.*Edit distance = ([0-9.]+)/) {return $2/$1;}
    else {return -1;}
 }
 else {return -1;}
}

#sub learn
#{
# (my $test) = @_;
## print "learn $test > $test.lrn\n";
# alarm $timeout;
# exec("learn $test > $test.output");
# exit(1);
#}

sub verify
{
 (my $name, my $datafile) = @_;
 my $badrate = 101.0;
 system ("cd gen; make $name-parse>&/dev/null");
 my $begintm = time;
 system ("gen/$arch/$name-parse $datafile >& $name.parse");
 my $elapse = time - $begintm;
 open (FILE, "<$name.parse") or die "Can't open $name.parse!";
 while (<FILE>)
 {
  if (/.*bad records =\s*([0-9]+).*/)
  {
   $bads = $1;
  }
  if (/.*total records =\s*([0-9]+).*/)
  {
   $total = $1;
   last;
  }
 }
 unlink "$name.parse";
 if (! -e "gold/$arch/blob-parse") {
  system ("cd gold; make blob-parse>&/dev/null");
 }
 $begintm = time;
 system ("gold/$arch/blob-parse $datafile >& /dev/null");
 my $blob_elapse = time - $begintm;

 return ((100.0 * ($total - $bads)/$total), $elapse, $blob_elapse);
}

sub inc
{
  (my $file, my $isize, my $lsize, my $doparse, my $adcw, my $uc) = @_;
  my $score = 0;
  my $time = 0;
  my $rptime = 0;
  my $exectime = 0;
  my $padstime = 0;
  my $blobtime = 0;
  my $num = 0;
  my $dist = -1;
  my $filename = getFileName $file;
  my $goldxml = "gold/$arch/$filename.pxml";
  if ($adcw >=0) {$adc_str = "-w $adcw";} else {$adc_str = ""}
  if ($uc >=0) {$uc_str = "-u $uc";} else {$uc_str = ""}

  for (my $j = 0; $j < $numTimes; $j++)
  {
    $exectime = 0;
    if ($doparse) {
      system ("increment -f $file -i $isize -l $lsize -output gen -reparse true $adc_str $uc_str > $filename.inc");
    } else
    {
      system ("increment -f $file -i $isize -l $lsize -output gen $adc_str $uc_str > $filename.inc");
    }
    #unlink("$filename.inc");
    if ($doparse) {
      ($rate, $ptime, $btime) = verify($filename, $file);
      if (-e $goldxml)
      {
	system ("descdist gen/$arch/$filename.pxml $goldxml >> $filename.inc");
      }
    }
    else {
	$rate = 0;
	$ptime = 0;
	$btime = 0;
    }
    ($tc, $adc, $score, $dist, $exectime, $reparsetime) = scan ("$filename.inc");
    if (!$score) {last;} #timeout reached
    $time += $exectime;
    $rptime += $reparsetime;
    $padstime += $ptime;
    $blobtime += $btime;
    $num++;
#    output("$filename (inc)", $score, $rate, $exectime, $reparsetime, $ptime, $btime, $num);
  }
  unlink("$filename.inc");
  if ($num > 0) {
    $time = $time/$num; 
    $rptime = $rptime/$num;
    $padstime = $padstime/$num;
    $blobtime = $blobtime/$num;
    return ($tc, $adc, $score, $dist, $rate, $time, $rptime, $padstime, $blobtime)
  }
  else {return (0, 0, 0, $dist, 0, 0, 0, 0, 0);}
}

if (!@ARGV) {
  print "Usage: wasl-run.pl init inc LARGE_FILES [-small] [-scale] [-incsize] 
                     [-single] [-noparse] [-w ADC_WEIGHT] [-u FLOAT]

LARGE_FILE must have at least 1K lines.
-small:   run tests on a set of small examples specified by \@testfiles in the script, 
          test files are stored in pads/infer/examples/data and they must be in the 
          currect working directory.
-scale init inc:   run the scaling tests on LARGE_FILE with init and inc sizes.
-incsize: run tests with various init learn size and incremental learn size on LARGE_FILE.
-single init inc: single run on the data file including learn, reparse and pads parse
-noparse: do not re-parse the data after learning the description.
-w ADC_WEIGHT: set the adc weight parameter.
-u FLOAT: set union clustering parameter\n";
  exit;
}
$initsize = shift @ARGV;
$incsize = shift @ARGV;

@largefiles = ();
$argscan = 1;
while ($argscan)
{
  my $myarg = shift @ARGV;
  if ($myarg =~ /^-.*/) {$argscan = 0; unshift (@ARGV, $myarg);}
  else {push (@largefiles, $myarg);}
}

$otherargs = join (' ', @ARGV);

print "wasl-run.pl Arguments: $otherargs\n";

if ($otherargs =~ /.*-noparse.*/) {
  $doparse=0;
}
else {$doparse=1;}
if ($otherargs =~ /.*-w ([0-9.]+).*/) {
  $adc_weight = $1;
} else 
{
  $adc_weight = -1;
}
if ($otherargs =~ /.*-u ([0-9.]+).*/) {
  $uc = $1;
} else 
{
  $uc = -1;
}

#Multiple smaller files tests
if ($otherargs =~ /.*-small.*/) {
  print "Begin comparison tests on small files\n";
  foreach $test (@testfiles) {
    $time = 0;
    $score = 0;
    $num = 0;
    $dist = -1;
    my $padstime = 0;
    my $blobtime = 0;
    my $rate = -1;
    for ($i=0; $i<$numTimes; $i++)
    {
     $exectime = 0;
     system ("learn $test > $test.lrn");
     ($tc, $adc, $score, $dist, $exectime, $rptime) = scan ("$test.lrn");
     unlink ("$test.lrn");
     $time += $exectime;
     #system("rm -f $test.output");
     if (!$score) {last;} #time out reached
     ($rate, $ptime, $btime) = verify($test, $test);
     $padstime+= $ptime;
     $blobtime+= $btime;
     printf ("$test (lrn): lntime = %.2f  padstime = %.2f  blobtime = %.2f  tc = %.2f  adc = %.2f  score = %.2f  (try #$num)\n", 
		$exectime, $ptime, $btime, $tc, $adc, $score);
     $num++;
    }
    if ($num > 0) {
      $time = $time/$num; #$num maybe less than $numTimes
      $padstime = $padstime/$num;
      $blobtime = $blobtime/$num;
      printf ("$test (lrn): lntime = %.2f  padstime = %.2f  blobtime = %.2f  tc = %.2f  adc = %.2f  score = %.2f  accuracy = %.2f\%\n", 
		$time, $padstime, $tc, $adc, $score, $rate);
    }
    else {
      print "$test (lrn): timed out\n";
    }
    ($tc, $adc, $score, $dist, $rate, $time, $rptime, $padstime) = 
	inc($test, $initsize, $incsize, $doparse, $adc_weight, $uc);
    output("$test (inc)", $tc, $adc, $score, $dist, $rate, $time, $rptime, $padstime, 0);
  }
  print "End comparison tests on small files\n";
}



foreach my $largefile (@largefiles)
{
# skip the switches in the arguments
if ($largefile =~ /^-.*/) {next;}

print "\n**** Processing $largefile! ****\n";

$beginwctm = time();
$wcoutput = `wc -l $largefile`;
$wc_elapse = time() - $beginwctm;
if ($wcoutput =~ /\s*(\d+).*/) {
 $largefile_lines = $1;
}
else {print "$largefile can't be found!\n"; exit}
if ($largefile_lines < 1000) {print "$largefile must be at least 1k lines!\n"; next}

$fname = getFileName($largefile);

print "wc -l elapse = $wc_elapse secs\n";

#Scaling tests... 
# we create 5 data points for the given large file
$step = int($largefile_lines/5);
if ($otherargs =~ /.*-scale.*/) {
  print "Begin scaling tests\n";
  for (my $i=1; $i<=4; $i++)
  {
    $size = $i * $step;
    system("head -n $size $largefile > $fname.$size");
    ($tc, $adc, $score, $dist, $rate, $time, $rptime, $padstime, $blobtime) = 
	inc ("$fname.$size", $initsize, $incsize, 0, $adc_weight, $uc);
    if ($score==0) {
        print "$fname.$size (init=$initsize, inc=$incsize): timed out\n";
	$timeout=1; last}
    elsif ($score==-1) {
        print "$fname.$size (init=$initsize, inc=$incsize): exception raised\n";
    }
    else {
      output("$fname.$size (init=$initsize, inc=$incsize)", $tc, $adc, $score, $dist, $rate, $time, $rptime, $padstime, $blobtime, 0);  
    }
    unlink ("$fname.$size");
  }
  #last round is the whole file itself...
    ($tc, $adc, $score, $dist, $rate, $time, $rptime, $padstime, $blobtime) = 
	inc ("$largefile", $initsize, $incsize, $doparse, $adc_weight, $uc);
    if ($score==0) {
        print "$fname.$largefile_lines (inc): timed out\n";
	$timeout=1; last}
    elsif ($score==-1) {
        print "$fname.$largefile_lines (inc): exception raised\n";
    }
    else {
    output("$fname.$largefile_lines (inc)", $tc, $adc, $score, $dist, $rate, $time, $rptime, $padstime, $blobtime, 0);
    }
  print "End scaling tests\n";
}

#init size and incremental size tests ...
#init size from 500 to max growing exponentially
#inc size from 50 to 300 with a step of 50
$timeout=0;
if ($otherargs =~ /.*-incsize.*/) {
  print "Begin init/incremental size tests\n";
  for (my $i = 500; $i <= $largefile_lines && !$timeout; $i=$i*2)
  {
   for (my $j = 50; $j <= 300; $j+=50)
   { 
    ($tc, $adc, $score, $dist, $rate, $time, $rptime, $padstime, $blobtime) = inc ($largefile, $i, $j, 0, $adc_weight, $uc);
    $dist = getGoldDist ($largefile);
    if ($score==0) {
        print "$fname (init=$i, inc=$j): timed out\n";
	$timeout=1; last}
    elsif ($score==-1) {
        print "$fname (init=$i, inc=$j): exception raised\n";
    }
    else {
        output("$fname (init=$i, inc=$j)", $tc, $adc, $score, $dist, $rate, $time, $rptime, $padstime, $blobtime, 0);
    }
   }
  }
  print "End init/incremental size tests\n";
}

if ($otherargs =~ /.*-single.*/) {
   print "Begin single test\n";
   ($tc, $adc, $score, $dist, $rate, $time, $rptime, $padstime, $blobtime) = inc ($largefile, $initsize, $incsize, $doparse, $adc_weight, $uc);
    if ($score==0) {
        print "$fname (init=$initsize, inc=$incsize): timed out\n";
	$timeout=1; last}
    elsif ($score==-1) {
        print "$fname (init=$initsize, inc=$incsize): exception raised\n";
    }
    else {
        output("$fname (init=$initsize, inc=$incsize)", $tc, $adc, $score, $dist, $rate, $time, $rptime, $padstime, $blobtime, 0);
    }
}
}

#TODO: shall we add a test for varying adc weight
# the problem is the weight seems to affect the quality of the output description
# from human perspective and can't be compared by looking at the score (since the
# weight is being varied). We could look at type complexity but the best complexity is
# the whole description being a Blob.
