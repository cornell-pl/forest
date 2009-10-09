#!/usr/bin/perl
# usage: wasl-run.pl LARGE_FILE
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
  ($hdr, $score, $rate, $time, $rptime, $padstime, $try) = @_;
  printf ("$hdr: lntime = %.2f  rptime = %.2f  padstime = %.2f  score = %.2f  ",  
		$time, $rptime, $padstime, $score);
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
  if (-e $file)
  { 
  open (FILE, "<$file") or die "Can't open $file!";
  while (<FILE>)
  {
   if (/Final comps = \(([0-9.]+)b, ([0-9.]+)b, ([0-9.]+)/) 
   {$score = $3;}
   elsif (/Total time = ([0-9.]+)/) 
   {$time = $1;}
   elsif (/Reparse time = ([0-9.]+)/)
   {$rptime = $1;}
  }
  return ($score, $time, $rptime)
  }
  else {
   print "$file doesn't exist!\n";
   {return (0, 0, 0);}
  }
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
 return ((100.0 * ($total - $bads)/$total), $elapse);
}

sub inc
{
  (my $file, my $isize, my $lsize) = @_;
  my $score = 0;
  my $time = 0;
  my $rptime = 0;
  my $exectime = 0;
  my $padstime = 0;
  my $num = 0;
  my $filename = getFileName $file;

  for (my $j = 0; $j < $numTimes; $j++)
  {
    #print "Learning $file: opt_level = 3\n";
    $exectime = 0;
    system ("increment -f $file -i $isize -l $lsize -output gen -reparse true > $filename.inc");
    ($score, $exectime, $reparsetime) = scan ("$filename.inc");
    unlink("$filename.inc");
    if (!$score) {last;} #timeout reached
    ($rate, $ptime) = verify($filename, $file);
    $time += $exectime;
    $rptime += $reparsetime;
    $padstime += $ptime;
    $num++;
    output("$filename (inc)", $score, $rate, $exectime, $reparsetime, $ptime, $num);
  }
  if ($num > 0) {
    $time = $time/$num; 
    $rptime = $rptime/$num;
    $padstime = $padstime/$num;
    return ($score, $rate, $time, $rptime, $padstime)
  }
  else {return (0, 0, 0, 0, 0);}
}


$largefile = shift @ARGV;
if (!$largefile) {
  print "Usage: wasl-run.pl LARGE_FILE [-small] [-scale] [-incsize]

LARGE_FILE must have at least 1K lines.
-small:   run tests on a set of small examples specified by \@testfiles in the script, 
          test files are stored in pads/infer/examples/data and they must be in the 
          currect working directory.
-scale:   run the scaling tests on LARGE_FILE.
-incsize: run tests with various init learn size and incremental learn size on LARGE_FILE.\n";
  exit;
}

$otherargs = join (' ', @ARGV);

$wcoutput = `wc -l $largefile`;
if ($wcoutput =~ /\s*(\d+).*/) {
 $largefile_lines = $1;
}
else {print "$largefile can't be found!\n"; exit}
if ($largefile_lines < 1000) {print "$largefile must be at least 1k lines!\n"; exit}

#Multiple smaller files tests
if ($otherargs =~ /.*-small.*/) {
  print "Begin comparison tests on small files\n";
  foreach $test (@testfiles) {
    $time = 0;
    $score = 0;
    $num = 0;
    my $padstime = 0;
    my $rate = -1;
    for ($i=0; $i<$numTimes; $i++)
    {
     $exectime = 0;
     system ("learn $test > $test.lrn");
     ($score, $exectime) = scan ("$test.lrn");
     unlink ("$test.lrn");
     $time += $exectime;
     #system("rm -f $test.output");
     if (!$score) {last;} #time out reached
     ($rate, $ptime) = verify($test, $test);
     $padstime+= $ptime;
     printf ("$test (lrn): lntime = %.2f  padstime = %.2f  score = %.2f  (try #$num)\n", $exectime, $ptime, $score);
     $num++;
    }
    if ($num > 0) {
      $time = $time/$num; #$num maybe less than $numTimes
      $padstime = $padstime/$num;
      printf ("$test (lrn): lntime = %.2f  padstime = %.2f  score = %.2f  accuracy = %.2f\%\n", 
		$time, $padstime, $score, $rate);
    }
    else {
      print "$test (lrn): timed out\n";
    }
    ($score, $rate, $time, $rptime, $padstime) = inc($test, $initsize, $incsize);
    output("$test (inc)", $score, $rate, $time, $rptime, $padstime, 0);
  }
  print "End comparison tests on small files\n";
}


$fname = getFileName($largefile);

#Scaling tests... 
# we create 10 data points for the given large file
$step = int($largefile_lines/10);
if ($otherargs =~ /.*-scale.*/) {
  print "Begin scaling tests\n";
  for (my $i=$step; $i<=$largefile_lines; $i+=$step)
  {
    system("head -n $i $largefile > $fname.$i");
    ($score, $rate, $time, $rptime, $padstime) = inc ("$fname.$i", $initsize, $incsize);
    output("$fname.$i (inc)", $score, $rate, $time, $rptime, $padstime, 0);
    unlink ("$fname.$i");
  }
  print "End scaling tests\n";
}

#init size and incremental size tests ...
#init size from 500 to max with step of max/10
#inc size from 100 to 500 with a step of 100
$timeout=0;
if ($otherargs =~ /.*-incsize.*/) {
  print "Begin init/incremental size tests\n";
  for (my $i = 500; $i <= $largefile_lines && !$timeout; $i+=$step)
  {
   for (my $j = 100; $j <= 500; $j+=100)
   { 
    ($score, $rate, $time, $rptime, $padstime) = inc ($largefile, $i, $j);
    if (!$score) {
        print "$fname (init=$i, inc=$j): timed out\n";
	$timeout=1; last}
    else {
        output("$fname (init=$i, inc=$j)", $score, $rate, $time, $rptime, $padstime, 0);
    }
   }
  }
  print "End init/incremental size tests\n";
}

#TODO: shall we add a test for varying adc weight
# the problem is the weight seems to affect the quality of the output description
# from human perspective and can't be compared by looking at the score (since the
# weight is being varied). We could look at type complexity but the best complexity is
# the whole description being a Blob.
