#!/usr/bin/perl
# usage: wasl-run.pl LARGE_FILE
# this script first creates a number of smaller files from
# the large file of size 200k, 400k, 600k, 800k and 1M lines each
# and then incrementally learn scriptions from these files using
# various settings of optimizations. The learn size is 100 and the
# chunk size is also 1000.

@testfiles = ( 
		"ai.big",
		"pws", "interface.loop", 
		"asl.log", "error_log", "access_log", 
		"coblitz.access.20090618_000", 
		"access.exlog.20090621_000",
		"debug_getbig.20090624_000",
		"dbg_req_redirect.20090624_000"
	     );
$numTimes = 3;
$timeout = 10;
$initsize = 500;
$incsize = 100;

sub scan
{
  (my $file) = @_;
  my $score=0, $time=0;
  if (-e $file)
  { 
  open (FILE, "<$file") or die "Can't open $file!";
  while (<FILE>)
  {
   if (/Final comps = \(([0-9.]+)b, ([0-9.]+)b, ([0-9.]+)/) 
   {$score = $3;}
   elsif (/Total time = ([0-9.]+)/) 
   {$time = $1;}
  }
  return ($score, $time)
  }
  else {
   print "$file doesn't exist!\n";
   {return (0, 0);}
  }
}

sub learn
{
 (my $test) = @_;
# print "learn $test > $test.lrn\n";
 alarm $timeout;
 exec("learn $test > $test.output");
 exit(1);
}

sub inc
{
  my ($file, $isize, $lsize) = @_;
  my $score = 0;
  my $time = 0;
  my $exectime = 0;
  for (my $j = 0; $j < $numTimes; $j++)
  {
    #print "Learning $file: opt_level = 3\n";
    $exectime = 0;
    system ("increment -f $file -i $isize -l $lsize > $file.inc");
    ($score, $exectime) = scan ("$file.inc");
    $time += $exectime;
  }
  $time = $time/$numTimes; 
  return ($time, $score)
}

$largefile = shift @ARGV;
if (!$largefile) {
  print "Usage: wasl-run.pl LARGE_FILE_NAME [-small] [-scale] [-incsize] \n";
  exit;
}

$otheraugs = join (' ', @ARGV);

#Multiple smaller files tests
if ($otheraugs =~ /.*-small.*/) {
  print "Begin comparison tests on small files\n";
  foreach $test (@testfiles) {
    $time = 0;
    $score = 0;
    $num = 0;
    for ($i=0; $i<$numTimes; $i++)
    {
     $num++;
     $exectime = 0;
     system ("learn $test > $test.lrn");
     ($score, $exectime) = scan ("$test.lrn");
     $time += $exectime;
     #system("rm -f $test.output");
     if (!$score) {last;} #time out reached
    }
    $time = $time/$num; #$num maybe less than $numTimes
    print "$test (lrn): time = $time  score = $score (averaged from $num times)\n";
    
    ($time, $score) = inc($test, $initsize, $incsize);
    print "$test (inc): time = $time  score = $score (averaged from $num times)\n";
  }
  print "End comparison tests on small files\n";
}

#Scaling tests... 
if ($otheraugs =~ /.*-scale.*/) {
  print "Begin scaling tests\n";
  for (my $i=200000; $i<=1000000; $i+=200000)
  {
    system("head -n $i $largefile > $largefile.$i");
    ($time, $score) = inc ($largefile.$i, $initsize, $incsize);
    print "$largefile.$i (inc): time = $time  score = $score\n";
  }
  print "End scaling tests\n";
}

#init size and incremental size tests ...
#init size from 100 to 1000 with step of 100
#inc size from 10 to 100 with a step of 10
if ($otheraugs =~ /.*-incsize.*/) {
  print "Begin init/incremental size tests\n";
  for (my $i = 100; $i <= 1000; $i+=100)
  {
   for (my $j = 10; $j <= 100; $j+=10)
   { 
    ($time, $score) = inc ($largefile, $i, $j);
    print "$largefile (init=$i, inc=$j): time = $time  score = $score\n";
   }
  }
  print "End init/incremental size tests\n";
}

#TODO: shall we add a test for varying adc weight
# the problem is the weight seems to affect the quality of the output description
# from human perspective and can't be compared by looking at the score (since the
# weight is being varied). We could look at type complexity but the best complexity is
# the whole description being a Blob.
