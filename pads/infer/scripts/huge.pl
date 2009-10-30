#!/usr/bin/perl
#this script incrementally learns a huge file > 1 GB
#use Time::HiRes qw ( time alarm sleep );

sub getFileName
{
 #getting the file name of the largefile
 my ($file) = @_;
 @parts = split(/\//, $file);
 return $parts[$#parts];
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
   if (/Final comps = (.*)$/)
   {$score = $1;}
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

 if (! -e "gen/$arch/blob-parse") {
  system ("cd gen; make blob-parse>&/dev/null");
 }
 $begintm = time;
 system ("gen/$arch/blob-parse $datafile >& /dev/null");
 my $blob_elapse = time - $begintm;

 return ((100.0 * ($total - $bads)/$total), $elapse, $blob_elapse);
}

$pads_home = `echo \$PADS_HOME`;
chomp $pads_home;
$arch = `$pads_home/ast-ast/bin/package.cvs`;
chomp $arch;

($initsize, $incsize, @largefiles, $rep) = @ARGV;
if (!$initsize) {
 print "Usage: huge.pl INIT_SZ INC_SZ LARGE_FILE_PATHS RE-PARSE[1|0]\n";
 exit;
}

if (!$rep || $rep == 0) {$reparse = "false";}
else {$reparse = "true";}

foreach my $largefile (@largefiles)
{
   $fname = getFileName($largefile);
   
   $beginwctm = time ();
   $wcoutput = `wc -l $largefile`;
   $wc_elapse = time() - $beginwctm;
   if ($wcoutput =~ /\s*(\d+).*/) {
    $largefile_lines = $1;
   }
   
   ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
   $atime,$mtime,$ctime,$blksize,$blocks) = stat($largefile);
   $multiplier = $size / (1024*1024*512);
   if ($multiplier < 1)
   {print "$largefile is too small!\n"; exit;}
   
   print "\n**** Processing  $largefile ****\n";
   print "wc -l elapse = $wc_elapse secs\n";
   
   $linecount = int($largefile_lines / $multiplier);
   if (! -e "$fname.$linecount.aa")
   {
     system ("split -l $linecount $largefile $fname.$linecount.");
   }
   @smallfiles = `ls $fname.$linecount.??`;
   chomp @smallfiles;
   $filenames = join (' ', @smallfiles);
   system ("increment -f $filenames -i $initsize -l $incsize -output gen -reparse $reparse -u true > $fname.inc");
   ($scores, $exectime, $reparsetime) = scan ("$fname.inc");
   print "Final comps = $scores\n";
   print "Total time = $exectime secs\n";
   if ($reparse == "true") {
     ($rate, $ptime, $btime) = verify($smallfiles[0], $largefile);
     print "Reparse time = $reparsetime secs\n";
     print "PADS parse time = $ptime secs\n";
     print "Blob parse time = $btime secs\n";
     print "Accuracy = $rate%\n";
   }

#$firstfile = shift @smallfiles;
#if (! -e "gen/$arch/$firstfile.pxml")
#{
#  system ("increment -f $firstfile -i $initsize -l $incsize -output gen > $firstfile.inc");
#  system ("cd gen; make $firstfile-parse>&/dev/null"); 
#}
#($score, $exectime, $reparsetime) = scan ("$firstfile.inc");
#if (!$score) {print "$firstfile timed out!\n"; exit;}
#$time+=$exectime;
#
#$xmlfile = "gen/$arch/$firstfile.pxml";
#if (! -e $xmlfile) {print "pxml file $xmlfile doesn't exist!\n"; exit;}
#foreach my $smallfile (@smallfiles)
#{
# if (! -e "gen/$arch/$smallfile.pxml")
# {
#  system ("increment -f $smallfile -d $xmlfile -i $initsize -l $incsize -output gen > $smallfile.inc");
#  system ("cd gen; make $smallfile-parse>&/dev/null"); 
# }
# ($scores, $exectime, $reparsetime) = scan ("$smallfile.inc");
# if (!$exectime) {print "$smallfile timed out!\n"; exit;}
# $time+=$exectime;
#
# $xmlfile = "gen/$arch/$smallfile.pxml";
# if (! -e $xmlfile) {print "pxml file $xmlfile doesn't exist!\n"; exit;}
#}

#system ("rm -f $fname.$linecount.*");
}

