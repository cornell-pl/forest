#!/usr/bin/perl
#this script incrementally learns a huge file > 1 GB

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
$pads_home = `echo \$PADS_HOME`;
chomp $pads_home;
$arch = `$pads_home/ast-ast/bin/package.cvs`;
chomp $arch;

($largefile) = @ARGV;
if (!$largefile) {
 print "Usage: huge.pl LARGE_FILE_PATH\n";
 exit;
}

$initsize = 500;
$fname = getFileName($largefile);
$wcoutput = `wc -l $largefile`;
if ($wcoutput =~ /\s*(\d+).*/) {
 $largefile_lines = $1;
}
($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
$atime,$mtime,$ctime,$blksize,$blocks) = stat($largefile);
$multiplier = $size / (1024*1024*1024);
if ($multiplier < 1)
{print "$largefile is too small!\n"; exit;}

$linecount = int($largefile_lines / $multiplier);

system ("split -l $linecount $largefile $fname.$linecount.");
@smallfiles = `ls $fname.$linecount.*`;
chomp @smallfiles;
$time = 0;
$firstfile = shift @smallfiles;
system ("increment -f $firstfile -i $initsize -l 100 -output gen > $firstfile.inc");
($score, $exectime, $reparsetime) = scan ("$firstfile.inc");
if (!$score) {print "$firstfile timed out!\n"; exit;}
$time+=$exectime;
system ("cd gen; make $firstfile-parse>&/dev/null"); 
$xmlfile = "gen/$arch/$firstfile.pxml";
if (! -e $xmlfile) {print "pxml file $xmlfile doesn't exist!\n"; exit;}
 
foreach my $smallfile (@smallfiles)
{
 system ("increment -f $smallfile -d $xmlfile -i $initsize -l 100 -output gen > $smallfile.inc");
 ($scores, $exectime, $reparsetime) = scan ("$smallfile.inc");
 if (!$exectime) {print "$smallfile timed out!\n"; exit;}
 $time+=$exectime;
 system ("cd gen; make $smallfile-parse>&/dev/null"); 
 $xmlfile = "gen/$arch/$smallfile.pxml";
 if (! -e $xmlfile) {print "pxml file $xmlfile doesn't exist!\n"; exit;}
}
print "Final comps = $scores\n";
print "Total time = $time secs\n";

