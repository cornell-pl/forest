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

@largefiles = @ARGV;
if (!@largefiles) {
 print "Usage: huge.pl LARGE_FILE_PATHS\n";
 exit;
}

foreach my $largefile (@largefiles)
{
$initsize = 500;
$fname = getFileName($largefile);
$wcoutput = `wc -l $largefile`;
if ($wcoutput =~ /\s*(\d+).*/) {
 $largefile_lines = $1;
}
($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
$atime,$mtime,$ctime,$blksize,$blocks) = stat($largefile);
$multiplier = $size / (1024*1024*512);
if ($multiplier < 1)
{print "$largefile is too small!\n"; exit;}

print "**** Processing  $largefile ****\n";

$linecount = int($largefile_lines / $multiplier);
if (! -e "$fname.$linecount.aa")
{
  system ("split -l $linecount $largefile $fname.$linecount.");
}
@smallfiles = `ls $fname.$linecount.??`;
chomp @smallfiles;
$time = 0;
$firstfile = shift @smallfiles;
if (! -e "gen/$arch/$firstfile.pxml")
{
  system ("increment -f $firstfile -i $initsize -l 100 -output gen > $firstfile.inc");
  system ("cd gen; make $firstfile-parse>&/dev/null"); 
}
($score, $exectime, $reparsetime) = scan ("$firstfile.inc");
if (!$score) {print "$firstfile timed out!\n"; exit;}
$time+=$exectime;

$xmlfile = "gen/$arch/$firstfile.pxml";
if (! -e $xmlfile) {print "pxml file $xmlfile doesn't exist!\n"; exit;}
foreach my $smallfile (@smallfiles)
{
 if (! -e "gen/$arch/$smallfile.pxml")
 {
  system ("increment -f $smallfile -d $xmlfile -i $initsize -l 100 -output gen > $smallfile.inc");
  system ("cd gen; make $smallfile-parse>&/dev/null"); 
 }
 ($scores, $exectime, $reparsetime) = scan ("$smallfile.inc");
 if (!$exectime) {print "$smallfile timed out!\n"; exit;}
 $time+=$exectime;

 $xmlfile = "gen/$arch/$smallfile.pxml";
 if (! -e $xmlfile) {print "pxml file $xmlfile doesn't exist!\n"; exit;}
}

#system ("rm -f $fname.$linecount.*");
print "Final comps = $scores\n";
print "Total time = $time secs\n\n";
}

