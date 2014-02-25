#!/usr/bin/perl
sub shuffle 
{
 my (@myarray) = @_;
 my @newarray = ();
 while (@myarray != ())
 {
  $index = int(rand ($#myarray+1));
  push (@newarray, $myarray[$index]);
  splice @myarray, $index, 1;
 }
 return @newarray;
}

($filename, $fromline, $toline, $times) = @ARGV;
if (!$filename) 
{
 print "Usage FILENAME FROMLINE TOLINE NUM_TIMES\n";
 exit;
}

$count = 0;
@lines=();
open(FILE, "<$filename") or die "Can't open $filename for input";
while (<FILE>)
{
 $count++;
 if ($count>=$fromline && $count<=$toline)
 {
  push (@lines, $_);
 }
 elsif ($count> $toline) 
 { break;}
}

close FILE;

for ($i = 1; $i<=$times; $i++)
{
  my @newpart = shuffle @lines;
  open (OUT, ">$filename.perturb.$i") or die "can't open output file";
  open(FILE, "<$filename") or die "Can't open $filename for input";
  $count = 0;
  while (<FILE>)
  {
   $count++;
   if ($count<$fromline || $count>$toline)
   {
    print OUT $_;
   }
   else 
   {
    my $line = shift @newpart;
    print OUT $line;
   }
  }
  close OUT;
  close FILE;
  print "written to $filename.perturb.$i\n";
}   

