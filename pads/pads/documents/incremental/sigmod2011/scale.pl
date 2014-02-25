#!/usr/bin/perl
@names = ("free_clickthroughs.dat.cleaned", "thirdpartycontent.log",
	"eventstream.current", "strace_jaccn.dat", 
	"LA-UR-06-0803-MX20_NODES_0_TO_255_EVENTS.csv", "messages.sdb",
	"HALO_have2impression.log.1", 
	"LA-UR-06-1446-MX16-NODE-NOZ.TXT", "searchevents.dat.cleaned",
	"4046.xls");

($file) = @ARGV;
if (!$file) 
{
 print "Usage: scale.pl FILE\n";
 exit;
}

foreach $name (@names)
{
 open (FILE, "<$file") or die "Can't open input file $file";
 $percent = 0.0;
 while (<FILE>)
 {
  if (/Processing $name/)
  {
   $percent += 0.2;
   print "#$name\n"
  }
  if ($percent > 0 && $percent<= 1.0)
  {
   if (/lntime = ([0-9.]+)/) 
   {
    print "$percent\t$1\n";
    $percent += 0.2;
   }
  }
  if ($percent > 1.0)
  {
   close FILE;
   print "\n\n";
   last;
  }
 }
}
