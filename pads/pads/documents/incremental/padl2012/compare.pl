#!/usr/bin/perl
@names = ("free_clickthroughs.dat.cleaned", "thirdpartycontent.log",
	"eventstream.current", "strace_jaccn.dat", 
	"LA-UR-06-0803-MX20_NODES_0_TO_255_EVENTS.csv", "messages.sdb",
	"HALO_have2impression.log.1", 
	"LA-UR-06-1446-MX16-NODE-NOZ.TXT", "searchevents.dat.cleaned",
	"4046.xls");

($defaultfile, $ucfile) = @ARGV;
if (!$defaultfile) 
{
 print "Usage: compare.pl DEFAULT_FILE UC_FILE\n";
 exit;
}

foreach $name (@names)
{
 open (FILE, "<$defaultfile") or die "Can't open input file $defaultfile";
 while (<FILE>)
 {
#  print $_;
  if (/$name.*\(inc\): lntime = ([0-9.]+).*score = ([0-9.]+).*dist = ([0-9.]+)/)
  {
   print "$name & $1 & $2 & $3 ";
   close FILE;
   last;
  }
 }
 open (FILE, "<$ucfile") or die "Can't open input file $ucfile";
 while (<FILE>)
 {
  if (/$name.*\(inc\): lntime = ([0-9.]+).*score = ([0-9.]+).*dist = ([0-9.]+)/)
  {
   print "& $1 & $2 & $3 \\\\ \\hline\n";
   close FILE;
   last;
  }
 }
}
