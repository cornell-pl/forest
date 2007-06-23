#!/usr/bin/perl 
$rateplot = "succ.plot";
$rategnu = "succ.gnu";
$timeplot = "time.plot";
$timegnu = "time.gnu";
@golden=("1967Transactions.short", "MER_T01_01.csv", "ai.3000",
	"asl.log",
        "boot.log", "crashreporter.log", 
        "crashreporter.log.modified",
	"dibbler.1000",
        "ls-l.txt", "netstat-an", "page_log",
        "quarterlypersonalincome", "railroad.txt", "scrollkeeper.log",
        "windowserver_last.log", "yum.txt"
        );
@sizes=(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80);
$header=
"set term postscript eps \"Helvetica\" 14
set style data lp
set xlabel \"Training size (%)\"\n";

open (OUT, ">$rateplot") or die "Can't open file for output!";
open (OUT1, ">$timeplot") or die "Can't open file for output!";
open (GNU, ">$rategnu") or die "Can't open gnu file for output!";
open (GNU1, ">$timegnu") or die "Can't open gnu file for output!";
print GNU $header;
print GNU1 $header;
print GNU "set key right bottom 
set ylabel \"Success rate (%)\"
set output \'successrate.eps\'
plot ";
print GNU1 "set key top left
set ylabel \"Avg Time (secs)
set output \'traintime.eps\'
plot ";
$index = 0;
foreach my $gold (@golden)
{
  print OUT "#$gold\n";
  print OUT1 "#$gold\n";
  if ($index< $#golden)
  {
   print GNU "\'$rateplot\' index $index title \"$gold\", ";
   print GNU1 "\'$timeplot\' index $index title \"$gold\", ";
  }
  else 
  {
   print GNU "\'$rateplot\' index $index title \"$gold\"\n";
   print GNU1 "\'$timeplot\' index $index title \"$gold\"\n";
  }
  for my $size (@sizes) 
  {
   open (FILE, "<$gold.train.$size") or die "can't open file $gold.train.$size for read!";
   while (<FILE>)
   {
    if (/The average time = ([0-9.]+)/)
    {
     print OUT1 "$size\t$1\n";
    }
    if (/The average success rate = ([0-9.]+)/)
    {
     print OUT "$size\t$1\n";
    }
   }
  }
  print OUT "\n\n"; 
  print OUT1 "\n\n"; 
  $index++
}
close OUT;
close OUT1;
close GNU;
close GNU1;
