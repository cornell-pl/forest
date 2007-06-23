#!/usr/bin/perl 

@golden=("1967Transactions.short", "MER_T01_01.csv", "ai.3000", 
	 "boot.log", "crashreporter.log", "dibbler.1000",
	"ls-l.txt", "netstat-an", "page_log", 
	"quarterlypersonalincome", "railroad.txt", "scrollkeeper.log",
	"windowserver_last.log", "yum.txt", "asl.log",
	"crashreporter.log.modified"
	);
foreach my $gold (@golden)
{
 $total1sttime = 0;
 $total2ndtime = 0;
 $mintotal = 10000;
 $maxtotal = -1;
 @minpair = (0, 0);
 @maxpair = (0, 0);
 $num=0;
 open (FILE, "<$gold.timing") or die "Cann't open file for reading";
 while (<FILE>) {
  if (/inference time = (.*)/) 
  { 
    $first = $1;
    $total1sttime+= $first;
    $num++;
  }
  elsif (/First reduction time = (.*)/)
  {
   $reduct1 = $1;
  }
  elsif (/Second reduction time = (.*)/)
  {
   $reduct2 = $1;
  }
  elsif (/Third reduction time = (.*)/)
  {
   $reduct3 = $1;
   $second = $reduct1 + $reduct2 + $reduct3;
   $total2ndtime += $second;
   if ($first +$second < $mintotal)
   {
    $mintotal = $first + $second;
    @minpair = ($first, $second);
   }
   elsif ($first +$second>= $maxtotal)
   {
    $maxtotal = $first+ $second;
    @maxpair = ($first, $second);
   }
  }
 }
 if ($minpair[0] == $maxpair[0] && $minpair[1] == $maxpair[2])
 {
  $total1sttime -= $minpair[0];
  $total2ndtime -= $minpair[1];
  $num--;
 }
 else {
  $total1sttime -= ($minpair[0]+$maxpair[0]); 
  $total2ndtime -= ($minpair[1]+$maxpair[1]);
  $num-=2;
 }

 printf ("$gold: \tinf: %.2f, \tref: %.2f, \ttotal: %.2f\n", 
		$total1sttime/$num,
		$total2ndtime/$num, ($total1sttime+$total2ndtime)/$num);
}
