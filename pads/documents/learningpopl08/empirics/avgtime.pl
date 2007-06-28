#!/usr/bin/perl 

@golden=("1967Transactions.short", "MER_T01_01.csv", "ai.3000", 
	"asl.log", "boot.log", "crashreporter.log", 
	"crashreporter.log.modified", "dibbler.1000",
	"ls-l.txt", "netstat-an", "page_log", 
	"quarterlypersonalincome", "railroad.txt", "scrollkeeper.log",
	"windowserver_last.log", "yum.txt" 
	);
foreach my $gold (@golden)
{
 $total1sttime = 0;
 $total2ndtime = 0;
 $totalalltime = 0;
 $mintotal = 10000;
 $maxtotal = -1;
 @mintriple = (0, 0, 0);
 @maxtriple = (0, 0, 0);
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
  }
  elsif (/Total time = (.*)/)
  {
   $total = $1;
   $totalalltime +=$total;
   if ($total < $mintotal)
   {
    $mintotal = $toal;
    @mintriple = ($first, $second, $total);
   }
   elsif ($total >= $maxtotal)
   {
    $maxtotal = $total;
    @maxtriple = ($first, $second, $total);
   }
  }
 }
 if ($mintriple[0] == $maxtriple[0] && $mintriple[1] == $maxtriple[2])
 {
  $total1sttime -= $mintriple[0];
  $total2ndtime -= $mintriple[1];
  $totalalltime -= $mintriple[2];
  $num--;
 }
 else {
  $total1sttime -= ($mintriple[0]+$maxtriple[0]); 
  $total2ndtime -= ($mintriple[1]+$maxtriple[1]);
  $totalalltime -= ($mintriple[2]+$maxtriple[2]);
  $num-=2;
 }

 printf ("$gold: \tinf: %.2f, \tref: %.2f, \ttotal: %.2f\n", 
		$total1sttime/$num,
		$total2ndtime/$num, $totalalltime/$num);
}
close FILE;
