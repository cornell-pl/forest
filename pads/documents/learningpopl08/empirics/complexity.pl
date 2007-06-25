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
 open (FILE, "<$gold.report") or die "Cann't open file for reading";
 $arrived = 0;
 while (<FILE>) {
  if (/New complexity/) {$arrived = 1;}
  if ($arrived) {
  if (/TC = ([0-9.]+)b,.*DC = ([0-9.]+)b.*normalized by (\d+) is ([0-9.]+)/) 
  { 
    $tc = $1;
    $dc = $2;
    $bytes = $3;
    $total = $4;
    printf ("$gold: ntc = %.4f  ndc = %.4f  total = %.4f\n", $tc/$bytes, $dc/$bytes, $total);
    last;
  }
  }
 }
}
close FILE;
