#!/usr/bin/perl
@golden=(#"1967Transactions.short", "MER_T01_01.csv", "ai.3000", 
	 #"boot.log", "crashreporter.log", "dibbler.1000",
	#"ls-l.txt", "netstat-an", "page_log", 
	#"quarterlypersonalincome", "railroad.txt", "scrollkeeper.log",
	#"windowserver_last.log", "yum.txt", "asl.log",
	"crashreporter.log.modified");

foreach my $gold (@golden)
{
 for ($i=0; $i<10; $i++)
 {
  `make $gold.rep`;
 }
}
