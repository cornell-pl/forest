#!/usr/bin/perl

($file) = @ARGV;
if (!$file) 
{
 print "Usage: vivekchop FILE\n";
 exit;
}
open (FILE, "<$file") or die "Can't open data file!\n";
$index=0;
open (NEW, ">$file.$index") or die "Can't open output file for writing";
while (<FILE>)
{
 if (/^Start: /)
 {
  close NEW;
  $index++;
  open (NEW, ">$file.$index") or die "Can't open output file for writing";
 }
 print NEW $_;
}
close NEW;
print "Finished writing to output files\n";
