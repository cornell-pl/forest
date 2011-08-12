#!/usr/bin/perl

($file) = @ARGV;
open (FILE, "<$file") or die "Can't open $file for read";
while (<FILE>)
{
 if (/Processing  (.*) \*\*\*\*/)
 {
  $name = $1;
 }
 elsif (/wc -l elapse = ([0-9.]+) secs/)
 {
  $wc = $1;
 }
 elsif (/Final comps = \(.*, ([0-9.]+)\)/)
 {
  $score = $1;
 }
 elsif (/Total time = ([0-9.]+) secs/)
 {
  $time = $1;
 }
 elsif (/Edit distance = ([0-9.]+)/)
 {
  $dist = $1;
 }
 elsif (/Reparse time = ([0-9.]+) secs/)
 {
  $reparse = $1;
 }
 elsif (/PADS parse time = ([0-9.]+) secs/)
 {
  $pads = $1;
 }
 elsif (/Blob parse time = ([0-9.]+) secs/)
 {
  $blob = $1;
 }
 elsif (/Accuracy = ([0-9.]+)%/)
 {
  $accuracy = "$1\\%";
  print "$name & $wc & $blob & $time & $score & $dist & $accuracy & $reparse & $pads \\\\ \\hline\n"
 }
}
close FILE;
 

