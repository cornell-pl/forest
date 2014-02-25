#!/usr/bin/perl


($filename) = @ARGV;
if (!$filename) {
  print "Usage: wasl-small.pl DATA_SOURCE_NAME\n";
  exit;
}
  print "\nlearn $filename > inc_output/$filename.learn.1\n";
  system ("learn $filename > inc_output/$filename.learn.1");
  print "\nlearn $filename > inc_output/$filename.learn.2\n";
  system ("learn $filename > inc_output/$filename.learn.2");
  print "\nlearn $filename > inc_output/$filename.learn.3\n";
  system ("learn $filename > inc_output/$filename.learn.3");
  print "\nincrement $filename 500 100 0 >inc_output/$filename.increment.1\n";
 system ("increment $filename 500 100 0 >inc_output/$filename.increment.1");
  print "\nincrement $filename 500 100 0 >inc_output/$filename.increment.2\n";
  system ("increment $filename 500 100 0 >inc_output/$filename.increment.2");
  print "\nincrement $filename 500 100 0 >inc_output/$filename.increment.3\n";
  system ("increment $filename 500 100 0 >inc_output/$filename.increment.3");



