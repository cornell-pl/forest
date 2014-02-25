#!/usr/bin/perl
#this utility selects from a given data file lines which contain certain pattern and 
#save the lines to another file with a suffix ID
($filename, $id, $str) = @ARGV;
if (!$filename || !$id) {
  print "Usage: select FILENAME ID PATTERN\n";
  exit
}
open (INF, "<$filename") or die "Can't read $filename for reading\n";
open (OUT, ">$filename.$id") or die "Can't read $filename.$id for writing\n";
while (<INF>)
{
 if ($_ =~ /$str/) {print OUT $_;}
}
close OUT;
close INF;
