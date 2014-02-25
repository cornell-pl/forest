#!/usr/bin/perl
@files = @ARGV;
if (!@files) {
 print "Usage: cleaneol.pl FILES\n";
 exit;
}

foreach my $file (@files) {
  open (FILE, "<$file") or die "can't open $file!";
  $_ = <FILE>;
  if ($_ !~ /\r\n$/) {
	close FILE;
	next;
  }
  close FILE;
  open (FILE, "<$file") or die "can't open $file!";
  open (OUT, ">$file.cleaned") or die "can't open $file.cleaned for output!";
  while (<FILE>)
  {
   $line = $_;
   $line =~ s/\r\n$/\n/g;
   print OUT $line;
  }
  close FILE;
  close OUT;
  print "Output cleaned file to $file.cleaned.\n";
}
