#!/usr/bin/env perl

my $d = 0;
while (<>) {
  if (/DIFFERS/) {
    $d++;
  }
  print;
}
print "\n\nNumber of regression tests with differences: $d\n\n";
