#!/usr/bin/env perl

LINE: while (<>) {
  next LINE if (/ignoring pragma/);
  if (/In file included from/) {
    my @ermsgs = ();
    push (@ermsgs, $_);
    ERLINE: while (<>) {
      if (/^\s+from\s/) {
	push (@ermsgs, $_);
	next ERLINE;
      } elsif (/ignoring pragma/) {
	next LINE; # drops everything pushed to ermsgs
      }
      # another kind of error
      push (@ermsgs, $_);
      for $ln ( @ermsgs ) {
	print $ln;
      }
      next LINE;
    }
  }
  print $_;
}
