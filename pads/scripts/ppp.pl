#!/usr/bin/env perl

goto usage if ($#ARGV != 0);
my $ifilename = $ARGV[0];

my $tfmt = "%-30s";
my %pads_ty =
  (
   d => "Pint",
   u => "Puint",
   s => "Pstring"
  );
my %stop_re =
  (
   d => "/[+-]?\\d/",
   u => "/[+]?\\d/",
   s => undef
  );

my ($after, $d, $elt, $expr, $first_follow, $follow_re);
my ($has_re, $i, $indent, $litpre, $ll, $parsed, $pty);
my ($rest, $toparse, $ty, $w);

open(IFILE, $ifilename) || die "Could not open input file $ifilename\n";

my $line         = 0;
my $emit_lineno  = 1;
my $just_emitted = 0;

LINE: while (<IFILE>) {
  $line++;
  if ($emit_lineno) {
    print "# $line \"$ifilename\"\n"; $just_emitted = 1;  $emit_lineno  = 0;
  } else {
    $just_emitted = 0;
  }
  if (/^(\s*)((.*?){(\s*Pre\s*)?:(.+?):}(\s*[;])?(.*))$/) {
    # found a line with >= 1 {: ... :} element
    ($indent, $rest) = ($1, $2);
    print "# $line \"$ifilename\"\n" if (!$just_emitted);
    print "// BEGIN PADS pre-processor translation of ${ifilename}:$line\n//   $rest\n";

  ELT: while ($rest =~ /^(.*?){(\s*Pre\s*)?:(.+?):}(\s*[;])?(.*)$/) {
      ($before, $has_re, $toparse, $rest) = ($1, $2, $3, $5);

      $before =~ s/^\s+//;
      print "$indent$before\n" if ($before);

      $litpre = ($has_re) ? "${indent}Pre \"/" : "${indent}\"";
      $litpost = ($has_re) ? "/\"" : "\"";
      $parsed = "";
      $follow_re = "/\$/";
      
      while (1) { # parse each %<expr> in the elt
	if (!$toparse) {
	  # done with entire {: ... :} elt
	  print $parsed;
	  next ELT;
	}
	# look at %<expr> forms one at a time, right to left 
	if ($toparse =~ /^(.*[^\\]%|%)(.*?)$/) {
	  ($toparse, $expr) = ($1, $2);
	  $toparse =~ s/%$//;
	  if ($expr =~ /^(\d*)I(\d+)([du])[(](\w+)[)](.*?)$/) {
	    # handle %Id, %Iu forms
	    ($w, $d, $ty, $id, $after) = ($1, $2, $3, $4, $5);
	    $i = 8;
	    if ($d == 64) {
	      $i = 64;
	    } else {
	      while ($i < ($d * 8)) { $i++; }
	    }
	    $pty = "$pads_ty{$ty}$i";
	    $pty .= "_FW(:${w}:)" if ($w);
	  } elsif ($expr =~ /^(\d*)(l|ll)?(d|u)[(](\w+)[)](.*?)$/) {
	    # handle %d, %u, %ld, %lu, %lld, %llu forms
	    ($w, $ll, $ty, $id, $after) = ($1, $2, $3, $4, $5);
	    $i = ($ll eq "ll") ? 64 : 32;
	    $pty = "$pads_ty{$ty}$i";
	    $pty .= "_FW(:${w}:)" if ($w);
	  } elsif ($expr =~ /^(\d*)(s)[(](\w+)[)](.*?)$/) {
	    # handle %s forms
	    ($w, $ty, $id, $after) = ($1, $2, $3, $4);
	    $pty = $pads_ty{$ty};
	    if ($w) {
	      $pty .= "_FW(:${w}:)";
	    } else {
	      # need a stop re
	      if ($after) {
		$first_follow = substr($after, 0, 1);
		# XXX need to deal with case where $first_follow is a special char XXX
		$follow_re = "/[$first_follow]/";
	      }
	      die "Cannot automatically determine what follows the %s($id) at ${ifilename}:$line\n"
		if (!defined($follow_re));
	      $pty .= "_SE(:\"$follow_re\":)";
	    }
	  } else {
	    # Did not understand %<expr> form
	    die "Did not understand this %<expr> expression: %$expr on ${ifilename}:$line\n";
	  }
	  # determined pty, id, after
	  $elt = sprintf("$indent$tfmt %s;\n", $pty, $id);
	  $elt .= "$litpre$after$litpost;\n" if ($after);
	  $parsed = "$elt$parsed";
	  $follow_re = $stop_re{$ty};
	} else {
	  # last case is to match a leading non %<expr>
	  $parsed = "$litpre$toparse$litpost;\n$parsed";
	  $toparse = "";
	}
      } # end while (1) for each %<expr> in an elt
    } # end while loop over each elt
    # emit rest of line, if any
    $rest =~ s/^\s+//;
    print "$indent$rest\n" if ($rest);
    print "// END PADS pre-precessor translation of ${ifilename}:$line\n";
    $emit_lineno = 1;
    next LINE;
  } # end if the line contains >= 1 elt
  # the line does not contain an elt, emit as-is
  print;
}
exit 0;

usage:
 die "Usage: ppp.pl <infile>";
