#!/usr/bin/env perl

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

my ($has_re, $elt, $indent, $litpre, $toparse, $expr, $after, $d, $w, $ty, $pty, $parsed, $follow_re);

my $line = 0;
LINE: while (<>) {
  $line++;
  if (/^(\s*){(\s*Pre\s*)?:(.+):}(;)?$/) {
    # found a {: ... :} block

    ($indent, $has_re, $toparse) = ($1, $2, $3);
    $has_re = "Pre " if ($has_re);
    $litpre = "$indent$has_re";
    $parsed = "";
    $follow_re = "/\$/";
    
    while (1) {
      if (!$toparse) {
	# done with entire {: ... :} block
	print $parsed;
	next LINE;
      }
      # look at %<expr> forms one at a time, right to left 
      if ($toparse =~ /^(.*[^\\]%|%)(.*?)$/) {
	($toparse, $expr) = ($1, $2);
	$toparse =~ s/%$//;
	if ($expr =~ /^(\d*)I(\d+)([du])[(](\w+)[)](.*?)$/) {
	  ($w, $d, $ty, $id, $after) = ($1, $2, $3, $4, $5);
	  my $i = 8;
	  if ($d == 64) {
	    $i = 64;
	  } else {
	    while ($i < ($d * 8)) { $i++; }
	  }
	  $pty = "$pads_ty{$ty}$i";
	  $pty .= "_FW(:${w}:)" if ($w);
	} elsif ($expr =~ /^(\d*)(s)[(](\w+)[)](.*?)$/) {
	  ($w, $ty, $id, $after) = ($1, $2, $3, $4);
	  $pty = $pads_ty{$ty};
	  if ($w) {
	    $pty .= "_FW(:${w}:)";
	  } else {
	    # need a stop re
	    if ($after) {
	      my $first_follow = substr($after, 0, 1);
	      # XXX need to deal with case where $first_follow is a special char XXX
	      $follow_re = "/[$first_follow]/";
	    }
	    die "Cannot automatically determine what follows the %s($id) on line $line\n"
	      if (!defined($follow_re));
	    $pty .= "_SE(:\"$follow_re\":)";
	  }
	} else {
	  # Did not understand this %<expr>
	  die "Did not understand this %<expr> expression: %$expr on line $line\n";
	}
	# determined pty, id, after
	$elt = sprintf("$indent$tfmt %s;\n", $pty, $id);
	$elt .= "$litpre\"$after\";\n" if ($after);
	$parsed = "$elt$parsed";
	$follow_re = $stop_re{$ty};
      } else {
	# last case is to match a leading non %<expr>
	$parsed = "$litpre\"$toparse\";\n$parsed";
	$toparse = "";
      }
    }
  }
  print;
}
