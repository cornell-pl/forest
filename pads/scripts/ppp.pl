#!/usr/bin/env perl

my $tfmt = "%-30s";
my %pads_ty =
  (
   d => "Pint",
   u => "Puint"
  );
my %stop_re =
  (
   d => "/[+-]?\d/",
   u => "/[+]?\d/"
  );

my ($has_re, $elt, $indent, $litpre, $toparse, $expr, $before, $after, $d, $ty, $pty, $parsed, $follow_re);
my $open_chars = "{/";

my $line = 0;
LINE: while (<>) {
  $line++;
  if (/^(\s*){(\s*Pre\s*)?:(.*):}(;)?$/) {
    ($indent, $has_re, $toparse) = ($1, $2, $3);
    $has_re = "Pre " if ($has_re);
    $litpre = "$indent$has_re";
    $parsed = "";
    $follow_re = "/\$/";
    EXPR: while (1) {
      if (!$toparse) {
	print $parsed;
	next LINE;
      }
      # look at %<expr> forms one at a time, right to left 
      if ($toparse =~ /^(.*[^\\]%|%)(.*?)$/) {
	($before, $expr) = ($1, $2);
	$before =~ s/%$//;
	$toparse = $before;
	if ($expr =~ /^I(\d+)([du])[(](\w+)[)](.*?)$/) {
	  ($d, $ty, $id, $after) = ($1, $2, $3, $4);
	  my $i = 8;
	  if ($d == 64) {
	    $i = 64;
	  } else {
	    while ($i < ($d * 8)) { $i++; }
	  }
	  $pty = $pads_ty{$ty};
	  $elt = sprintf("$indent$tfmt %s;\n", "$pty$i", $id);
	  $elt .= "$litpre\"$after\";\n" if ($after);
	  $parsed = "$elt$parsed";
	  $follow_re = $stop_re{$ty};
	  next EXPR;
	}
	if ($expr =~ /^(\d+)s[(](\w+)[)](.*?)$/) {
	  ($d, $id, $after) = ($1, $2, $3);
	  $elt = sprintf("$indent$tfmt %s;\n", "Pstring_FW(:$d:)", $id);
	  $elt .= "$litpre\"$after\";\n" if ($after);
	  $parsed = "$elt$parsed";
	  $follow_re = undef;
	  next EXPR;
	}
	if ($expr =~ /^s[(](\w+)[)](.*?)$/) {
	  ($id, $after) = ($1, $2);
	  if ($after) {
	    my $first_follow = substr($after, 0, 1);
	    $follow_re = "/[$first_follow]/";
	  }
	  die "Cannot automatically determine what follows the %s($id) on line $line\n"
	    if (!defined($follow_re));
	  $elt = sprintf("$indent$tfmt %s;\n", "Pstring_SE(:\"$follow_re\":)", $id);
	  $elt .= "$litpre\"$after\";\n" if ($after);
	  $parsed = "$elt$parsed";
	  $follow_re = undef;
	  next EXPR;
	}
	# Did not understand this %<expr>
	die "Did not understand this %<expr> expression: %$expr on line $line\n";
      } else {
	# last case is to match a leading non %<expr>
	$parsed = "$litpre\"$toparse\";\n$parsed";
	$toparse = "";
      }
    }
  }
  print;
}
