#!/usr/bin/env perl

$prev_empty = 0;
$pragma_prototyped = 0;
while (<>) {
  if (/^\s*$/) {
    if ($prev_empty == 1) {
      next;
    }
    $prev_empty = 1;
    print;
    next;
  }
  if (/\#pragma prototyped/) {
    if ($pragma_prototyped == 1) {
      next;
    }
    $pragma_prototyped = 1;
    print;
    next;
  }
  if ((/\# /) || (/\#line/)) { # do not emit source file info
    next;
  }
  foreach $part (split(/\"([^\"]|[\\][\"])*\"/)) {
    my $origpart = quotemeta($part);
    my $modpart = $part;
    $modpart =~ s/;([^\"])/;\n\1/g;
    $modpart =~ s/([\{\}])/\1\n/g;
    s/$origpart/$modpart/g;
  }
  while (/\n\n/) {
    s/\n\n/\n/g;
  }
  s/\}\s*;/\};/g;
  print;
  $prev_empty = 0;
}
