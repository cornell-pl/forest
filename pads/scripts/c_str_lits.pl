#!/usr/bin/env perl

goto usage if ($#ARGV != -1);

while (<>) {
  #        1  2                   3      4 5
  while (s/(\"([^\"]|[\\][\"])*)\"(\s*)\"(([^\"]|[\\][\"])*\")/\1\4/g) {}
  print;
}
