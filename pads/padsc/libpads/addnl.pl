#!/usr/bin/env perl

print "#include \"libpadsc-internal.h\"\n";
print "#include <ctype.h>\n\n";

while (<>) {
  if (!(/^\s*$/) && !(/\# /)) { # do not emit source file info
    s/;/;\n/g;
    s/{/{\n/g;
    s/}/}\n/g;
    while (/\n\n/) {
      s/\n\n/\n/g;
    }
    print;
  }
}
