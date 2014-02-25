#!/usr/bin/perl
# Determine the process Ids of the apache web server

my ( @psaux ) = `ps -aux | grep httpd | grep -v grep | grep -v perl`;
my ( @pss )   = ();

foreach $ps (@psaux) {
  local ($own, $pid, @rest) = split (/ +/, $ps);
  push ( @pss, $pid );
}

my ( $npids ) = $#pss + 1;
my ( $pids ) = join ( " ", @pss );
print "$pids\n" if $npids > 0;
print "NONE\n" if $npids == 0;
