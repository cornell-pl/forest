#!/usr/bin/perl
# Kill apache, and start it fresh

# Make sure we are running as root.
my ( $id ) = `id | cut -f1 -d" "`;
die "You need to run this as root" if !($id =~ "root");

my ( $squelch ) = `squelch-apache.pl really`;
my ( $fresh )   = `apachectl start`;
my ( $pids )    = `httpdPids.pl`;
print "After restarting apache, httpd pids now active: $pids\n";

