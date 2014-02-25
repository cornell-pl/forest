#!/usr/bin/perl
# Kill all the apache processes.

my ( $nargs ) = $#ARGV + 1;
die "No argument specified\n" if ( $nargs != 1 );
my ( $arg ) = $ARGV[0];

# Get the list of pids of the apache web server.
my ( $pids ) = `httpdPids.pl`;
print "Nothing to kill\n" if ( $pids eq "NONE" );
exit 0 if ( $pids =~ "NONE" );
chomp ( $pids );

# Make sure we are running as root.
my ( $id ) = `id | cut -f1 -d" "`;
die "You need to run this as root" if !($id =~ "root");

# If we get here, we are running as root

# Don't actually kill the web server processes unless the argument is "really".
exit 0 if ( $arg ne "really" );

print "Executing kill -9 $pids\n";
my ( $kill ) = `kill -9 $pids`;
$pids = `httpdPids.pl`;
print "After killing apache, httpd pids now active: $pids\n";
