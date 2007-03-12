#!/usr/bin/perl

# My apologies for adding a perl script to the make process. Having
# compilation manager (CM) and make together are enough complexity
# without having a perl script thrown in. However, I want to record
# a table a maximum depth parameters for the different test cases, and
# I want it stored in a way that is useable in commands, and I could
# not find good way to do this using only make. I considered a shell
# script, but if you have to write a script, might as well use a real
# scripting language.

#----------------------------------------------------------------------
# Determine the maximum depth for a test case
#----------------------------------------------------------------------

$maxdepth{"1967Transactions.short"}    = "";
$maxdepth{"MER_T01_01.csv"}            = "";
$maxdepth{"ProfSec.log"}               = "";
$maxdepth{"SCESETUP.LOG"}              = "-maxdepth 4";
$maxdepth{"access_log"}                = "";
$maxdepth{"ai-simple"}                 = "";
$maxdepth{"ai.3000"}                   = "";
$maxdepth{"apache.txt"}                = "-maxdepth 9";
$maxdepth{"asl.log"}                   = "ERROR TOO LONG";
$maxdepth{"backup1.log"}               = "";
$maxdepth{"backup2.log"}               = "";
$maxdepth{"boot.log"}                  = "";
$maxdepth{"crashreporter.log"}         = "";
$maxdepth{"daily.out"}                 = "-maxdepth 4";
$maxdepth{"dibbler.1000"}              = "";
$maxdepth{"dmesg"}                     = "-maxdepth 6";
$maxdepth{"error_log"}                 = "-maxdepth 4";
$maxdepth{"hp-array"}                  = "";
$maxdepth{"hp-struct"}                 = "";
$maxdepth{"ijnltif"}                   = "-maxdepth 3";
$maxdepth{"install.log"}               = "-maxdepth 7";
$maxdepth{"interactions.txt.small"}    = "-maxdepth 6";
$maxdepth{"interface.loop"}            = "-maxdepth 4";
$maxdepth{"lsof"}                      = "-maxdepth 4";
$maxdepth{"netstat"}                   = "";
$maxdepth{"netstat-an"}                = "";
$maxdepth{"page_log"}                  = "";
$maxdepth{"quarterlypersonalincome"}   = "";
$maxdepth{"railroad.txt"}              = "";
$maxdepth{"rpmpkgs"}                   = "";
$maxdepth{"scesetup2.log"}             = "";
$maxdepth{"scrollkeeper.log"}          = "";
$maxdepth{"simplegroups"}              = "";
$maxdepth{"simplexml"}                 = "";
$maxdepth{"windowserver_last.log"}     = "";
$maxdepth{"yum.log"}                   = "";
$maxdepth{"yum.txt"}                   = "";

# Print out the maximum depth parameter to pass to ./learn
print "$maxdepth{$ARGV[0]}";
