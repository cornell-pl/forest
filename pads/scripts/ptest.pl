#!/usr/bin/env perl

my $pads_home = $ENV{"PADS_HOME"};
if ($pads_home eq "") {
  print "\n    env variable PADS_HOME must be set";
  print "\n    set PADS_HOME and try again\n\n";
  exit(-1);
}
goto usage if ($#ARGV < 1 || $#ARGV > 2);
my $package = "$pads_home/ast-ast/bin/package.cvs";
my $arch = `$package` || die "\n    Could not execute $package\n\n";
chomp($arch);
my $mtmplf = "$pads_home/scripts/templates/GNUmakefile.ptest";
open (MTMPL, "$mtmplf") || die "\n    Could not open template file $mtmpl\n\n";
my $mtmpl = "";
while (<MTMPL>) {
  $mtmpl .= $_;
}
my $pspec = $ARGV[0];
$pspec =~ s|.*[\/\\]||g;
my $ppath = $ARGV[0];
$ppath =~ s|[\/\\]?[^\/\\]*$||;
if ($ppath eq "") {
  $ppath = ".";
}
if (! ($pspec =~ /.[.]p$/)) {
  print "\n    pspec must have the form [<dirpath>]foo.p\n";
  goto usage;
}
if (! (-e "$ppath/$pspec")) {
  print "\n    pspec $ppath/$pspec does not exist\n";
  goto usage;
}
my $pname = $pspec;
$pname =~ s/[.]p$//;
my $pspec_c = "$pname.c";
my $pspec_h = "$pname.h";
my $pty = $ARGV[1];
my $maxrecs = ($#ARGV == 2) ? $ARGV[2] : 0;
my $mfile = "tmp_GNUmakefile.ptest";
my $efile = "tmp_rwxml_$pname";
my $cfile = "$efile.c";
my $gendir = ".";
# XXX_TODO if exists ./gen or ../gen then set gendir to that directory

print "XXX_REMOVE pspec = $pspec\n";
print "XXX_REMOVE ppath = $ppath\n";
print "XXX_REMOVE pname = $pname\n";
print "XXX_REMOVE pspec_c = $pspec_c\n";
print "XXX_REMOVE pspec_h = $pspec_h\n";
print "XXX_REMOVE pty = $pty\n";
print "XXX_REMOVE maxrecs = $macrecs\n";
print "XXX_REMOVE mfile = $mfile\n";
print "XXX_REMOVE efile = $efile\n";
print "XXX_REMOVE cfile = $cfile\n";
print "\n";

$mtmpl =~ s/=GENDIR=/$gendir/g;
$mtmpl =~ s/=PPATH=/$ppath/g;
$mtmpl =~ s/=EFILE=/$efile/g;


open (MOUT, ">$mfile") || die "\n    Could not open $mfile for output\n\n";
print MOUT $mtmpl;
close (MOUT);

open (COUT, ">$cfile") || die "\n    Could not open $cfile for output\n\n";
print COUT "#define PADS_TY(suf) $pty ## suf
#define IO_DISC_MK P_nlrec_make(0)
#include \"$pspec_h\"
#include \"template/read_orig_write_xml.h\"

";
close(COUT);

print "\nBuilding test program\n";
my $res = `gmake -f $mfile 2>&1` || die "\n    Build of test program failed\n\n";
if ($res =~ /\`?$pty\'?\s+undeclared/is) {
  print "\nBuild of test program failed, ptype $pty not found\n\n";
  exit(-1);
}
if ($res =~ /error/is) {
  print "\n$res\n\nBuild of test program failed, see error(s) above\n\n";
  exit(-1);
}
print "\nRunning test program.  Input is stdin, you must hit ^D to terminate input and get results.\n\n";
my $res2 = `$arch/$efile 2>&1`;
$res2 =~ s|Input file = /dev/stdin||s;
$res2 =~ s|Output file = /dev/stdout||s;
while ($res2 =~ m|\n\n|) {
  $res2 =~ s|\n\n|\n|sg;
}
print "\nResult:\n$res2\n";
exit(0);

usage:
print "\n    usage:  ptest.pl <pspec> <ptype> [ <maxrecs> ]\n\n";
exit(-1);

