#!/usr/bin/env perl

# =========================
sub readFile
{
    my( $name ) = @_;
    my $data = "";
    undef $/; # set to read to EOF
    open( IN_FILE, "<$name" ) || return "";
    $data = <IN_FILE>;
    $/ = "\n";
    close( IN_FILE );
    return $data;
}

# =========================
sub cleanup
{
    my( $txt ) = @_;
    chomp($txt);
    $txt =~ s/\s//g;
    return $txt;
}

# =========================
# main program

my $dbg=0;

my $pads_home = $ENV{"PADS_HOME"};
if ($pads_home eq "") {
  # try to find PADS_HOME from script path
  if ($0 =~ m|/getprobeinfo.pl$|) {
    my $path = $0;
    $path =~ s|/getprobeinfo.pl$||;
    $path .= "/..";
    $pads_home = `(cd $path; pwd)` || "";
    $pads_home = &cleanup($pads_home);
    print "\n*** Guessing PADS_HOME = [$pads_home]\n\n" if ($dbg);
  }
}
if ($pads_home eq "") {
  print "\n    Error: either env variable PADS_HOME must be set
           OR the invocation of this script must have a path component
           so that the script can figure out PADS_HOME

    Set PADS_HOME or use a relative or absolute path to the script and try again\n\n";
  exit -1;
}

my $ast_arch = `$pads_home/ast-ast/bin/package.cvs`;
$ast_arch = &cleanup($ast_arch);
my $ast_home = "$pads_home/ast-ast/arch/$ast_arch";
my $targ = "$pads_home/mk/foo.rules.arch.$ast_arch.mk";

if ($dbg) {
  print "   ast_arch = $ast_arch\n";
  print "   ast_home = $ast_home\n";
  print "   targ     = $targ\n";
}

if (-e $targ) {
  my $text = &readFile($targ);
  if ($text !~ /mam/) {
    # targ does not look valid, remove it
    my $res = `/bin/rm -f $targ` || "oops";
  }
}

if (! -e $targ) {
  my $probe_dir = "$ast_home/lib/probe/C/mam";
  my $probe_file = `grep -l mam_cc_DEBUG $probe_dir/* | head -1`;
  $probe_file = &cleanup($probe_file);
  print "probe_file = $probe_file\n" if ($dbg);
  open(INP, $probe_file) or die "\nCould not open file $probe_file for reading\n\n";
  my @txt = ();
  my $is_gnu = 0;
  while (<INP>) {
    chomp;
    if (/GNU/) { $is_gnu = 1; }
    next if /generated by/;
    next if /$\s+^/;
    s/setv //;
    s/$/ /;
    s/CC.SHARED.REGISTRY.PATH/mam_cc_SHARED_REGISTRY_PATH/g;
    s|\$\(BINDIR:P=R=\$\(DLLDIR\)\)|../lib|g;
    s/\\\$(\S+)/\'\$\$$1\'/g;
    s/ /=/;
    s/ $//;
    push(@txt, $_);
  }
  open (TARG, ">$targ") or die "\nFailed to open file $targ for writing\n\n";
  print TARG join("\n", @txt) . "\n";
  if ($is_gnu) {    # mark as GNU
    print TARG "PADS_CC_IS_GNU=1\n";
  }
  close (TARG);
}
print "done\n";
