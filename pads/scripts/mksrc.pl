#!/usr/bin/env perl

goto usage if ($#ARGV != 2);
my $ifilename = $ARGV[0];
my $odir = $ARGV[1];
my $pads_home = $ARGV[2];
my $sdir = "$pads_home/scripts";

my $cc;
chomp($cc = $ENV{'CC'});
if (length($cc) == 0) {
    chomp($cc = `type gcc 2>/dev/null`);
    $cc =~ s/gcc is//;
}
if (length($cc) == 0) {
  chomp($cc = `type cc 2>/dev/null`);
    $cc =~ s/cc is//;
}
if (length($cc) == 0) {
  print "\n** No environment variable CC, and no gcc or cc in your path.\n** Please fix and re-run make.\n\n";
  exit -1;
}

my $cc_include =  "-I $pads_home/padsc/include";

open(IFILE, $ifilename) || die "Could not open input file $ifilename\n";

$defgen = 0;
top: while (<IFILE>) {
  s/\#gen_include/\#include/g;
  if (/DEFGEN\((.*)\)/) {
    $defgenfile = $1;
    open(DFILE, ">$odir/tmp1.$defgenfile") || die "could not open $odir/tmp1.$defgenfile\n";
    $defgen = 1;
    if ($defgenfile =~ /[.]h$/) {
      print DFILE "#ifdef _USE_PROTO\n";
      print DFILE "#pragma prototyped\n";
      print DFILE "#endif\n";
    }
    print DFILE "/*\n * WARNING: GENERATED FILE.  Do not edit this file, edit $ifilename instead. \n */\n\n";
    next;
  }
  if (/BEGIN_MACGEN\((.*)\)/) {
    $mgenfile = $1;
    open(MGFILE, ">$odir/tmp1.$mgenfile") || die "could not open $odir/tmp1.$mgenfile\n";
    if ($mgenfile =~ /[.]h$/) {
      print MGFILE "#ifdef _USE_PROTO\n";
      print MGFILE "#pragma prototyped\n";
      print MGFILE "#endif\n";
    }
    print MGFILE "/*\n * WARNING: GENERATED FILE.  Do not edit this file, edit $ifilename instead. \n */\n\n";
    open(MGTMPFILE, ">$odir/tmp2.$mgenfile") || die "could not open $odir/tmp2.$mgenfile\n";
    while (<IFILE>) {
      s/\#gen_include/\#include/g;
      if (/END_HEADER/) {
	goto end_macgen_header;
      }
      print MGFILE;
    }
    die "END_HEADER not found while building $mgenfile";
  end_macgen_header:
    close(MGFILE);
    while (<IFILE>) {
      s/\#gen_include/\#include/g;
      if (/BEGIN_TRAILER/) {
	goto begin_macgen_trailer;
      }
      print MGTMPFILE;
    }
    die "BEGIN_TRAILER not found while building $mgenfile";
  begin_macgen_trailer:
    close(MGTMPFILE);
    $cmd = "$cc -E $odir/tmp2.$mgenfile $cc_include | $sdir/addnl.pl | cat >> $odir/tmp1.$mgenfile 2>&1";
    $res = `$cmd`;
    if ($res =~ /ERROR/) {
      die "Error running command $cmd\n";
    }
    open(MGFILE, ">>$odir/tmp1.$mgenfile") || die "could not open $odir/tmp1.$mgenfile\n";
    while (<IFILE>) {
      s/\#gen_include/\#include/g;
      if (/END_MACGEN/) {
	goto end_macgen;
      }
      print MGFILE;
    }
    die "END_MACGEN not found while building $mgenfile";
  end_macgen:
    close(MGFILE);
    $cmd = "cat $odir/tmp1.$mgenfile | $sdir/stripnlnl.pl | cat > $odir/$mgenfile";
    $res = `$cmd`;
    if ($res =~ /ERROR/) {
      die "Error running command $cmd\n";
    }
    next top;
  }
  if (/BEGIN_MACROS\((.*)\)/) {
    $macfile = $1;
    $defname = uc($macfile);
    $defname =~ s/\-/_/g;
    $defname =~ s/\./_/g;
    $defname = "__$defname"."__";
    open(MFILE, ">$odir/tmp1.$macfile") || die "could not open $odir/tmp1.$macfile\n";
    if ($macfile =~ /[.]h$/) {
      print MFILE "#ifdef _USE_PROTO\n";
      print MFILE "#pragma prototyped\n";
      print MFILE "#endif\n";
    }
    print MFILE "/*\n * WARNING: GENERATED FILE.  Do not edit this file, edit $ifilename instead. \n */\n\n";
    print MFILE "#ifndef $defname\n#define $defname\n\n";
    while (<IFILE>) {
      s/\#gen_include/\#include/g;
      if (/END_HEADER/) {
	goto end_mac_header;
      }
      print MFILE;
    }
    die "END_HEADER not found while building $macfile";
  end_mac_header:
    $seen_define = 0;
    while (<IFILE>) {
      s/\#gen_include/\#include/g;
      if (/BEGIN_TRAILER/) {
	goto begin_mac_trailer;
      }
      chomp;
      if (/\#define/) {
	print MFILE "\n\n$_ \\\n";
	$seen_define = 1;
      } elsif (/END_MACRO/) {
	$seen_define = 0;
      } elsif ($seen_define) {
	print MFILE "$_ \\\n";
      } else {
	print MFILE "$_\n";
      }
    }
    die "BEGIN_TRAILER not found while building $macfile";
  begin_mac_trailer:
    print MFILE "\n\n";
    while (<IFILE>) {
      s/\#gen_include/\#include/g;
      if (/END_MACROS/) {
	goto end_mac;
      }
      print MFILE;
    }
    die "END_MACROS not found while building $macfile";
  end_mac:
    print MFILE "\n#endif  /*  $defname  */\n\n";
    close(MFILE);
    $cmd = "cat $odir/tmp1.$macfile | $sdir/stripnlnl.pl | cat > $odir/$macfile";
    $res = `$cmd`;
    if ($res =~ /ERROR/) {
      die "Error running command $cmd\n";
    }
    next top;
  }
  if ($defgen == 1) {
    print DFILE;
  } else {
    if (/\#\#/) {
      # comment line at the top -- do nothing
    } else {
      chomp;
      if (length($_) > 0) {
	print "Warning: DEFGEN not specified, so this line has no home:\n$_\n";
      }
    }
  }
  next top;
}
if ($defgen == 1) {
  close(DFILE);
  $cmd = "cat $odir/tmp1.$defgenfile | $sdir/stripnlnl.pl | cat > $odir/$defgenfile";
  $res = `$cmd`;
  if ($res =~ /ERROR/) {
    die "Error running command $cmd\n";
  }
}
$cmd = "/bin/rm -f $odir/tmp[12].*";
$res = `$cmd`;
if ($res =~ /ERROR/) {
  die "Error running command $cmd\n";
}

exit 0;

usage:
 die "Usage: mksrc.pl <infile> <target_dir> <PADS_HOME>";
