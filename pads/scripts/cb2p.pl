#!/usr/bin/env perl

use File::Basename;

# hash tables
my %orig_ids = ();
my %map_id = ();
my %alt_ids = ();
my %aux_arrays = ();
my %field_ids = ();
my %fieldgen = ();
my %altnm = ();
my %arraynm = ();
my %array_generated = ();
my %elt_type = ();
my %arr_count = ();
my %is_str = ();
my %redef = ();
my %dlengths = ();
my %dtylengths = ();

# arrays
my @lines;
my @defs;

# these arrays act like stacks
my @lev;
my @rest;
my @arrays;
my @dbytes;

# scalars
my $nst = 0;
my $altctr = 1;
my $level = 1;
my $get_top_id = 1;
my ($id, $popid, $id1, $id2, $line, $lineno, $the_rest);
my ($max, $array_param, $is_struct, $ty, $dlen);
my ($tmp, $root, $field, $def);
$lev[$nst] = -99;

goto usage if ($#ARGV != 1);
my $cbook  = $ARGV[0];
my $outdir = $ARGV[1];
$outdir =~ s:[/]$::;

chomp($pads_home = $ENV{'PADS_HOME'});
if ($pads_home eq "") {
  print "Env variable PADS_HOME must be set\n";
  exit(-1);
}

if ($cbook =~ /[.]int$/) {
  open(CBIN, "cat $cbook|") or die("failed to open $cbook\n");
} else {
  open(CBIN, "cat $cbook | $pads_home/scripts/cb.py|") or die("failed to open $cbook or failed to exec $pads_home/scripts/cb.py\n");
}

my $cprefix = basename($cbook);
$cprefix =~ s/[.](.*)//;
my $padsfile = "$outdir/$cprefix" . "_gen.p";

open(POUT, ">$padsfile") or die("failed to open $padsfile for writing\n");

push(@lines, "0 -2 __T_H_E_BEGINNING___0 1 none\n");
push(@lines, "0 -1 __T_H_E_TOP___0 1 none\n");
while (<CBIN>) {
  if ($get_top_id && /^\d+ \d+ (\S+)_\d+ /) {
    $id1 = $1;
    $top_id = $id1 . "_top";
    $get_top_id = 0;
  }
  push(@lines, $_);
}
push(@lines, "999999999 -99 __T_H_E_END___0 1 none\n");
close(CBIN) or die("failure while reading $cbook\n");

$lines[1] =~ s/__T_H_E_TOP__/$top_id/;

# remove extra numbers at end of IDs if they are unnecessary
foreach $line (@lines) {
  $_ = $line;
  if (/^\d+ [-]?\d+ (\S+)_\d+ /) {
    $id1 = $1;
    $orig_ids{$id1}++;
  }
}
foreach $line (@lines) {
  $_ = $line;
  if (/^(\d+) [-]?\d+ ((\S+)_\d+) /) {
    ($lineno, $id1, $id2) = ($1, $3, $2);
    if ($orig_ids{$id1} == 1) { # only one, remove the extension
      $map_id{$id2} = $id1;
    } else { # more than one, use lineno
      $map_id{$id2} = $id1 . "_ln_$lineno";
      print "XXX_REMOVE map_id{$id2} = $map_id{$id2}\n";
    }
  }
}

foreach $line (@lines) {
  $_ = $line;
  if (/^(\d+) r (\S+) (\S+)/) {
    ($lineno, $id1, $id2) = ($1, $2, $3);
    $id1 = $map_id{$id1};
    $id2 = $map_id{$id2};
    if (defined($redef{$id2})) {
      $root = $redef{$id1} = $redef{$id2};
    } else {
      $root = $redef{$id1} = $id2;
    }
    if (defined($alt_ids{$root})) {
      $alt_ids{$root} .= "|$id1";
    } else {
      $alt_ids{$root} .= "$root|$id1";
    }
    print "XXX_REMOVE Redefine:  $id1 is an alternate for previously-defined $id2\n";
    print "XXX_REMOVE  with root $root current alternatives: $alt_ids{$root}\n";
  }
}
foreach $line (@lines) {
  $_ = $line;
  if (/^(\d+) r (\S+) (\S+)/) {
    next;
  }
  if (/^(\d+) ([-]?\d+) (\S+)\s?(.*)/) {
    ($lineno, $level, $id1, $the_rest) = ($1, $2, $3, $4);
    $id1 = $map_id{$id1};
    if ($level <= $lev[$nst]) {
      print "XXX_REMOVE Changing from level $lev[$nst] to final level $level -- popping levels\n";
      while ($nst && $level <= $lev[$nst]) {
	$popid = $id[$nst];
	print "XXX_REMOVE -- popping nst $nst (lev $lev[$nst], id $popid, rest $rest[$nst])\n";
	if ($nst > 1) {
	  $parentid = $id[$nst-1];
	  if ($rest[$nst-1] !~ /none/) {
	    die("something is wrong, parent is not structured");
	  }
	  # parent is a struct or alternate, add a field and possibly an array declaration
	  ($max, $array_param, $is_struct, $ty, $dlen) = &eval_ty($popid, $rest[$nst]);
	  print "XXX_REMOVE popid $popid max is $max\n";
	  if ($max > 1) {
	    &add_array($ty);
	    $ty = $arraynm{$ty};
	    $aux_arrays{$parentid} .= "$ty#";
	    print "XXX_REMOVE aux_arrays{$parentid} now = $aux_arrays{$parentid}\n";
	    # final ty for array case
	    $ty = sprintf("%s(:%s:)", $ty, $array_param);
	  }			# else ty is OK as-is
	  $field = sprintf("    %-40s %25s // Field length:  %4s\n", $ty, $popid . ";", $dlengths{$popid});
	  $fieldgen{$popid} = $field;
	  $field_ids{$parentid} .= "$popid#";
	}
	if ($rest[$nst] =~ /none/) {
	  $arrays[$nst] =~ s/[\#]$//;
	  foreach my $a (split(/[\#]/, $arrays[$nst])) {
	    print POUT "$a\n";
	  }
	  if ($nst > 1) {
	    $fmt_arrays{$popid} = "";
	    $fmt_structs{$popid} = "";
	    push(@defs, "$popid");
	  }
	}
	$nst--;
      }
    }
    if ($level != -99) {
      print "XXX_REMOVE Changing from level $lev[$nst] to $level\n";
      $nst++;
      print "XXX_REMOVE -- pushed nst $nst (lev $level, id $id1, rest $the_rest)\n";
      $lev[$nst]    = $level;
      $id[$nst]     = $id1;
      $rest[$nst]   = $the_rest;
      $arrays[$nst] = "";
      $dbytes[$nst] = 0;
    }
  } else {
    die("unexpected input: $_");
  }
}

foreach $def (@defs) {
  &padsgen($def);
}
close(POUT) or die("failure while writing $padsfile\n");
exit(0);

usage:
print "\nusage: cb2p <copybook> <outdir>\n\n";
exit(-1);

# ==============================
# Subroutine: blen($type, $p1, $p2)
#   returns length in bytes
sub blen
{
  my ($type, $p1, $p2) = @_;
  my ($p, $rv, $word);

  return $p1                if ($type eq "X");  # chars
  return $p1                if ($type eq "i");  # uns. numbers     PIC 9999 DISPLAY
  return $p1                if ($type eq "is"); # signed numbers   PIC S999 DISPLAY
  return $p1+$p2            if ($type eq "f");  # uns. fixed point PIC 9v99 DISPLAY
  return $p1+$p2            if ($type eq "fs"); # signed fixed pt  PIC S9V9 DISPLAY
  return int(($p1+2)/2)     if ($type eq "I");  # bcd + (non)sign  PIC 9999 PACKED-
  return int(($p1+2)/2)     if ($type eq "Is"); # bcd + sign       PIC S999 PACKED-
  return int(($p1+$p2+2)/2) if ($type eq "F");  # signed bcd       PIC S9V9 PACKED-
  if ($type =~ /[bB]/) {
    # some binary type...
    $p = (10 ^ ($p1 + $p2)) -1;                   # largest number described by pic
    if ($p < 65536) {                             # half word if possible
      return 2;
    }
    $word = 4294967295;                           # largest unsigned # in 4 bytes
    for ($rv = 4; $p > $word; $p /= $word) {      # count # words needed for
      $rv += 4;                                   # the largest number
    }
    return $rv;                                   # and return as length
  }
  # unrecognized type - error
  return 0;
}

# ==============================
# Subroutine: padsbasety($type, $p1, $p2)
#   returns ($ty, $tlen)
#
#  returns instantiated PADS type and on-disk length
sub padsbasety
{
  my ($type, $p1, $p2) = @_;
  my ($ty, $tlen);

  $tlen = &blen($type, $p1, $p2);

  if ($type eq "X") {
    # chars
    $ty = sprintf("Pe_string_FW(:%d:)", $p1);
  } elsif ($type eq "is") {
    # signed numbers   PIC S999 DISPLAY
    $ty = sprintf("Pebc_int64(:%d:)", $p1);
  } elsif ($type eq "i") {
    # uns. numbers     PIC 9999 DISPLAY
    $ty = sprintf("Pebc_uint64(:%d:)", $p1);
  } elsif ($type eq "fs") {
    # signed fixed pt  PIC S9V9 DISPLAY
    $ty = sprintf("Pebc_fpoint64(:%d,%d:)", $p1+$p2, $p2);
  } elsif ($type eq "f") {
    # uns. fixed point PIC 9v99 DISPLAY
    $ty = sprintf("Pebc_ufpoint64(:%d,%d:)", $p1+$p2, $p2);
  } elsif ($type eq "Is") {
    # bcd + sign       PIC S999 PACKED-
    $ty = sprintf("Pbcd_int64(:%d:)", $p1);
  } elsif ($type eq "I") {
    # bcd + (non)sign  PIC 9999 PACKED-
    $ty = sprintf("Pbcd_int64(:%d:)", $p1);
  } elsif ($type eq "F") {
    # signed bcd       PIC S9V9 PACKED-
    $ty = sprintf("Pbcd_fpoint64(:%d,%d:)", $p1+$p2, $p2);
  } elsif ($type eq "bs") {
    # signed binary
    $ty = sprintf("Psbh_int64(:%d:)", $tlen);
  } elsif ($type eq "b") {
    # uns binary
    $ty = sprintf("Psbh_uint64(:%d:)", $tlen);
  } elsif ($type eq "Bs") {
    # signed binary fixedpoint
    $ty = sprintf("Psbh_fpoint64(:%d,%d:)", $tlen, $p2);
  } elsif ($type eq "B") {
    # uns. binary fixedpoint
    $ty = sprintf("Psbh_ufpoint64(:%d,%d:)", $tlen, $p2);
  } else {
    # error
    die("XXX should not get here, failure in padsbasety($type,$p1,$p2)\n");
  }
  return ($ty, $tlen);
}

# ==============================
# Subroutine: str_or_alt($id)
#   returns (type name to use for struct or alternate, "struct" or "alt")
sub str_or_alt
{
  my ($id) = @_;
  if (defined($redef{$id})) {
    if (!defined($altnm{$redef{$id}})) {
      $altnm{$redef{$id}} = sprintf("Alt_%d_T", $altctr++);
    }
    return ($altnm{$redef{$id}}, "alt");
  }
  return ($id . "_T", "struct");
}

# ==============================
# Subroutine: add_dlen($dlen1, $dlen2)
#
sub add_dlen
{
  my ($dlen1, $dlen2) = @_;
  return "var" if ($dlen1 eq "var" || $dlen2 eq "var");
  return $dlen1 + $dlen2;
}

# ==============================
# Subroutine: mul_dlen($dlen1, $dlen2)
#
sub mul_dlen
{
  my ($dlen1, $dlen2) = @_;
  return "var" if ($dlen1 eq "var" || $dlen2 eq "var");
  return $dlen1 * $dlen2;
}

# ==============================
# Subroutine: max_dlen($dlen1, $dlen2)
#
sub max_dlen
{
  my ($dlen1, $dlen2) = @_;
  return "var" if ($dlen1 eq "var" || $dlen2 eq "var");
  return ($dlen1 > $dlen2) ? $dlen1 : $dlen2;
}

# ==============================
# Subroutine: eval_ty($id, $rest)
#   returns ($max, $array_param, $is_struct, $ty, $dlen)
#
#    if $is_struct == 1, $ty is a structured type
#      otherwise $ty is an instantiated PADS base type
#    if $max is > 1, this elt is an array with elt type $ty
#      (the array should be instantiated with $array_param)
#    $dlen is only set for $is_struct == 0, and it is
#       either "var" or the number of byte on disk
#       ($array_param * sizeof($ty))

sub eval_ty
{
  my ($id, $rest) = @_;
  my ($max, $array_param, $is_struct, $ty, $dlen, $dtylen, $s_or_a);
  my ($num, $min, $var_name, $type, $p1, $p2, $tlen);
  my ($fids, $fid, $aids, $aid);

  if ($rest =~ /(\S+) (\S+) (\S+) (\S+)/) {
    ($num, $type, $p1, $p2) = ($1, $2, $3, $4);
  } elsif ($rest =~ /(\S+) (\S+) (\S+)/) {
    ($num, $type, $p1, $p2) = ($1, $2, $3, "none");
  } elsif ($rest =~ /(\S+) (\S+)/) {
    ($num, $type, $p1, $p2) = ($1, $2, "none", "none");
  } else {
    ($num, $type, $p1, $p2) = ($rest, "none", "none", "none");
  }

  # $num is either exact # or max!min!var_name
  my @aa = split(/[!]/, $num);
  ($max, $min, $array_param) = ($aa[0], $aa[1], $aa[2]);
  if ($array_param eq "") {
    $array_param = $max;
    $min = $max;
  } elsif (defined($map_id{$array_param})) {
    $array_param = $map_id{$array_param};
  }
  if ($type =~ /none/) {
    $dtylen = 0;
    $is_struct = 1;
    ($ty, $s_or_a) = &str_or_alt($id);
    if ($s_or_a eq "struct") {
      $fids = $field_ids{$id};
      $fids =~ s/[\#]$//;
      foreach $fid (split(/[\#]/, $fids)) {
	$dtylen = &add_dlen($dtylen, $dlengths{$fid});
      }
    } else {
      $aids = $alt_ids{$id};
      $aids =~ s/[\#]$//;
      foreach $aid (split(/[\#]/, $aids)) {
	$dtylen = &max_dlen($dtylen, $dlengths{$aid});
      }
    }
    if ($min != $max) {
      $dlen = "var";
    } else {
      $dlen = &mul_dlen($dtylen, $max);
    }
    $dtylengths{$id} = $dtylen;
  } else {
    $is_struct = 0;
    ($ty, $tlen) = &padsbasety($type, $p1, $p2);
    if ($min != $max) {
      $dlen = "var";
    } else {
      $dlen = $max * $tlen;
    }
  }
  $dlengths{$id} = $dlen;

  print "XXX_REMOVE eval_ty(id $id, rest $rest) returning (max $max, array_param $array_param, is_struct $is_struct, ty $ty, dlen $dlen)\n";
  return ($max, $array_param, $is_struct, $ty, $dlen);
}

# ==============================
# Subroutine: padsgen($def)
#   emit PADS code for $def

sub padsgen
{
  my ($def) = @_;

  if (defined($alt_ids{$def})) {
    &padsgen_alt($def);
  } else { # a normal Pstruct
    &padsgen_str($def);
  }
}

# ==============================
# Subroutine: padsgen_alt($def)
#   emit Palternates decl for $def
#   (preceded by any aux arrays and structs)

sub padsgen_alt
{
  my ($def) = @_;
  my ($aids, $aid);
  $aids = $alt_ids{$def};
  $aids =~ s/[\#]$//;
  my @split_alts = split(/[\#]/, $aids);
  foreach $aid (@split_alts) {
    if ($is_str{$aid}) {
      &padsgen_str($aid);
    } else {
      &padsgen_aux_arrays($aid);
    }
  }
  print POUT "Palternates $altnm{$def} {\n";
  foreach $aid (@split_alts) {
    print POUT $fieldgen{$aid};
  }
  print POUT "};\n\n";
}

# ==============================
# Subroutine: padsgen_str($def)
#   emit Pstruct decl for $def
#   (preceded by any aux arrays)

sub padsgen_str
{
  my ($def) = @_;
  my ($stnm, $fids, $fid);
  $fids = $field_ids{$def};
  $fids =~ s/[\#]$//;
  $stnm = $def . "_T";
  &padsgen_aux_arrays($def);
  print POUT "Pstruct $stnm \{\n";
  foreach $fid (split(/[\#]/, $fids)) {
    print POUT $fieldgen{$fid};
  }
  printf POUT "};                                                                     // Struct length: %4s\n\n", $dtylengths{$def};
}

# ==============================
# Subroutine: padsgen_aux_arrays($def)
#   emit aux Parray decls that precede $def, if necessary

sub padsgen_aux_arrays
{
  my ($def) = @_;
  my ($ars, $ar);
  $ars = $aux_arrays{$def};
  $ars =~ s/[\#]$//;
  foreach $ar (split(/[\#]/, $ars)) {
    if (!defined($array_generated{$ar})) {
      $array_generated{$ar} = 1;
      print  POUT "Parray $ar (:unsigned int len:) {\n";
      printf POUT "    %s [len];\n", $elt_type{$ar};
      print  POUT "};\n\n";
    }
  }
}

# ==============================
# Subroutine: add_array($ty)
#   returns 1 if had to add an aux array, else 0

sub add_array
{
  my ($ty) = @_;
  return 0 if (defined($arraynm{$ty}));
  my $anm = "array_of_$ty";
  $anm =~ s/\s//g;
  $anm =~ s/[(][:]/_/g;
  $anm =~ s/[:][)]//g;
  $anm =~ s/[,]/_/g;
  $arraynm{$ty} = $anm;
  $elt_type{$anm} = $ty;
  return 1;
}
