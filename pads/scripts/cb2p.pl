#!/usr/bin/env perl

use File::Basename;

# hash tables
my %controls_array = ();
my %orig_ids = ();
my %map_id = ();
my %first_alt = ();
my %last_alt = ();
my %aux_arrays = ();
my %field_ids = ();
# my %fieldgen = ();
my %arraynm = ();
my %array_generated = ();
my %elt_type = ();
my %arr_count = ();
my %is_str = ();
my %redef = ();
my %dfieldlength = ();
my %dtylength = ();

# arrays
my @lines;
my @defs;

# these arrays act like stacks
my @lev;
my @rest;
my @arrays;
my @dbytes;

# scalars
my $dbg = 0;
my $nst = 0;
my $altctr = 1;
my $level = 1;
my $get_top_id = 1;
my ($popid, $id1, $id2, $line, $lineno, $the_rest);
my ($max, $array_param, $ty);
my ($tmp, $the_alt, $field, $def);
my ($targetid, $parentid);
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
      print "XXX_REMOVE map_id{$id2} = $map_id{$id2}\n" if ($dbg);
    }
    $is_alt{$map_id{$id2}} = 0;
  }
}

foreach $line (@lines) {
  $_ = $line;
  if (/^(\d+) r (\S+) (\S+)/) {
    ($lineno, $id1, $id2) = ($1, $2, $3);
    $id1 = $map_id{$id1};
    $id2 = $map_id{$id2};
    if (!defined($redef{$id2})) {
      # found a new alt
      $the_alt = sprintf("alt_%d", $altctr++);
      $is_alt{$the_alt} = 1;
      $redef{$id2} = $the_alt;
      $first_alt{$the_alt} = $id2;
    } else { # add to existing ALT chain
      $the_alt = $redef{$id2};
    }
    $last_alt{$the_alt} = $id1;
    $redef{$id1} = $the_alt;
    print "XXX_REMOVE Redefine:  $id1 is an alternate for previously-defined $id2, alt = $the_alt\n" if ($dbg);
  }
  if (/\d+[!]\d+[!](\S+) /) {
    $id1 = $1;
    $controls_array{$id1} = 1;
    print "XXX_REMOVE controls_array{$id1} = 1\n" if ($dbg);
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
      print "XXX_REMOVE Changing from level $lev[$nst] to final level $level -- popping levels\n" if ($dbg);
      while ($nst && $level <= $lev[$nst]) {
	$popid = $id[$nst];
	print "XXX_REMOVE -- popping nst $nst (lev $lev[$nst], id $popid, rest $rest[$nst])\n" if ($dbg);
	if ($nst > 1) {
	  $parentid = $id[$nst-1];
	  $targetid = $parentid;
	  if (defined($redef{$popid})) {
	    $targetid = $redef{$popid};
	  }
	  if ($rest[$nst-1] !~ /none/) {
	    die("something is wrong, parent is not structured");
	  }
	  # parent is a struct, add a field and possibly an array declaration
	  ($max, $array_param, $ty) = &eval_ty($popid, $rest[$nst]);
	  print "XXX_REMOVE popid $popid max is $max\n" if ($dbg);
	  if ($max > 1) {
	    &add_array($ty);
	    $ty = $arraynm{$ty};
	    $aux_arrays{$targetid} .= "$ty#";
	    print "XXX_REMOVE aux_arrays{$targetid} now = $aux_arrays{$targetid}\n" if ($dbg);
	    # final ty for array case
	    $ty = sprintf("%s(:%s:)", $ty, $array_param);
	  }			# else ty is OK as-is
	  $fieldty{$popid} = $ty;
	  # $field = sprintf("    %-40s %25s // Field length:  %4s\n", $ty, $popid . ";", $dfieldlength{$popid});
	  # $fieldgen{$popid} = $field;
	  $field_ids{$targetid} .= "$popid#";
	  if ($first_alt{$targetid} eq $popid) {
	    $ty = $targetid . "_t";
	    $fieldty{$targetid} = $ty;
	    # $field = sprintf("    %-40s %25s // Field length:  %4s\n", $ty, $targetid . ";", $dfieldlength{$targetid});
	    # $fieldgen{$targetid} = $field;
	    $field_ids{$parentid} .= "$targetid#";
	  }
	}
	if ($rest[$nst] =~ /none/) {
	  if ($nst > 1) {
	    push(@defs, "$popid");
	    if (defined($last_alt{$targetid}) && $last_alt{$targetid} eq $popid) {
	      push(@defs, "$targetid");
	      &calc_alt_lengths($targetid);
	    }
	  }
	}
	$nst--;
      }
    }
    if ($level != -99) {
      print "XXX_REMOVE Changing from level $lev[$nst] to $level\n" if ($dbg);
      $nst++;
      print "XXX_REMOVE -- pushed nst $nst (lev $level, id $id1, rest $the_rest)\n" if ($dbg);
      $lev[$nst]    = $level;
      $id[$nst]     = $id1;
      $rest[$nst]   = $the_rest;
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
    $ty = sprintf("Pebc_int%d(:%d:)", ($p1<10 ? 32 : 64), $p1);
  } elsif ($type eq "i") {
    # uns. numbers     PIC 9999 DISPLAY
    $ty = sprintf("Pebc_uint%d(:%d:)", ($p1<10 ? 32 : 64), $p1);
  } elsif ($type eq "fs") {
    # signed fixed pt  PIC S9V9 DISPLAY
    $ty = sprintf("Pebc_fpoint%d(:%d,%d:)", ($p1+$p2<10 ? 32 : 64), $p1+$p2, $p2);
  } elsif ($type eq "f") {
    # uns. fixed point PIC 9v99 DISPLAY
    $ty = sprintf("Pebc_ufpoint%d(:%d,%d:)", ($p1+$p2<10 ? 32 : 64), $p1+$p2, $p2);
  } elsif ($type eq "Is") {
    # bcd + sign       PIC S999 PACKED-
    $ty = sprintf("Pbcd_int%d(:%d:)", ($p1<10 ? 32 : 64), $p1);
  } elsif ($type eq "I") {
    # bcd + (non)sign  PIC 9999 PACKED-
    $ty = sprintf("Pbcd_int%d(:%d:)", ($p1<10 ? 32 : 64), $p1);
  } elsif ($type eq "F") {
    # signed bcd       PIC S9V9 PACKED-
    $ty = sprintf("Pbcd_fpoint%d(:%d,%d:)", ($p1+$p2<10 ? 32 : 64), $p1+$p2, $p2);
  } elsif ($type eq "bs") {
    # signed binary
    $ty = sprintf("Psbh_int%d(:%d:)", ($tlen<5 ? 32 : 64), $tlen);
  } elsif ($type eq "b") {
    # uns binary
    $ty = sprintf("Psbh_uint%d(:%d:)", ($tlen<5 ? 32 : 64), $tlen);
  } elsif ($type eq "Bs") {
    # signed binary fixedpoint
    $ty = sprintf("Psbh_fpoint%d(:%d,%d:)", ($tlen<5 ? 32 : 64), $tlen, $p2);
  } elsif ($type eq "B") {
    # uns. binary fixedpoint
    $ty = sprintf("Psbh_ufpoint%d(:%d,%d:)", ($tlen<5 ? 32 : 64), $tlen, $p2);
  } else {
    # error
    die("XXX should not get here, failure in padsbasety($type,$p1,$p2)\n");
  }
  return ($ty, $tlen);
}

# ==============================
# Subroutine: dlen_add($dlen1, $dlen2)
#
sub dlen_add
{
  my ($dlen1, $dlen2) = @_;
  return "var" if ($dlen1 eq "var" || $dlen2 eq "var");
  return $dlen1 + $dlen2;
}

# ==============================
# Subroutine: dlen_mul($dlen1, $dlen2)
#
sub dlen_mul
{
  my ($dlen1, $dlen2) = @_;
  return "var" if ($dlen1 eq "var" || $dlen2 eq "var");
  return $dlen1 * $dlen2;
}

# ==============================
# Subroutine: dlen_max($dlen1, $dlen2)
#
sub dlen_max
{
  my ($dlen1, $dlen2) = @_;
  return "var" if ($dlen1 eq "var" || $dlen2 eq "var");
  return ($dlen1 > $dlen2) ? $dlen1 : $dlen2;
}

# ==============================
# Subroutine: eval_ty($id, $rest)
#   returns ($max, $array_param, $ty)
#
#    $ty is a struct type name
#      or an instantiated PADS base type
#    if $max is > 1, this elt is an array with elt type $ty
#      (the array should be instantiated with $array_param)

sub eval_ty
{
  my ($id, $rest) = @_;
  my ($max, $array_param, $ty, $dlen, $dtylen, $s_or_a);
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
    $ty = $id . "_t";
    $dtylen = &calc_struct_length($id);
    if ($min != $max) {
      $dlen = "var";
    } else {
      $dlen = &dlen_mul($dtylen, $max);
      print "XXX_REMOVE id $id dtylen $dtylen dlen $dlen\n" if ($dbg);
    }
  } else {
    ($ty, $tlen) = &padsbasety($type, $p1, $p2);
    if ($min != $max) {
      $dlen = "var";
    } else {
      $dlen = $max * $tlen;
    }
  }
  $dfieldlength{$id} = $dlen;

  print "XXX_REMOVE eval_ty(id $id, rest $rest) returning (max $max, array_param $array_param, ty $ty)\n" if ($dbg);
  return ($max, $array_param, $ty);
}

# ==============================
# Subroutine: calc_struct_length($def)
#    calculate $dtylength{$def}

sub calc_struct_length
{
  my ($def) = @_;
  my $dlen = 0;

  $fids = $field_ids{$def};
  $fids =~ s/[\#]$//;
  foreach $fid (split(/[\#]/, $fids)) {
    $dlen = &dlen_add($dlen, $dfieldlength{$fid});
  }
  $dtylength{$def} = $dlen;
  return $dlen;
}

# ==============================
# Subroutine: calc_alt_lengths($def)
#    calculate $dtylength{$def}
#   which is also $dfieldlength{$def}

sub calc_alt_lengths
{
  my ($def) = @_;
  my $dlen = 0;

  $fids = $field_ids{$def};
  $fids =~ s/[\#]$//;
  foreach $fid (split(/[\#]/, $fids)) {
    $dlen = &dlen_max($dlen, $dfieldlength{$fid});
  }
  $dtylength{$def} = $dlen;
  $dfieldlength{$def} = $dlen;
}

# ==============================
# Subroutine: padsgen($def)
#   emit Pstruct/Palternates decl for $def
#   (preceded by any aux arrays)

sub padsgen
{
  my ($def) = @_;
  if ($is_alt{$def}) {
    &padsgen_alt($def);
  } else {
    &padsgen_struct($def);
  }
}

# ==============================
# Subroutine: padsgen_struct($def)
#   emit Pstruct decl for $def
#   (preceded by any aux arrays)

sub padsgen_struct
{
  my ($def) = @_;
  my ($fids, $fid);
  &padsgen_aux_arrays($def);
  printf POUT "Pstruct %s_t \{\n", $def;
  $fids = $field_ids{$def};
  $fids =~ s/[\#]$//;
  foreach $fid (split(/[\#]/, $fids)) {
    $field = sprintf("    %-40s %25s // Field length:  %4s\n", $fieldty{$fid}, $fid . ";", $dfieldlength{$fid});
    print POUT $field;
  }
  printf POUT "};                                                                     // Total length: %5s\n\n", $dtylength{$def};
}

# ==============================
# Subroutine: padsgen_alt($def)
#   emit Palternates decl for $def
#   (preceded by any aux arrays)

sub padsgen_alt
{
  my ($def) = @_;
  my ($fids, $fid);
  &padsgen_aux_arrays($def);
  printf POUT "Palternates %s_t \{\n", $def;
  $fids = $field_ids{$def};
  $fids =~ s/[\#]$//;
  foreach $fid (split(/[\#]/, $fids)) {
    $field = sprintf("    %-40s %25s // Field length:  %4s\n", $fieldty{$fid}, $fid . ";", $dfieldlength{$fid});
    print POUT $field;
  }
  printf POUT "};                                                                     // Total length: %5s\n\n", $dtylength{$def};
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
