#!/usr/bin/env perl

# dibbler_vetter.pl
#
#   usage:  dibbler_vetter.pl [INPUT_FILE [SEQ_MASK [CLEAN_FILE [ERROR_FILE]]]]
#
# Where the defaults are:

# my $INPUT_FILE  = "../../data/dibbler_short";
# my $SEQ_MASK    = 7;
# my $CLEAN_FILE  = "/home/kfisher/esig/dibbler_clean_PY";
# my $ERROR_FILE  =  "/home/kfisher/esig/dibbler_errors_PY";
#   (SEQ_MASK arg must be numberic; P_CheckAndSet == 7, P_Set == 1)

my $INPUT_FILE  = "data";
my $SEQ_MASK    = 7;
my $CLEAN_FILE  = "perl_vet.${SEQ_MASK}.clean";
my $ERROR_FILE  = "perl_vet.${SEQ_MASK}.error";

# dibber_vetter.pl parses input lines from INPUT_FILE. It attempts
# to make all of the checks that dibbler_vetter.c (via the generated
# read code) will make.  For lines that are correct, it outputs
# dibbler lines to CLEAN_FILE, with the following 'cleanup' actions
# applied: (1) phone numbers equal to zero are replaced with blanks.
# All error lines are output to ERROR_FILE without modification.  In
# addition, error messages describing the kind of error and the error
# location are output to stderr.

# strategy: do a split, check each field in turn for validity

# some constants
my $MAX_UINT32 =  4294967295;

my $MIN_INT64  = -9223372036854775807;
my $MAX_INT64  =  9223372036854775807;

my $MAX_UINT64 =  18446744073709551615;

if ($#ARGV >= 0) {
  $INPUT_FILE = $ARGV[0];
}
if ($#ARGV >= 1) {
  $SEQ_MASK = $ARGV[1];
}
if ($#ARGV >= 2) {
  $CLEAN_FILE = $ARGV[2];
}
if ($#ARGV >= 3) {
  $ERROR_FILE = $ARGV[3];
}

printf "Input file %s\n", $INPUT_FILE;
printf "Sequence mask %d\n", $SEQ_MASK;
printf "    (P_Set == %d)\n", 1;
printf "    (P_CheckAndSet  == %d)\n", 7;
printf "Clean file %s\n", $CLEAN_FILE;
printf "Error file %s\n", $ERROR_FILE;

open(INF, $INPUT_FILE) or die "Could not open $INPUT_FILE for reading\n";
open(OUTF, ">$CLEAN_FILE") or die "Could not open $CLEAN_FILE for writing\n";
open(ERRF, ">$ERROR_FILE") or die "Could not open $ERROR_FILE for writing\n";

my $summary_header = <INF>;
if ($summary_header =~ /^0|(?:\d+)$/) {
  print OUTF $summary_header;
} else {
  print STDERR "Invalid summary header on line 0, aborting\n";
  exit -1;
}

my $line = 0;
LINE: while (<INF>) {
  chomp;
  my @a  = split (/\|/);
  my $len = @a;
  $line++;
  my $char = 0;

  if ($len < 1) {  # can this happen?
    printf STDERR "Line $line: unexpected empty line\n";
    print ERRF $_;
    next LINE;
  }
  # validate $a[0]  (order_num)
  if ($a[0] !~ /^\d+$/) {
    printf STDERR "Line $line char $char: field order_num is not a uint32\n";
    print ERRF $_ . "\n";
    next LINE;
  } elsif ($a[0] > $MAX_UINT32) {
    printf STDERR "Line $line char $char: field order_num too large, does not fit in a uint32\n";
    print ERRF $_  . "\n";
    next LINE;
  }
  $char += (length($a[0]) + 1);
  if ($len < 2) {
    printf STDERR "Line $line char $char: found end of line, expecting vertical bar (no fields found after order_num)\n";
    print ERRF $_  . "\n";
    next LINE;
  }
  # validate $a[1]  (att_order_num)
  if ($a[1] !~ /^\d+$/) {
    printf STDERR "Line $line char $char: field att_order_num is not a uint32\n";
    print ERRF $_  . "\n";
    next LINE;
  } elsif ($a[1] > $MAX_UINT32) {
    printf STDERR "Line $line char $char: field att_order_num too large, does not fit in a uint32\n";
    print ERRF $_  . "\n";
    next LINE;
  }
  $char += (length($a[1]) + 1);
  if ($len < 3) {
    printf STDERR "Line $line char $char: found end of line, expecting vertical bar (no fields found after att_order_num)\n";
    print ERRF $_  . "\n";
    next LINE;
  }
  # validate $a[2]   (ord_version)
  if ($a[2] !~ /^\d+$/) {
    printf STDERR "Line $line char $char: field ord_version is not a uint32\n";
    print ERRF $_  . "\n";
    next LINE;
  } elsif ($a[2] > $MAX_UINT32) {
    printf STDERR "Line $line char $char: field ord_version too large, does not fit in a uint32\n";
    print ERRF $_ . "\n";
    next LINE;
  }
  $char += (length($a[2]) + 1);
  if ($len < 4) {
    printf STDERR "Line $line char $char: found end of line, expecting vertical bar (no fields found after ord_version)\n";
    print ERRF $_  . "\n";
    next LINE;
  }
  # validate $a[3]   (service_tn)
  if ($a[3] !~ /^\d*$/) {
    printf STDERR "Line $line char $char: field service_tn is not blank or a uint64\n";
    print ERRF $_  . "\n";
    next LINE;
  } elsif ($a[3] > $MAX_UINT64) {
    printf STDERR "Line $line char $char: field service_tn too large, does not fit in a uint64\n";
    print ERRF $_  . "\n";
    next LINE;
  }
  $char += (length($a[3]) + 1);
  if ($len < 5) {
    printf STDERR "Line $line char $char: found end of line, expecting vertical bar (no fields found after service_tn)\n";
    print ERRF $_  . "\n";
    next LINE;
  }
  # validate $a[4]   (billing_tn)
  if ($a[4] !~ /^\d*$/) {
    printf STDERR "Line $line char $char: field billing_tn is not blank or a uint64\n";
    print ERRF $_  . "\n";
    next LINE;
  } elsif ($a[4] > $MAX_UINT64) {
    printf STDERR "Line $line char $char: field billing_tn too large, does not fit in a uint64\n";
    print ERRF $_  . "\n";
    next LINE;
  }
  $char += (length($a[4]) + 1);
  if ($len < 6) {
    printf STDERR "Line $line char $char: found end of line, expecting vertical bar (no fields found after billing_tn)\n";
    print ERRF $_  . "\n";
    next LINE;
  }
  # validate $a[5]   (nlp_service_tn)
  if ($a[5] !~ /^\d*$/) {
    printf STDERR "Line $line char $char: field nlp_service_tn is not blank or a uint64\n";
    print ERRF $_  . "\n";
    next LINE;
  } elsif ($a[5] > $MAX_UINT64) {
    printf STDERR "Line $line char $char: field nlp_service_tn too large, does not fit in a uint64\n";
    print ERRF $_ . "\n";
    next LINE;
  }
  $char += (length($a[5]) + 1);
  if ($len < 7) {
    printf STDERR "Line $line char $char: found end of line, expecting vertical bar (no fields found after nlp_service_tn)\n";
    print ERRF $_  . "\n";
    next LINE;
  }
  # validate $a[6]    (nlp_billing_tn)
  if ($a[6] !~ /^\d*$/) {
    printf STDERR "Line $line char $char: field nlp_billing_tn is not blank or a uint64\n";
    print ERRF $_  . "\n";
    next LINE;
  } elsif ($a[6] > $MAX_UINT64) {
    printf STDERR "Line $line char $char: field nlp_billing_tn too large, does not fit in a uint64\n";
    print ERRF $_  . "\n";
    next LINE;
  }
  $char += (length($a[6]) + 1);
  if ($len < 8) {
    printf STDERR "Line $line char $char: found end of line, expecting vertical bar (no fields found after nlp_billing_tn)\n";
    print ERRF $_  . "\n";
    next LINE;
  }
  # validate $a[7]   (zip_code)
  if ($a[7] =~ /^$/) {
    # blank is OK
  } elsif ($a[7] =~ /^(\d+)$/) {
    # small or big zip code, must fit in uint64
    if ($a[7] > $MAX_UINT64) {
      printf STDERR "Line $line char $char: field zip_code is too large, does not fit in a uint64\n";
      print ERRF $_ . "\n";
      next LINE;
    }
  } elsif ($a[7] =~ /^(\d+)[\-\/ ](\d+)$/) {
    my ($zip, $suffix) = ($1, $2);
    # zip must fit in uint32, suffix must fit in uint32
    if ($zip > $MAX_UINT32) {
      printf STDERR "Line $line char $char: field zip_code zip+suffix form, zip does not fit in a uint32\n";
      print ERRF $_ . "\n";
      next LINE;
    }
    if ($suffix > $MAX_UINT32) {
      printf STDERR "Line $line char $char: field zip_code zip+suffix form, suffix does not fit in a uint32\n";
      print ERRF $_ . "\n";
      next LINE;
    }
  } else {
    printf STDERR "Line $line char $char: field zip_code is not empty, or an unsigned number, or a zip+suffix form\n";
    print ERRF $_ . "\n";
    next LINE;
  }
  $char += (length($a[7]) + 1);
  if ($len < 9) {
    printf STDERR "Line $line char $char: found end of line, expecting vertical bar (no fields found after zip_code)\n";
    print ERRF $_ . "\n";
    next LINE;
  }
  # validate $a[8]    (ramp)
  if ($a[8] =~ /^[\-]?(\d+)$/) {
    # normal ramp, must fit in an int64 
    if ($a[8] < $MIN_INT64 || $a[8] > $MAX_INT64) {
      printf STDERR "Line $line char $char: field ramp normal case does not fit in an int64\n";
      print ERRF $_ . "\n";
      next LINE;
    }
  } elsif ($a[8] =~ /^no_ii(\d+)$/) {
    # special generated ramp, digits must fit in a uint64
    my $ramp = $1;
    if ($ramp > $MAX_UINT64) {
      printf STDERR "Line $line char $char: field ramp no_ii case does not fit in a uint64\n";
      print ERRF $_ . "\n";
      next LINE;
    }
  } else {
    printf STDERR "Line $line char $char: field ramp invalid, not an int64 without the leading 'no_ii', nor a uint64 with the leading 'no_ii')\n";
    print ERRF $_ . "\n";
    next LINE;
  }
  $char += (length($a[8]) + 1);
  if ($len < 10) {
    printf STDERR "Line $line char $char: found end of line, expecting vertical bar (no fields found after ramp)\n";
    print ERRF $_ . "\n";
    next LINE;
  }
  # validate $a[9]    (order_type)
  # (nothing to validate, any string will do)
  $char += (length($a[9]) + 1);
  if ($len < 11) {
    printf STDERR "Line $line char $char: found end of line, expecting vertical bar (no fields found after order_type)\n";
    print ERRF $_ . "\n";
    next LINE;
  }
  # validate $a[10]   (order_details)
  if ($a[10] !~ /^\d+$/) {
    printf STDERR "Line $line char $char: field order_details is not a uint32\n";
    print ERRF $_ . "\n";
    next LINE;
  } elsif ($a[10] > $MAX_UINT32) {
    printf STDERR "Line $line char $char: field order_details too large, does not fit in a uint32\n";
    print ERRF $_ . "\n";
    next LINE;
  }
  $char += (length($a[10]) + 1);
  if ($len < 12) {
    printf STDERR "Line $line char $char: found end of line, expecting vertical bar (no fields found after order_details)\n";
    print ERRF $_ . "\n";
    next LINE;
  }
  # validate $a[11]   (ununsed)
  # (nothing to validate, any string will do)
  $char += (length($a[11]) + 1);
  if ($len < 13) {
    printf STDERR "Line $line char $char: found end of line, expecting vertical bar (no fields found after unused)\n";
    print ERRF $_ . "\n";
    next LINE;
  }
  # validate $a[12]   (stream)
  # (nothing to validate, any string will do)
  $char += (length($a[12]) + 1);
  if ($len < 14) {
    printf STDERR "Line $line char $char: found end of line, expecting vertical bar (no fields found after stream)\n";
    print ERRF $_ . "\n";
    next LINE;
  }
  my $idx = 13;
  while ($idx < $len) {
    my $state = $a[$idx];
    # validate $state
    # (nothing to validate, any string will do)
    $char += (length($state) + 1);
    $idx++;
    if ($idx >= $len) {
      printf STDERR "Line $line char $char: found end of line, expecting vertical bar (no timestamp field after state in event)\n";
      print ERRF $_ . "\n";
      next LINE;
    }
    my $tstamp = $a[$idx];
    # validate $tstamp
    if ($tstamp !~ /^\d+$/) {
      printf STDERR "Line $line char $char: field tstamp in state-tstamp pair is not a uint32\n";
      print ERRF $_ . "\n";
      next LINE;
    } elsif ($tstamp > $MAX_UINT32) {
      printf STDERR "Line $line char $char: field tstamp in state-tstamp pair too large, does not fit in a uint32\n";
      print ERRF $_ . "\n";
      next LINE;
    }
    $char += (length($tstamp) + 1);
    $idx++;
  }
  if ($SEQ_MASK > 1) { # not exactly the right check
    for ($i = 14; $i < $idx-2; $i += 2) {
      if ($a[$i] > $a[$i+2]) {
	printf STDERR "Line $line char $char: Parray Pwhere clause violated (non-decreasing check failed)\n";
	print ERRF $_ . "\n";
	next LINE;
      }
    }
  }
  # OK, the whole line is OK, do telephone number fixups
  if ($a[3] eq "0") {
    $a[3] = "";
  }
  if ($a[4] eq "0") {
    $a[4] = "";
  }
  if ($a[5] eq "0") {
    $a[5] = "";
  }
  if ($a[6] eq "0") {
    $a[6] = "";
  }
  # and print to OUTF
  print OUTF join("|", @a) . "\n";
}

close(INF);
close(OUTF);
close(ERRF);
exit 0;

