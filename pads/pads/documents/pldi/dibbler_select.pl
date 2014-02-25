#!/usr/bin/env perl

# dibbler_select.pl
#
#   usage:  dibbler_select.pl [SELECT_STATE [CLEAN_INPUT_FILE [OUTPUT_FILE]]]
#
# Where the defaults are:

my $SELECT_STATE = "21";
my $CLEAN_INPUT_FILE = "perl_vet.7.clean";
my $OUTPUT_FILE  = "perl_select.${SELECT_STATE}";

# strategy: use Perl's regexp pattern matcher
#           including its ability to precompile a pattern
#           and apply it to each line

if ($#ARGV >= 0) {
  $SELECT_STATE = $ARGV[0];
  $OUTPUT_FILE  = "perl_select.${SELECT_STATE}";
}
if ($#ARGV >= 1) {
  $CLEAN_INPUT_FILE = $ARGV[1];
}
if ($#ARGV >= 2) {
  $OUTPUT_FILE = $ARGV[2];
}

printf "Select state %s\n", $SELECT_STATE;
printf "Clean input file %s\n", $CLEAN_INPUT_FILE;
printf "Output file %s\n", $OUTPUT_FILE;

open(INF, $CLEAN_INPUT_FILE) or die "Could not open $CLEAN_INPUT_FILE for reading\n";
open(OUTF, ">$OUTPUT_FILE") or die "Could not open $OUTPUT_FILE for writing\n";

# $1:   order_num
#  A:   att_order_num
#  B:   ord_version
#  C:   service_tn
#  D:   billing_tn
#  E:   nlp_service_tn
#  F:   nlp_billing_tn
#  G:   zip_code
#  H:   dib_ramp
#  I:   order_type
#  J:   order_details
#  K:   unused
#  L:   stream
#  pairs:   zero or more pairs
#  targ:    target state

#                    1     A      B      C      D      E      F      G      H      I      J      K      L      pairs              targ
my $compiled = qr/^(\d+)\|[^|]*\|[^|]*\|[^|]*\|[^|]*\|[^|]*\|[^|]*\|[^|]*\|[^|]*\|[^|]*\|[^|]*\|[^|]*\|[^|]*(?:\|[^|]*\|[^|]*)*\|$SELECT_STATE\|/;

my $pattern = "^(\d+)\|[^|]*\|[^|]*\|[^|]*\|[^|]*\|[^|]*\|[^|]*\|[^|]*\|[^|]*\|[^|]*\|[^|]*\|[^|]*\|[^|]*(?:\|[^|]*\|[^|]*)*\|$SELECT_STATE\|";
print "pattern = [$pattern]\n";

my $line1 = <INF>;
print OUTF $line1; # just print first line to out file, assume summary header is correct

LINE: while (<INF>) {
  if (m/$compiled/) {
    print OUTF "$1\n";
  }
}

close(INF);
close(OUTF);
exit 0;
