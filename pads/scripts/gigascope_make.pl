#!/usr/bin/env perl

use Getopt::Long;

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
# main program

my $dbg=1;

print "\n";
my $pads_home = $ENV{"PADS_HOME"};
if ($pads_home eq "") {
  print "    env variable PADS_HOME must be set\n";
  print "    set PADS_HOME and try again\n\n";
  exit -1;
}

my %opt = ();
GetOptions( "usage"      => \$usage,
            "demodir:s"  => \$demodir,
            "pspec:s"    => \$pspec,
	    "template:s" => \$template,
	    "rectype:s"  => \$rectype,
	    "hdrtype:s"  => \$hdrtype,
	    "nrec:s"     => \$nrec,
	    "maxrecs:i"  => \$maxrecs,
	    "iodisc:s"   => \$iodisc );

if ($ARGV[0]) {
  print "The following command line arguments were not valid: " . join(' ', @ARGV) . "\n";
  goto usage;
}
goto usage if $usage;

my @missing = ();
push(@missing, "pspec")     if (!defined($pspec));
push(@missing, "template")  if (!defined($template));
push(@missing, "rectype")   if (!defined($rectype));
push(@missing, "hdrtype")   if (!defined($hdrtype) && ($template eq "hdr_recs_repeat" || $template eq "hdr_recs"));
push(@missing, "nrec")      if (!defined($nrec)    && ($template eq "hdr_recs_repeat"));

if (@missing) {
  print "  The following required options are missing: " . join(", ", @missing) . "\n";
  goto usage;
}

goto usage if ($template !~ /(recs|hdr_rec|hdr_recs_repeat)/);

if ($dbg) {
  print "  demodir  = $demodir\n"  if $demodir;
  print "  pspec    = $pspec\n"    if $pspec;
  print "  template = $template\n" if $template;
  print "  rectype  = $rectype\n"  if $rectype;
  print "  hdrtype  = $hdrtype\n"  if $hdrtype;
  print "  maxrecs  = $maxrecs\n"  if $maxrecs;
  print "  iodisc   = $iodisc\n"   if $iodisc;
  print "  nrec     = $nrec\n"     if $nrec;
  print "\n";
}

if (!defined($demodir)) {
  $demodir = ".";
}
if ($demodir !~ m|/$|) {
  $demodir .= "/";
}

if (!(-e $pspec)) {
  print "  Error: pspec $pspec not found\n\n" ;
  exit -1;
}

my $ptext = &readFile($pspec);
if ($ptext eq "") {
  print "  Could not open pspec file $pspec for reading\n\n";
  exit -1;
}

if ($ptext !~ /Pstruct\s+$rectype/) {
  print "  Could not find Pstruct $rectype in pspec file $pspec\n\n";
  exit -1;
}

if ($hdrtype && $ptext !~ /Pstruct\s+$hdrtype/) {
  print "  Could not find Pstruct $rectype in pspec file $pspec\n\n";
  exit -1;
}

my $prefix = $pspec;
$prefix =~ s|^(.*)/||g;
$prefix =~ s/.p$//;
my $trfile = "$demodir$prefix" . ".transform";
my $scfile = "$demodir$prefix" . ".schema";
my $mkfile = "$demodir$prefix" . ".make";

my $tfile = "$pads_home/scripts/templates/tr_$template";
my $ttext = &readFile($tfile);
if ($ttext eq "") {
  print "  Could not find template file $tfile\n\n";
  exit -1;
}

$ttext =~ s/PSPEC_FILE/$pspec/g;
$ttext =~ s/SCHEMA_FILE/$scfile/g;
$ttext =~ s/REC_TYPE/$rectype/g;
$ttext =~ s/HDR_TYPE/$hdrtype/g if $hdrtype;

print "\nCreating $trfile\n" if ($dbg);

open (TRF, ">$trfile") or die "\nCould not open temporary file $trfile for writing\n\n";
print TRF $ttext;
close (TRF);
print "\nDone creating $trfile\n" if ($dbg);

print "\nInvoking padsc on $trfile\n" if ($dbg);
my $res = `$pads_home/scripts/padsc -T $trfile $pspec`;
print "\nDone invoking padsc on $trfile\n\nres=[$res]\n\n" if ($dbg);

exit 0;

usage:
print "\n  Usage:  gigascope_make.pl [options]

    where options are:
     -u/--usage             : output this usage message
     -d/--demodir           : directory to write output files (default is current working dir)
     -p/--pspec <file>      : specify PADS spec file (required)
     -t/--template <name>   : specify template name (required), see template descriptions below
     -r/--rectype <type>    : specify main record type (required)
     -h/--hdrtype <type>    : specify header type (may be required, depends on template)
     -n/--nrec <expr>       : expression giving # of records following a header
                              (may be required, depends on template). Use variable global_hdr
                              in the expression, as in: --nrec=global_hdr.count
     -m/--maxrecs <#>       : specify max # of record to process (optional)
     -i/--iodisc <iodisc>   : specify IO discipline (optional)
                              default is norec, other choices: nlrec, ...

    Template Descriptions
    =====================

         recs
         ----
         Input just contains a sequence of one or more records of type rectype,
         terminated by end of input.

         hdr_recs
         --------
         Input contains a single header of type hdrtype, followed by a sequence of one
         or more records of type rectype.  If an nrec expression is given, it
         should give the number of records which follow the header.  Use
         variable global_hdr (of type hdrtype), as in: --nrec=global_hdr.count

         If no nrec expression is given, the series of records following the
         header is terminated by end of input.

        hdr_recs_repeat
        ---------------
        Input contains one or more groupings, where each grouping consists of
        a header followed by a sequence of records.  In other words, the input
        pattern is  

           <header><rec><rec> ... <header><rec><rec> ... <header><rec><rec> ... 

        For this template, the -n/nrec option is required, so that the generated
        code will know when it is time to read the next header.  Use
        variable global_hdr (of type hdrtype), as in: --nrec=global_hdr.count

    Output Files
    ============

        If pspec is foo.p, then the output files are:

           foo.schema    : schema file
           foo_funs.h    : C declarations of the get functions specified in foo.schema
           foo_funs.c    : C implementations of the get functions specified in foo.schema

    Limitations, Known Problems
    ===========================

        1. Currently, rectype and hdrtype must both be Pstruct types.

        2. The only fields that are reflected in the schema are
           simple base type fields.  In the future, we should expand
           nested types and reflect all 'leaf' base type fields
           as top-level fields in the schema.

";
exit -1;
