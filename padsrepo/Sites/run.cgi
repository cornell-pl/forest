#!/usr/bin/perl

use strict;
use CGI::Pretty;
use Shell;

$CGI::DISABLE_UPLOADS = 1;
$CGI::POST_MAX        = 1000000;

my $q          = new CGI;
my $src        = $q->param ( "datasource" );
my $processing = $q->param ( "Processing" );

print $q->header ( "text/html" ),
      $q->start_html ( -title   => 'Generate outputs'
                     , -style   => { -src => 'css/main.css' }
                     );

# print $q->img ( { -src => "images/logo_galois.gif", -align => 'LEFT' } );

# print $q->br ();
# print $q->br ();
# print $q->br ();

my $home     = "/Users/peterwhite";
my $site     = "$home/Sites";
my $lrn      = "$home/pads/learning/scratch";
my $gen      = "$lrn/gen";
my $dataRoot = "$lrn/data";
my $datafile = "$dataRoot/$src";
my $learn1   = `$site/rl $src`       if ( $processing =~ /Internal/ );
$learn1      = `$site/rl $src accum` if ( $processing =~ /Accumulator/ );
$learn1      = `$site/rl $src xml`   if ( $processing =~ /XML/ );
$learn1      = `$site/rl $src fmt`   if ( $processing =~ /Reformat/ );
$learn1      = `$site/rl $src all`   if ( $processing =~ /All/ );

# Slurp up the data file
open(INF,$datafile) or die "Can't open $datafile for reading: $!\n";
my @data = <INF>;  #Read the file into an array
close(INF);
chomp(@data);
my $datas = join ( "\n", @data );

# print $q->p ( "Result of learn1 is: $learn1\n ");

# Now slurp up the Ty file
open (TY, "<$lrn/gen/Ty") || die "Can't open gen/Ty for reading: $!\n";
my @ty = <TY>;
close (TY);
my $tyFile = join ( "", @ty);

print $q->start_form ( -method => "get"
                     , -action => "nothing.cgi"
                     );

print $q->h2 ( "Input data ($src, $processing)" );
print $q->textarea ( -name    => $src
                   , -rows    => "30"
                   , -cols    => "100"
                   , -wrap    => "off"
                   , -default => $datas
                   );

print $q->h2 ( "Internal type ($src)" );
print $q->textarea ( -name    => "Ty"
                   , -rows    => "30"
                   , -cols    => "100"
                   , -wrap    => "off"
                   , -default => $tyFile
                   );

if ( $processing =~ /Accumulator|All/ )
{ # Now slurp up the Accumulator output
  print $q->h2 ( "Accumulator report ($src)" );

  open (ACCUM, "<$gen/$src-accum.out") || die "Can't open gen/$src-accum.out for reading\n";
  my @accum = <ACCUM>;
  close (ACCUM);
  my $accumFile = join ( "", @accum);

  print $q->textarea ( -name    => "accum"
                     , -rows    => "30"
                     , -cols    => "100"
                     , -wrap    => "off"
                     , -default => $accumFile
                     );
}

if ( $processing =~ /XML|All/ )
{ # Now slurp up the XML output
  print $q->h2 ( "XML ($src)" );

  open (XML, "<$gen/$src-xml.out") || die "Can't open $gen/$src-xml.out for reading\n";
  my @xml = <XML>;
  close (XML);
  my $xmlFile = join ( "", @xml);

  print $q->textarea ( -name    => "XML"
                     , -rows    => "30"
                     , -cols    => "100"
                     , -wrap    => "off"
                     , -default => $xmlFile
                     );
}

if ( $processing =~ /Reformat|All/ )
{ # Now slurp up the Reformatting output
  print $q->h2 ( "Reformat output" );
    open (FMT, "<$gen/$src-fmt.out") || die "Can't open $gen/$src-fmt.out for reading\n";
  my @fmt = <FMT>;
  close (FMT);
  my $fmtFile = join ( "", @fmt);

  print $q->textarea ( -name    => "FMT"
                     , -rows    => "30"
                     , -cols    => "100"
                     , -wrap    => "off"
                     , -default => $fmtFile
                     );

}

print $q->end_form;

print $q->end_html;

exit 0;
