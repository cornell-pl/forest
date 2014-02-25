#!/usr/bin/perl

use strict;
use CGI::Pretty;
use Shell;

$CGI::DISABLE_UPLOADS = 1;
$CGI::POST_MAX        = 1000000;

my $q = new CGI;

my $usertext   = $q->param ( "Roll your own" );
my $processing = $q->param ( "Processing" );
my @userstuff  = split( /\n/, $usertext);
my $nlines     = $#userstuff + 1;

# Generate a data file from the user input
my $home     = "/Users/peterwhite";
my $site     = "$home/Sites";
my $lrn      = "$home/pads/learning/scratch";
my $gen      = "$lrn/gen";
my $dataRoot = "$lrn/data";
my $datafile = "$dataRoot/RollYourOwn";
open (RYO, ">$datafile") || die "Can't open $datafile for writing: $!\n";
print RYO $usertext;
close (RYO);

# Slurp up the data file
open(INF,$datafile) or die "Can't open $datafile for reading: $!\n";
my @data = <INF>;  #Read the file into an array
close(INF);
chomp(@data);
my $datas = join ( "\n", @data );

print $q->header ( "text/html" ),
      $q->start_html ( -title   => 'Generate outputs'
                     , -style   => { -src => 'css/main.css' }
                     );

print $q->img ( { -src => "images/logo_galois.gif", -align => 'LEFT' } );

print $q->br ();
print $q->br ();
print $q->br ();

my $home     = "/Users/peterwhite";
my $site     = "$home/Sites";
my $lrn      = "$home/pads/learning/scratch";
my $learn1   = "gronk";
$learn1      = `$site/rl RollYourOwn internal` if ( $processing =~ /Internal/ );
$learn1      = `$site/rl RollYourOwn accum`    if ( $processing =~ /Accumulator/ );
$learn1      = `$site/rl RollYourOwn xml`      if ( $processing =~ /XML/ );
$learn1      = `$site/rl RollYourOwn fmt`      if ( $processing =~ /Reformat/ );
$learn1      = `$site/rl RollYourOwn all`      if ( $processing =~ /All/ );

# Now slurp up the Ty file
open (TY, "<$gen/Ty") || die "Can't open $gen/Ty for reading\n";
my @ty = <TY>;
close (TY);
my $tyFile = join ( "", @ty);

print $q->start_form ( -method => "get"
                     , -action => "nothing.cgi"
                     );

print $q->h2 ( "Input data (Roll your own, $processing)" );
print $q->textarea ( -name    => "Ty"
                   , -rows    => "30"
                   , -cols    => "100"
                   , -wrap    => "off"
                   , -default => $tyFile
                   );

print $q->h2 ( "Internal type (Roll your own)" );
print $q->textarea ( -name    => "Ty"
                   , -rows    => "30"
                   , -cols    => "100"
                   , -wrap    => "off"
                   , -default => $tyFile
                   );

if ( $processing =~ /Accumulator|All/ )
{ # Now slurp up the Accumulator output
  print $q->h2 ( "Accumulator report (Roll your own)" );

  open (ACCUM, "<$gen/RollYourOwn-accum.out") || die "Can't open $gen/RollYourOwn-accum.out for reading\n";
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
  print $q->h2 ( "XML (Roll your own)" );

  open (XML, "<$gen/RollYourOwn-xml.out") || die "Can't open $gen/RollYourOwn-xml.out for reading\n";
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
    open (FMT, "<$gen/RollYourOwn-fmt.out") || die "Can't open $gen/RollYourOwn-fmt.out for reading\n";
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
