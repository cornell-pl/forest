#!/usr/bin/perl -w

use CGI::Pretty;
use Shell;

$CGI::DISABLE_UPLOADS = 1;
$CGI::POST_MAX        = 1000000;

my $q = new CGI;

my $datasource = $q->param ( "Data source" );

# Form to select processing of the data
sub selectProcessing
{ local ( $runscript, $src ) = @_;
  print $q->start_form ( -method => "get"
                       , -action => $runscript
                       );
  print $q->scrolling_list ( -name     => "Processing"
                           , -values   => [ "Internal parser"
                                          , "Accumulator report"
                                          , "XML output"
                                          , "Reformat"
                                          , "All"
                                          ]
                           , -default  => "Internal parser"
                           , -size     => 5
                           , -align    => 'LEFT'
                           , -multiple => 'false'
                           );
  print $q->submit ( -name    => "Submit"
                   , -default => "None"
                   );
  # Hidden form element to send the name of the datasource to the next script
  print $q->hidden ( -name    => "datasource"
                   , -default => $src
                   );
  print $q->end_form; # End of form to select processing of the data
}

# Prepare a sample of roll your own data
my @sample = ( "nov 1, 1975|3|Carlson J. Kessel"
             , "january 5 1997|5|Howard K. Bilge"
             , "091199|none|Bob Jones"
             , "1/12/06|8|Jerry Johnson"
             , "09111980|4.3|Whyia C. Nott"
             );

my $sampleData = join ( "\n", @sample );
$sampleData .= "\n";

# Start of the HTML is common to both cases
print $q->header ( "text/html" ),
      $q->start_html ( -title   => 'Gather data'
                     , -style   => { -src => 'css/main.css' }
                     );

print $q->img ( { -src => "images/logo_galois.gif", -align => 'LEFT' } );

print $q->br ();
print $q->br ();
print $q->br ();

if ( $datasource ne "Roll your own" )
{ # Slurp the data file
  print $q->h2 ( "$datasource" );

  my $home     = "/Users/peterwhite";
  my $site     = "$home/Sites";
  my $lrn      = "$home/pads/learning/scratch";
  my $dataRoot = "$lrn/data";
  my $datafile = "$dataRoot/$datasource";
  open(INF,$datafile) or die "Can't open $datafile for reading: $!\n";
  my @lines = <INF>;  #Read the file into an array
  close(INF);
  chomp(@lines);
  my $multiLine = join ( "\n", @lines );

# Form to present the input data selected by the users
  print $q->start_form ( -method => "get"
                       , -action => "nothing.cgi"
                       );
  print $q->textarea ( -name    => $datasource
                     , -rows    => "30"
                     , -cols    => "100"
                     , -wrap    => "off"
                     , -default => $multiLine
                     );
  print $q->end_form; # End of form to present the input data selected by the users
  selectProcessing ( "run.cgi", $datasource ); # Form to select processing of the data
} else {
# Form to get the input data selected by the users
  print $q->start_form ( -method => "get"
                       , -action => "rollYourOwn.cgi"
                       );

  print $q->textarea ( -name    => "Roll your own"
                     , -rows    => "30"
                     , -cols    => "100"
                     , -wrap    => "off"
                     , -default => "$sampleData"
                     );
  print $q->br ();
  print $q->br ();

  print $q->scrolling_list ( -name     => "Processing"
                           , -values   => [ "Internal parser"
                                          , "Accumulator report"
                                          , "XML output"
                                          , "Reformat"
                                          , "All"
                                          ]
                           , -default  => "Internal parser"
                           , -size     => 5
                           , -align    => 'LEFT'
                           , -multiple => 'false'
                           );
  print $q->submit ( -name    => "Submit"
                   , -default => "None"
                   );

  print $q->end_form; # End of form to select processing of the data
}

print $q->end_html; # Common end to HTML for both cases
exit 0;


# my $sh = Shell->new;
# my $x1 = $sh->echo ("howdy");
# my $x2 = $sh->ls ();
# my $x3 = open ( "/Users/peterwhite/Sites/junk|");
# my $x4 = `$site/junk`;
# my $x5 = `$home/bin/junk`;
# my $x6 = $sh->pwd ();
# my $learn1 = `$site/rl`;
# my $learn2 = `cd $lrn; ./learn`;

# print $q->p ( "Result of echo is: $x1\n ");
# print $q->p ( "Result of ls is: $x2\n ");
# print $q->p ( "Result1 of junk is: $x3\n ");
# print $q->p ( "Result2 of junk is: $x4\n ");
# print $q->p ( "Result3 of junk is: $x5\n ");
# print $q->p ( "Result of pwd is: $x6\n ");
# print $q->p ( "Result of learn1 is: $learn1\n ");
# print $q->p ( "Result of learn2 is: $learn2\n ");
# print $q->p ( "Data source is $datafile\n");
