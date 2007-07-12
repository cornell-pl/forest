#!/usr/bin/perl

use strict;
use CGI::Pretty;
use Shell;

$CGI::DISABLE_UPLOADS = 1;
$CGI::POST_MAX        = 1000000;

my $q = new CGI;

print $q->header ( "text/html" ),
      $q->start_html ( -title   => 'Do nothing'
                     , -style   => { -src => 'css/main.css' }
                     );
print $q->h1 ( "No action here" );
print $q->end_html;

exit 0;
