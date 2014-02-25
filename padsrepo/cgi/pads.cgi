#!/usr/bin/perl

use strict;
use CGI::Pretty;
use CGI::Carp qw (fatalsToBrowser);
BEGIN {
    sub carp_error {
        my $error_message = shift;
        my $q = new CGI;
        my $discard_this = $q->header( "text/html" );
        error( $q, $error_message );
    }
    CGI::Carp::set_message( \&carp_error );
}

sub error {
    my( $q, $error_message ) = @_;
    
    print $q->header( "text/html" ),
          $q->start_html( "Error" ),
          $q->h1( "Error" ),
          $q->p( "Sorry, the following error has occurred: " ),
          $q->p( $q->i( $error_message ) ),
          $q->end_html;
    exit;
}

$CGI::DISABLE_UPLOADS = 0;
$CGI::POST_MAX        = 1000000;

my $q = new CGI;

print $q->header ( "text/html" ),
      $q->start_html ( -title   => 'Pads demo, Galois, Inc'
                     , -style   => { -src => 'css/main.css' }
                     );

print $q->img ( { -src => "images/logo_galois.gif", -align => 'LEFT' } );

print $q->br ();
print $q->br ();
print $q->br ();

print $q->start_form ( -method => "get"
                     , -action => "data.cgi"
                     );
print $q->h3 ( "Data sources");
print $q->scrolling_list ( -name => "Data source"
                         , -values    => [ "Roll your own"
                                         , "MER_T01_01.csv"
                                         , "ai.3000"
                                         , "1967Transactions.short"
                                         , "asl.log"
                                         , "boot.log"
                                         , "crashreporter.log"
                                         , "crashreporter.log.modified"
                                         , "dibbler.1000"
                                         , "ls-l.txt"
                                         , "netstat-an"
                                         , "page_log"
                                         , "quarterlypersonalincome"
                                         , "railroad.txt"
                                         , "rpmpkgs"
                                         , "scrollkeeper.log"
                                         , "windowserver_last.log"
                                         , "yum.txt"
                                         ]
                         , -default   => "ai.3000"
                         , -size      => 18
                         , -multiple  => 'false'
                         , -linebreak => 'true'
                         );
print $q->submit ( -name    => "Submit"
                 , -default => "None"
                 );
print $q->end_form;

# print $q->table ( { -border => 1
#                   , -width  => "320"
#                   }
#                 , $q->Tr ( [ $q->th ( { -bgcolor => "#00bfff" }
#                                       , [ "Name", "Value" ]
#                                     )
#                            , $q->td ( [ "Server Name", $ENV{SERVER_NAME} ] )
#                            , $q->td ( [ "Listening on Port", $ENV{SERVER_PORT} ] )
#                            , $q->td ( [ "Server Software", $ENV{SERVER_SOFTWARE} ] )
#                            , $q->td ( [ "Server Protocol", $ENV{SERVER_PROTOCOL} ] )
#                            , $q->td ( [ "CGI Version", $ENV{GATEWAY_INTERFACE} ] )
#                            , $q->td ( [ "Script name", $ENV{SCRIPT_NAME} ] )
#                            ]
#                          )
#                 );

print $q->end_html;



