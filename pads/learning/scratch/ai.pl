#!/usr/bin/perl

open (YUMTXT,"<data/ai.3000") || die "Can't open ai.3000";

sub smlhdr
{ local ( $file, $name ) = @_;
  print $file "structure Yum$name = struct\n";
  print $file "    open Tokens\n";
  print $file "\n";
  print $file "    fun mkPhonyLoc ( ln : int ) : location = mkLoc 0 0 1 ln\n";
}

sub smlend
{ local ( $file ) = @_;
  print $file "end\n";
}

sub separator
{  local ( $indent, $recno ) = @_;
   local ( $spaces ) = " " x $indent;
   return "$spaces  " if $recno <= 1;
   return "$spaces, " if $recno > 1;
}

sub printField
{  local ( $file, $name, @a ) = @_;
   local ( $cnt ) = 0;
   print $file "    val $name : LToken list =\n";
   print $file "    [\n";
   for ( my $j = 1; $j < $#a+1; $j++ )
   { local ( $sep ) = separator (4, $j);
     local ( $cur ) = $a[$j];
     if ( $cur ne "EMPTY" ) {
        print $file "$sep$cur";
        $cnt += 1;
     }
   }
   print $file "    ]\n";
   print $file "    val ${name}Freq : int = $cnt\n";
   print $file "\n";
}

$recno = 1;
while ($record = <YUMTXT>) {
  chop ($record);
  ( $iporhost, @rest ) = split (/ /, $record);
  push (@host, "( Phostname \"$iporhost\", mkPhonyLoc $recno )\n") if $iporhost =~ /[A-Za-z]/;
  push (@ip, "( Pip \"$iporhost\", mkPhonyLoc $recno )\n") if ! ( $iporhost =~ /[A-Za-z]/ );
  $recno += 1;
}

# @insupdpkg   = grep(!/EMPTY/, @insupdpkg);
# @insupdarch  = grep(!/EMPTY/, @insupdarch);

print "# of ip = $#ip\n";
print "# of host = $#host\n";

open (AIIPORHOSTSML,">aiIpOrHost.sml") || die "Can't open aiIpOrHost.sml";

&smlhdr ( AIIPORHOSTSML, "Tokens" );

&printField ( AIIPORHOSTSML, "ip", @ip );
&printField ( AIIPORHOSTSML, "host", @host );

&smlend ( AIIPORHOSTSML );

close ( AIIPORHOSTSML );
