#!/usr/bin/perl

open (YUMTXT,"<data/yum.txt") || die "Can't open yum.txt";

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
  ( $date1, $date2, $time, $sw1, $package, $release) = split (/ /, $record);
  $dates[$recno] = "( Pdate \"$date1 $date2\", mkPhonyLoc $recno )\n";
  $times[$recno] = "( Ptime \"time\", mkPhonyLoc $recno )\n";
  $sw1 =~ s/://;
  $sw  = "( Pstring \"$sw1\", mkPhonyLoc $recno )\n";
  $switches[$recno] = $sw;
  if (($sw1 =~ /Updated/) || ($sw1 =~ /Installed/)) {
    ( $pkgname, $arch )  = split (/\./, $package);
    ( $major, $minor )   = split (/\-/, $release);
    $insupdpkg[$recno]   = "( Pstring \"$pkgname\", mkPhonyLoc $recno )\n";
    $insupdarch[$recno]  = "( Pstring \"$arch\", mkPhonyLoc $recno)\n";
    $insupdmajor[$recno] = "( Pstring \"$major\", mkPhonyLoc $recno)\n";;
    $insupdminor[$recno] = "( Pstring \"$minor\", mkPhonyLoc $recno)\n";;
    $erasedpkg[$recno]   = "EMPTY";
  }
  else {
    $insupdpkg[$recno]   = "EMPTY";
    $insupdarch[$recno]  = "EMPTY";
    $insupdmajor[$recno] = "EMPTY";
    $insupdminor[$recno] = "EMPTY";
    $erasedpkg[$recno]   = "( Pstring \"$package\", mkPhonyLoc $recno)\n";
  }
  $recno += 1;
}

print "\n";
@insupdpkg   = grep(!/EMPTY/, @insupdpkg);
@insupdarch  = grep(!/EMPTY/, @insupdarch);
@insupdmajor = grep(!/EMPTY/, @insupdmajor);
@insupdminor = grep(!/EMPTY/, @insupdminor);
@erasedpkg   = grep(!/EMPTY/, @erasedpkg);
@insupd = grep(!/Erased/,@switches);
@erased = grep(/Erased/,@switches);
print "# of insupd = $#insupd\n";
print "# of insupdpkg = $#insupdpkg, $#insupdarch, $#insupdmajor, $#insupdminor\n";
print "# of erased = $#erased, $#erasedpkg\n";

open (YUMSML,">yumTokens.sml") || die "Can't open yumTokens.sml";
open (YUMDATES,">yumDates.sml") || die "Can't open yumDates.sml";
open (YUMTIMES,">yumTimes.sml") || die "Can't open yumTimes.sml";
open (YUMINSUPD, ">yumInsUpd.sml") || die "Can't open yumInsUpd.sml";
open (YUMPKGNAME, ">yumPkgName.sml") || die "Can't open yumPkgName.sml";
open (YUMERASED, ">yumErased.sml") || die "Can't open yumErased.sml";
open (YUMARCH, ">yumArch.sml") || die "Can't open yumArch.sml";
open (YUMMAJOR, ">yumMajor.sml") || die "Can't open yumMajor.sml";
open (YUMMINOR, ">yumMinor.sml") || die "Can't open yumMinor.sml";

&smlhdr ( YUMSML, "Tokens" );
&smlhdr ( YUMDATES, "Dates" );
&smlhdr ( YUMTIMES, "Times" );
&smlhdr ( YUMINSUPD, "InsUpd" );
&smlhdr ( YUMPKGNAME, "PackageName" );
&smlhdr ( YUMERASED, "Erased" );
&smlhdr ( YUMARCH, "Arch" );
&smlhdr ( YUMMAJOR, "Major" );
&smlhdr ( YUMMINOR, "Minor" );

&printField ( YUMDATES, "dates", @dates );
&printField ( YUMTIMES, "times", @times );
&printField ( YUMINSUPD, "insupdTokens", @insupd );
&printField ( YUMPKGNAME, "packagenameTokens", @insupdpkg );
&printField ( YUMERASED, "erasedTokens", @erased );
&printField ( YUMERASED, "erasedpkgTokens", @erasedpkg );
&printField ( YUMARCH, "archTokens", @insupdarch );
&printField ( YUMMAJOR, "majorTokens", @insupdmajor );
&printField ( YUMMINOR, "minorTokens", @insupdminor );

print YUMSML "    val sp1tok : LToken list = repeatLToken $recno (Pstring \" \") 6 6\n";
print YUMSML "    val sp2tok : LToken list = repeatLToken $recno (Pstring \" \") 15 15\n";

print YUMSML "    val colsptok : LToken list = repeatLToken $recno (Pstring \": \") 0 0\n";
print YUMSML "    val dottok : LToken list = repeatLToken $#insupd (Pstring \"\.\") 0 0\n";
print YUMSML "    val dashtok : LToken list = repeatLToken $#insupd (Pstring \"\-\") 0 0\n";
print YUMSML "    val eortok : LToken list = repeatLToken $#erased (Pstring \" \") 0 0\n";
print YUMSML "    val insupdsptok : LToken list = repeatLToken $#insupd (Pstring \" \") 0 0\n";

&smlend ( YUMSML );
&smlend ( YUMDATES );
&smlend ( YUMTIMES );
&smlend ( YUMINSUPD );
&smlend ( YUMPKGNAME );
&smlend ( YUMERASED );
&smlend ( YUMARCH );
&smlend ( YUMMAJOR );
&smlend ( YUMMINOR );

close (YUMTXT);
close (YUMSML);
close (YUMDATES);
close (YUMTIMES);
close (YUMINSUPD);
close (YUMPKGNAME);
close (YUMERASED);
close (YUMARCH);
close (YUMMAJOR);
close (YUMMINOR);
