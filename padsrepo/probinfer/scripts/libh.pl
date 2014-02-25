#!/usr/bin/perl

$structurename = ucfirst($prefix."Lex");
my $PROBLEARN_HOME = $ENV{"PROBLEARN_HOME"};
$lexfile = $PROBLEARN_HOME."/src/libh.sml";
print "Writing to libh.sml...\n";
open (LEX, ">$lexfile") or die "Cannot open libh.sml for write!\n";
print LEX
"structure LibH = struct
    local 
	val lh = DynLinkage.open_lib
		    { name = \"$PROBLEARN_HOME/src/regex_c.so\", global = true, lazy = true }
    in
        fun libh s = let
	    val sh = DynLinkage.lib_symbol (lh, s)
	in
	    fn () => DynLinkage.addr sh
	end
    end
end; 
";
close LEX;
