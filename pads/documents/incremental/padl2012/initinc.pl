#!/usr/bin/perl
($path, $name) = @ARGV;
if (!$path) 
{
 print "Usage: initinc.pl PATH NAME\n";
 exit;
}

open (FILE, "<$path") or die "Can't open $path for read";
local %tab;
while (<FILE>)
{
  if (/$name \(init=(\d+), inc=(\d+)\): lntime = ([0-9.]+).*score = ([0-9.]+).*dist = ([0-9.]+)/)
  {
   $key = $1."-".$2;
   $tab{$key} = "$3 $4 $5";
  }
}

$escaped_name = $name;
$escaped_name =~ s/_/\\_/g;

print "\\begin{table}[!t]
\\caption{$escaped_name}
\\label{tab:$name}
\\centering
\\begin{tabular}{|c||c|c|c|c|c|}
\\hline
init/inc & 25 & 100 & 400 & 1600 & 6400 \\\\ \\hline \\hline\n";

for ($i = 500; $i<=64000; $i=$i*2)
{
 $line1 = " ";
 $line2 = "$i ";
 $line3 = " ";
 for ($j=25; $j<=6400; $j=$j*4)
 {
  my $key = "$i-$j";
  my @list = split (/ /, $tab{$key});
  $line1 = $line1 . " & $list[0]"; 
  $line2 = $line2 . " & $list[1]"; 
  $line3 = $line3 . " & $list[2]"; 
 }
 print $line1 . "\\\\ \n";
 print $line2 . "\\\\ \n";
 print $line3 . "\\\\ \\hline \n";
}
print "\\end{tabular}
\\end{table}\n\n";
close FILE;
