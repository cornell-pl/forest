set term postscript eps "Helvetica" 14
set style data histogram
set style histogram cluster gap 1
set style fill pattern border -1
set boxwidth 0.9
set auto x
set xtic rotate by -30 scale 0
set yrange [0:6]
set ylabel "Normalized edit distance score"
set output 'adc.eps'
plot 'uc.adc.dat' using 2:xtic(1) ti col, '' u 3 ti col, '' u 4 ti col, '' u 5 ti col, '' u 6 ti col
