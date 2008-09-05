set term postscript eps "Helvetica" 14
set style data lp
set xlabel "Percentage of chunks in ai.3000 used to infer description (%)"
set xrange [0:50]
set ylabel "Percentage of chunks in ai.3000 parsed correctly (%)"
set output 'ai_2.eps'
plot "ai.3000.accuracy.dat" notitle
