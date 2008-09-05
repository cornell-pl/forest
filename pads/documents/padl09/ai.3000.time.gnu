set term postscript eps "Helvetica" 14
set style data lp
set xlabel "Percentage of chunks in ai.3000 (%)"
set xrange [0:50]
set ylabel "Execution time (secs)"
set output 'ai_1.eps'
plot "ai.3000.time.dat"
