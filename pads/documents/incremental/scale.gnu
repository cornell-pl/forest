set term postscript eps size 5, 2.5 "Helvetica" 14
set key top left
set style data lp
set xlabel "Number of Lines (in 1000's)"
set ylabel "Total Exec Time (secs)"
set output 'scale.eps'
plot 'million.dat'  title "Learning Time for Large Web Logs"
