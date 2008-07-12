set term postscript eps size 5, 2.5 "Helvetica" 14
set style data lp
set key top left
set xrange [0:850]
set xlabel "Number of nodes"
set ylabel "Average Latency (secs)"
set output 'latency.eps'
plot 'latency.dat' index 0 title "Network (without archiving)", 'latency.dat' index 1 title "System (without archiving)", 'latency.dat' index 2 title "Network (with archiving)", 'latency.dat' index 3 title "System (with archiving)"
