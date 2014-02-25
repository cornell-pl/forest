set term postscript eps size 5,2.5 "Helvetica" 14
set key top right
set style data lp
set xlabel "Number of nodes"
set xrange [0:850]
set ylabel "Thoughput (items per sec)"
set output 'throughput.eps'
plot 'throughput.dat' index 0 title "Without archiving", 'throughput.dat' index 1 title "With archiving"
