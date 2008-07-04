set term postscript eps "Helvetica" 14
set style data lp
set xlabel "Number of nodes"
#set key top left
set ylabel "Thoughput (items per sec)"
set output 'throughput.eps'
plot 'throughput.dat' index 0 title "Without archiving", 'throughput.dat' index 1 title "With archiving"
