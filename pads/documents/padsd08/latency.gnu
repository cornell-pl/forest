set term postscript eps "Helvetica" 14
set style data lp
set xlabel "Number of nodes"
#set key top left
set ylabel "Average Latency (secs)"
set output 'latency.eps'
plot 'latency.dat' index 0 title "Network (without archiving)", 'latency.dat' index 1 title "System (without archiving)", 'latency.dat' index 2 title "Network (with archiving)", 'latency.dat' index 3 title "System (with archiving)"
