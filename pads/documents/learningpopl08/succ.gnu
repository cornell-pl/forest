set term postscript eps "Helvetica" 14
set style data lp
set xlabel "Training size (%)"
set key right bottom 
set ylabel "Success rate (%)"
set output 'successrate.eps'
plot 'succ.plot' index 0 title "1967Transactions.short", 'succ.plot' index 1 title "MER_T01_01.csv", 'succ.plot' index 2 title "ai.3000", 'succ.plot' index 3 title "asl.log", 'succ.plot' index 4 title "boot.log", 'succ.plot' index 5 title "crashreporter.log", 'succ.plot' index 6 title "crashreporter.log.modified", 'succ.plot' index 7 title "dibbler.1000", 'succ.plot' index 8 title "ls-l.txt", 'succ.plot' index 9 title "netstat-an", 'succ.plot' index 10 title "page_log", 'succ.plot' index 11 title "quarterlypersonalincome", 'succ.plot' index 12 title "railroad.txt", 'succ.plot' index 13 title "scrollkeeper.log", 'succ.plot' index 14 title "windowserver_last.log", 'succ.plot' index 15 title "yum.txt"
