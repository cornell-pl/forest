set term postscript eps "Helvetica" 14
set style data lp
set xlabel "Training size (%)"
set key top left
set ylabel "Avg Time (secs)"
set output 'traintime.eps'
plot 'time.plot' index 0 title "1967Transactions.short", 'time.plot' index 1 title "MER_T01_01.csv", 'time.plot' index 2 title "ai.3000", 'time.plot' index 3 title "asl.log", 'time.plot' index 4 title "boot.log", 'time.plot' index 5 title "crashreporter.log", 'time.plot' index 6 title "crashreporter.log.modified", 'time.plot' index 7 title "dibbler.1000", 'time.plot' index 8 title "ls-l.txt", 'time.plot' index 9 title "netstat-an", 'time.plot' index 10 title "page_log", 'time.plot' index 11 title "quarterlypersonalincome", 'time.plot' index 12 title "railroad.txt", 'time.plot' index 13 title "scrollkeeper.log", 'time.plot' index 14 title "windowserver_last.log", 'time.plot' index 15 title "yum.txt"
