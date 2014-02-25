set term postscript eps "Helvetica" 14
set style data lp
set xlabel "Proportion of Orig Data"
set key top left
set ylabel "Learning Time (secs)"
set output 'scale-default.eps'
plot 'default.scale.dat' index 0 title "free_clickthroughs.dat", 'default.scale.dat' index 1 title "thirdpartycontent.log", 'default.scale.dat' index 2 title "eventstream.current", 'default.scale.dat' index 3 title "strace_jaccn.dat", 'default.scale.dat' index 4 title "LA-UR-EVENTS.cvs", 'default.scale.dat' index 5 title "messages.sdb", 'default.scale.dat' index 6 title "HALO_have2impression.log", 'default.scale.dat' index 7 title "LA-UR-NODE-NOZ.TXT", 'default.scale.dat' index 8 title "searchevents.data", 'default.scale.dat' index 9 title "4046.xls"

set output 'scale-uc.eps'
plot 'uc.scale.dat' index 0 title "free_clickthroughs.dat", 'uc.scale.dat' index 1 title "thirdpartycontent.log", 'uc.scale.dat' index 2 title "eventstream.current", 'uc.scale.dat' index 3 title "strace_jaccn.dat", 'uc.scale.dat' index 4 title "LA-UR-EVENTS.cvs", 'uc.scale.dat' index 5 title "messages.sdb", 'uc.scale.dat' index 6 title "HALO_have2impression.log", 'uc.scale.dat' index 7 title "LA-UR-NODE-NOZ.TXT", 'uc.scale.dat' index 8 title "searchevents.data", 'uc.scale.dat' index 9 title "4046.xls"
