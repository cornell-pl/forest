set style data histogram
set style histogram cluster gap 1
set style fill pattern border 1
#set xtic rotate by -45
set xtic nomirror
set yrange[0:100]
set key top left
set bmargin 2
set tmargin 1
set lmargin 4
set rmargin 0

#set border 9
#unset ytic
#set y2tic border nomirror
#set term postscript eps "Helvetica" 35 size 5, 2.5
#set boxwidth 0.9 relative
#set xrange[-0.5:0.5]
#set output 'histogram4.eps'
#plot 'histogram4.dat' using 2 notitle, '' u 3 notitle, '' u 4 notitle, '' u 5 notitle, '' u 6 notitle, '' u 7 notitle, '' u 8 notitle, '' u 9 notitle, '' u 10 notitle, '' u 11 notitle, '' u 12 notitle, '' u 13 notitle, '' u 14 notitle, '' u 15 notitle, '' u 16 notitle,'' u 17 notitle,'' u 18 notitle,'' u 19 notitle,'' u 20 notitle,'' u 21:xticlabels(1) notitle
#
#unset ytic
#unset y2tic
#set border 1
#set term postscript eps "Helvetica" 35 size 4.5, 2.5
#set boxwidth 0.9 relative
#set xrange[-0.5:1.5]
#set output 'histogram2.eps'
#plot 'histogram2.dat' using 2 notitle, '' u 3 notitle, '' u 4:xtic(1) notitle, '' u 5 notitle, '' u 6 notitle, '' u 7 notitle, '' u 8 notitle, '' u 9:xticlabels(1) notitle
#
#unset ytic
#unset y2tic
#set border 1
#set term postscript eps "Helvetica" 35 size 2.5, 2.5
#set boxwidth 0.9 relative
#set xrange[-0.5:2.5]
#set output 'histogram3.eps'
#plot 'histogram3.dat' using 2 notitle, '' u 3 notitle, '' u 4:xticlabels(1) notitle 

#set ytic nomirror
##unset y2tic
#set border 3
#set term postscript eps "Helvetica" 35 size 5, 3.5
#set boxwidth 0.9 relative
#set xrange[-2.5:2.5]
#set output 'histogram1-1.eps'
#plot 'histogram1-1.dat' using 2 title "0 times", '' u 3 title "1 time", '' u 4:xticlabels(1) title "2 times" 

set ytic nomirror
#unset y2tic
set border 3
set term postscript eps "Helvetica" 35 size 5, 3
set boxwidth 0.9 relative
set xrange[-1.5:3.5]
set output 'histogram1-2.eps'
plot 'histogram1-2.dat' using 2 title "0 time", '' u 3 title "1 time", '' u 4:xticlabels(1) title "2 times" 

#set ytic nomirror
##unset y2tic
#set border 3
#set term postscript eps "Helvetica" 35 size 5, 3.5
#set boxwidth 0.9 relative
#set xrange[-0.5:4.5]
#set output 'histogram0.eps'
#plot 'histogram0.dat' using 2 title "0 times", '' u 3 title "1 time", '' u 4:xticlabels(1) title "2 times" 

