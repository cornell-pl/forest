set term postscript eps "Helvetica" 14
set style data lp
set xlabel "Normalized Type Complexity (%)"
#set key top left
set ylabel "Min training size"
set output 'traintycomp.eps'
plot 'train-tycomp.dat' index 2 title "90% Accuracy", 'train-tycomp.dat' index 3 title "95% Accuracy"
