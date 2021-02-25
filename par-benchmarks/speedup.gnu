set xlabel 'Threads'
set ylabel 'Speedup'
set key right top outside
set datafile separator " "
plot for [col=2:14] data using 1:col with lines title columnheader, \
     x with lines lt rgb 'black' lw 2 notitle
