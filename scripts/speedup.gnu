set xlabel 'Cores'
set ylabel 'Speedup'
set key above
set datafile separator " "
set style function linespoints
set y2tics 1,2
set grid y2tics lt 1 lw 1 lc rgb "#000000"
set xrange [1:20]
set xtics nomirror 1,3
set yrange [1:20]
unset ytics
plot for [col=2:14] data using 1:col with linespoints title columnheader axes x1y2, \
     x with lines lt rgb 'black' lw 2 notitle axes x1y2
