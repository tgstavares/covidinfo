#set term dumb size 235,50
set term postscript eps enhanced color round size 18cm,6cm #font 13
set output "figure_scripts/fig_histograms.eps"

data = "Delays_mexico_states.csv"

set multiplot layout 1,2
set style data histogram

#set xtics in scale 0.00001
set ytics in
set xtics nomirror out

binwidth = 2
bin(x,width)=width*floor(x/width) + binwidth/2.0

#boxes fill pattern 1
set style fill solid 0.25
set xrange[0:60]
set yrange[0:0.5]

set title "Death reporting delay distribution (days) - Queretaro"
set boxwidth binwidth
plot data using (bin($0,binwidth)):($3) smooth freq with boxes lc 'red' notitle

set title "Death reporting delay distribution (days) - Tamaulipas"
set boxwidth binwidth
plot data using (bin($0,binwidth)):($4) smooth freq with boxes lc 'blue' notitle
