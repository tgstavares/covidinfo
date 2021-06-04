#set term dumb size 235,50
set term postscript eps enhanced color round size 18cm,6cm #font 13
set output "figure_scripts/fig_histograms_country.eps"


set multiplot layout 1,2

#set xtics in scale 0.00001
set ytics in
set xtics nomirror out


set title "Total deaths - Mexico"
set datafile separator comma
dat2 = "Data_Reported_all_both.csv"
set xdata time
set timefmt "%Y-%m-%d"
set xrange["2020-04-12":"2020-10-01"]
set key t l
plot dat2 using 2:3 w l lw 5 lc 'black' title "occurred", \
     dat2 using 2:4 w l lw 5 dt (1,1) lc 'red' title "reported"

set xdata
set title "Death reporting delay distribution (days) - Mexico"
set datafile separator "\t"
data = "Delays_mexico_states.csv"
set style data histogram
binwidth = 2
set boxwidth binwidth
bin(x,width)=width*floor(x/width) + binwidth/2.0
#boxes fill pattern 1
set style fill solid 0.25
set xrange[0:60]
set yrange[0:0.5]
set boxwidth binwidth
plot data using (bin($0,binwidth)):($1) smooth freq with boxes lc 'black' notitle
