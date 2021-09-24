#set term dumb size 235,50
set term postscript eps enhanced color round size 18cm,6cm #font 13
set output "fig_country.eps"

set multiplot layout 1,2

#set xtics in scale 0.00001
set ytics in
set xtics nomirror out


set datafile separator comma
dat2 = "Data_Reported_all_both.csv"

set title "Total deaths - Mexico"
set xdata time
set timefmt "%Y-%m-%d"
set xrange["2020-04-12":"2020-10-01"]
set key t l
plot dat2 using 2:4 w l lw 5 lc 'black' title "occurred", \
     dat2 using 2:6 w l lw 5 dt (1,1) lc 'red' title "reported"

set title "Total cases - Mexico"
set xdata time
set timefmt "%Y-%m-%d"
set xrange["2020-04-12":"2020-10-01"]
set key t l
plot dat2 using 2:3 w l lw 5 lc 'black' title "occurred", \
     dat2 using 2:5 w l lw 5 dt (1,1) lc 'red' title "reported"

unset multiplot
