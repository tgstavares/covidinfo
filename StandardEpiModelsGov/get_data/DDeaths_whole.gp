#set term dumb size 235,50
set term postscript eps enhanced color round size 18cm,6cm #font 13
set output "ddeaths_country.eps"

set multiplot layout 1,1

#set xtics in scale 0.00001
set ytics in
set xtics nomirror out


set datafile separator comma
dat2 = "Data_Reported_all_both.csv"

set title "Daily deaths - Mexico"
set xdata time
set timefmt "%Y-%m-%d"
set xrange["2020-04-13":"2020-10-01"]

back2 = back1 = 0
shift(x) = (back2 = back1, back1 = x)

set key t l
plot dat2 using 2:(shift($4), $0<1 ? 1/0: $4-back2) w l lw 5 lc 'black' title "occurred", \
     dat2 using 2:(shift($6), $0<1 ? 1/0: $6-back2) w l lw 5 dt (1,1) lc 'red' title "reported"

unset multiplot
