set term postscript eps enhanced color round size 17cm,6cm #font 13
set output "figure_scripts/fig_lagged_days.eps"
#set term dumb size 215,55

dat1 = "data/maxdailydeathsdelays.txt"

set multiplot layout 1,2

set xtics in scale 0.00001
set ytics in
set xtics nomirror out

set xrange[0:20]
#set xrange[0:300*4]

set title "maximum daily deaths during the pandemic"
set xlabel "days of lagged beliefs"
set key b r Right
plot dat1 index 0 u 1:2 w l lw 5 lc 'black' notitle "unlagged beliefs"

set title "total deaths at the day 120 of the pandemic"
set key t l Right
plot dat1 index 0 u 1:3 w l lw 5 lc 'black' notitle "rational beliefs"

unset multiplot
