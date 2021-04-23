set term dumb size 235,50
#set term postscript eps enhanced color round size 22cm,6cm #font 13
#set output "figure_scripts/fig_Delays_dynamic.eps"

dat1 = "data/Epi_nofrictions.txt"
dat2 = "data/Epi_delays.txt"

set multiplot layout 1,4

set xtics in scale 0.00001
set ytics in
set xtics nomirror out
set xtics 50
set grid front

set xrange[10:259]
set title "Infected (days since t_{0})"
set key b r Right
plot dat2 index 0 u 1:9 w l lw 5 lc 'black' title "real", \
     dat2 index 0 u 1:10 w l lw 5 dt (1,1) lc 'red' title "forecast"

#set xrange[10:499]
set title "Delays (days since t_{0})"
set key b r Right
plot dat2 index 0 u 1:11 w l lw 5 dt (1,1) lc 'red' notitle "nofrictions"

set xrange[10:259]
set title "Total deaths (days since t_{0})"
set key b r Right
plot dat2 index 0 u 1:4  w l lw 5 lc 'black' title "occurred", \
     dat2 index 0 u 1:12 w l lw 5 dt (1,1) lc 'red' title "reported"

#set xrange[10:199]
set title "Daily deaths (days since t_{0})"
set key b r Right
plot dat2 index 0 u 1:5  w l lw 5 lc 'black' title "occurred", \
     dat2 index 0 u 1:13 w l lw 5 dt (1,1) lc 'red' title "reported"

unset multiplot
