#set term postscript eps enhanced color round size 17cm,6cm #font 13
#set output "figure_scripts/fig_sim_deaths.eps"
set term dumb size 215,55

dat1 = "data/simul1.txt"
dat2 = "data/simul2.txt"
dat3 = "data/simul3.txt"

set multiplot layout 1,2

set xtics in scale 0.00001
set ytics in
set xtics nomirror out

set xrange[0:150]
#set xrange[0:300*4]

set title "daily deaths (days since begining of epidemic)"
set key b r Right
plot dat1 index 0 u 1:12 w l lw 5 lc 'black' title "unlagged beliefs", \
     dat2 index 0 u 1:12 w l lw 5 lc 'black' dt (2,2) title "6 day lagged beliefs", \
     dat3 index 0 u 1:12 w l lw 5 lc 'black' dt (1,1) title "9 day lagged beliefs"

set title "hours of susceptible (days since begining of epidemic)"
set key t l Right
plot dat1 index 0 u 1:($4) w l lw 5 lc 'black' notitle "rational beliefs", \
     dat2 index 0 u 1:($4) w l lw 5 lc 'black' dt (2,2) notitle "rational beliefs", \
     dat3 index 0 u 1:($4) w l lw 5 lc 'black' dt (1,1) notitle "rational beliefs"

unset multiplot
