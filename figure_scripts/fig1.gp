set term postscript eps enhanced color round size 22cm,11cm #font 13
set output "figure_scripts/fig_sim.eps"
#set term dumb size 235,55

dat1 = "data/simul1.txt"
dat2 = "data/simul2.txt"
dat3 = "data/simul3.txt"

set multiplot layout 2,3

set xtics in scale 0.00001
set ytics in
set xtics nomirror out

set xrange[0:150]

set title "infected (days since init)"
set key b r Right
plot dat1 index 0 u 1:7 w l lw 5 lc 'black' title "unlagged beliefs", \
     dat2 index 0 u 1:7 w l lw 5 lc 'black' dt (2,2) title "6 day lagged beliefs", \
     dat3 index 0 u 1:7 w l lw 5 lc 'black' dt (1,1) title "9 day lagged beliefs"

set title "total cases (days since init)"
set key t l Right
plot dat1 index 0 u 1:(1-$6) w l lw 5 lc 'black' notitle "rational beliefs", \
     dat2 index 0 u 1:(1-$6) w l lw 5 lc 'black' dt (3,3) notitle "rational beliefs", \
     dat3 index 0 u 1:(1-$6) w l lw 5 lc 'black' dt (1,1) notitle "rational beliefs"

set title "total deaths (days since init)"
set key t l Right
plot dat1 index 0 u 1:9 w l lw 5 lc 'black' notitle "rational beliefs", \
     dat2 index 0 u 1:9 w l lw 5 lc 'black' dt (3,3) notitle "rational beliefs", \
     dat3 index 0 u 1:9 w l lw 5 lc 'black' dt (1,1) notitle "rational beliefs"

set title "probability of infection (days since init)"
set key t l Right
plot dat1 index 0 u 1:($2) w l lw 5 lc 'black' notitle "rational beliefs", \
     dat2 index 0 u 1:($2) w l lw 5 lc 'black' dt (3,3) notitle "rational beliefs", \
     dat3 index 0 u 1:($2) w l lw 5 lc 'black' dt (1,1) notitle "rational beliefs"

set title "hours of susceptible (days since init)"
set key t l Right
plot dat1 index 0 u 1:($4) w l lw 5 lc 'black' notitle "rational beliefs", \
     dat2 index 0 u 1:($4) w l lw 5 lc 'black' dt (3,3) notitle "rational beliefs", \
     dat3 index 0 u 1:($4) w l lw 5 lc 'black' dt (1,1) notitle "rational beliefs"

set title "aggregate hours (days since init)"
set key t l Right
plot dat1 index 0 u 1:($11) w l lw 5 lc 'black' notitle "rational beliefs", \
     dat2 index 0 u 1:($11) w l lw 5 lc 'black' dt (3,3) notitle "rational beliefs", \
     dat3 index 0 u 1:($11) w l lw 5 lc 'black' dt (1,1) notitle "rational beliefs"

unset multiplot
