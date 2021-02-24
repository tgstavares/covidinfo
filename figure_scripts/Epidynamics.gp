#set term postscript eps enhanced color round size 17cm,6cm #font 13
#set output "figure_scripts/fig_sim_deaths.eps"
set term dumb size 245,50

dat1 = "data/Epi_nofrictions.txt"
dat2 = "data/Epi_delays.txt"

set multiplot layout 1,4

set xtics in scale 0.00001
set ytics in
set xtics nomirror out

set xrange[2:499]
set title "Susceptible pop (days since begining of epidemic)"
set key b l Left
plot dat1 index 0 u 1:3 w l lw 5 lc 'black' title "nofrictions", \
     dat2 index 0 u 1:3 w l lw 5 lc 'black' title "delays"

#set xrange[2:499]
set title "total deaths (days since begining of epidemic)"
set key b r Right
plot dat1 index 0 u 1:4 w l lw 5 lc 'black' notitle "nofrictions", \
     dat2 index 0 u 1:4 w l lw 5 lc 'black' notitle "nofrictions"

set xrange[2:399]
set title "daily deaths (days since begining of epidemic)"
set key b r Right
plot dat1 index 0 u 1:5 w l lw 5 lc 'black' notitle "nofrictions", \
     dat2 index 0 u 1:5 w l lw 5 lc 'black' notitle "nofrictionsm"

#set xrange[2:199]
set title "hours supply (days since begining of epidemic)"
set key b r Right
plot dat1 index 0 u 1:7 w l lw 5 lc 'black' notitle "nofrictions", \
     dat2 index 0 u 1:7 w l lw 5 lc 'black' notitle "nofrictions"

# , \
#      dat2 index 0 u 1:12 w l lw 5 lc 'black' dt (2,2) title "6 day lagged beliefs", \
#      dat3 index 0 u 1:12 w l lw 5 lc 'black' dt (1,1) title "9 day lagged beliefs"

# set title "hours of susceptible (days since begining of epidemic)"
# set key t l Right
# plot dat1 index 0 u 1:($4) w l lw 5 lc 'black' notitle "rational beliefs", \
#      dat2 index 0 u 1:($4) w l lw 5 lc 'black' dt (2,2) notitle "rational beliefs", \
#      dat3 index 0 u 1:($4) w l lw 5 lc 'black' dt (1,1) notitle "rational beliefs"

unset multiplot
