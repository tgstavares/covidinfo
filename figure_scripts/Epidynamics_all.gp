#set term dumb size 235,50
set term postscript eps enhanced color round size 22cm,12cm #font 13
set output "figure_scripts/fig_Epy_dynamic_all.eps"

dat1 = "data/Epi_nofrictions.txt"
dat2 = "data/Epi_delays.txt"

set multiplot layout 2,4

set xtics in scale 0.00001
set ytics in
set xtics nomirror out
set xtics 50
set grid front

set xrange[10:249]
set title "susceptible pop (days since t_{0})"
set key b r Right
plot dat1 index 0 u 1:3 w l lw 5 lc 'black' title "nofrictions (black lines)", \
     dat2 index 0 u 1:3 w l lw 5 dt (1,1) lc 'red' title "delays (red lines)"

#set xrange[10:499]
set title "total deaths (days since t_{0})"
set key b r Right
plot dat1 index 0 u 1:4 w l lw 5 lc 'black' notitle "nofrictions", \
     dat2 index 0 u 1:4 w l lw 5 dt (1,1) lc 'red' notitle "nofrictions"

set xrange[10:249]
set title "daily deaths (days since t_{0})"
set key b r Right
plot dat1 index 0 u 1:5 w l lw 5 lc 'black' notitle "nofrictions", \
     dat2 index 0 u 1:5 w l lw 5 dt (1,1) lc 'red' notitle "nofrictionsm"

#set xrange[10:199]
set title "hours supply (days since t_{0})"
set key t r
plot dat1 index 0 u 1:8  w l lw 5 lc 'black' notitle "nofrictions", \
     dat2 index 0 u 1:14 w l lw 5 dt (1,1) lc 'red' notitle "delays"

set xrange[10:259]
set title "Infected beliefs (days since t_{0})"
set key b r Right
plot dat2 index 0 u 1:9 w l lw 5 lc 'red' title "real", \
     dat2 index 0 u 1:10 w l lw 5 dt (1,1) lc 'red' title "forecast"

#set xrange[10:499]
set title "Delays (days since t_{0})"
set key b r Right
plot dat2 index 0 u 1:11 w l lw 5 dt (1,1) lc 'red' notitle "nofrictions"

set xrange[10:259]
set title "Total deaths (days since t_{0})"
set key b r Right
plot dat2 index 0 u 1:4  w l lw 5 lc 'red' title "occurred", \
     dat2 index 0 u 1:12 w l lw 5 dt (1,1) lc 'red' title "reported"

#set xrange[10:199]
set title "Daily deaths (days since t_{0})"
set key b r Right
plot dat2 index 0 u 1:5  w l lw 5 lc 'red' title "occurred", \
     dat2 index 0 u 1:13 w l lw 5 dt (1,1) lc 'red' title "reported"

# , \
#      dat2 index 0 u 1:12 w l lw 5 lc 'black' dt (2,2) title "6 day lagged beliefs", \
#      dat3 index 0 u 1:12 w l lw 5 lc 'black' dt (1,1) title "9 day lagged beliefs"

# set title "hours of susceptible (days since t_{0})"
# set key t l Right
# plot dat1 index 0 u 1:($4) w l lw 5 lc 'black' notitle "rational beliefs", \
#      dat2 index 0 u 1:($4) w l lw 5 lc 'black' dt (2,2) notitle "rational beliefs", \
#      dat3 index 0 u 1:($4) w l lw 5 lc 'black' dt (1,1) notitle "rational beliefs"

unset multiplot
