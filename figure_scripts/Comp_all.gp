#set term dumb size 235,50
set term postscript eps enhanced color round size 22cm,12cm #font 13
set output "figure_scripts/fig_comp_all.eps"

dat1 = "data/Epi_nofrictions.txt"
dat2 = "data/Epi_delays_3.txt"
dat3 = "data/Epi_delays_4.txt"

set multiplot layout 2,3

#set xtics in scale 0.00001
set ytics in
set xtics nomirror in
set xtics 50
set grid front

set xrange[10:249]
set title "Susceptible pop (days since t_{0})"
set key b l Right
plot dat1 index 0 u 1:3 w l lw 5 lc 'black' title "no frictions", \
     dat2 index 0 u 1:3 w l lw 5 dt (1,1) lc 'red' title "Queretaro", \
     dat3 index 0 u 1:3 w l lw 5 dt (3,3) lc 'blue' title "Tamaulipas"

#set xrange[10:499]
set title "total deaths (days since t_{0})"
set key b r Right
plot dat1 index 0 u 1:4 w l lw 5 lc 'black' notitle "nofrictions", \
     dat2 index 0 u 1:4 w l lw 5 dt (1,1) lc 'red' notitle "nofrictions", \
     dat3 index 0 u 1:4 w l lw 5 dt (3,3) lc 'blue' notitle "nofrictions"

set xrange[10:249]
set title "daily deaths (days since t_{0})"
set key b r Right
plot dat1 index 0 u 1:5 w l lw 5 lc 'black' notitle "nofrictions", \
     dat2 index 0 u 1:5 w l lw 5 dt (1,1) lc 'red' notitle "nofrictionsm", \
     dat3 index 0 u 1:5 w l lw 5 dt (3,3) lc 'blue' notitle "nofrictionsm"

#set xrange[10:199]
set title "hours supply (days since t_{0})"
set key t r
plot dat1 index 0 u 1:7 w l lw 5 lc 'black' notitle "no frictions", \
     dat2 index 0 u 1:7 w l lw 5 dt (1,1) lc 'red' notitle "Querataro", \
     dat3 index 0 u 1:7 w l lw 5 dt (3,3) lc 'blue' notitle "Tamaulipas"

set xrange[10:249]
set title "log diff forecast error infected (days since t_{0})"
set key b r Right
plot dat2 index 0 u 1:(log($10)-log($9)) w l lw 5 dt (1,1) lc 'red'  notitle "delays as Queretaro", \
     dat3 index 0 u 1:(log($10)-log($9)) w l lw 5 dt (3,3) lc 'blue' notitle "delays as Tamaulipas"

#set xrange[30:499]
set title "reporting delay (days since t_{0})"
set key b r Right
plot dat2 index 0 u 1:11 w l lw 5  dt (1,1) lc 'red'  notitle "nofrictions", \
     dat3 index 0 u 1:11 w l lw 5  dt (3,3) lc 'blue' notitle "nofrictions"

# , \
#      dat2 index 0 u 1:12 w l lw 5 lc 'black' dt (2,2) title "6 day lagged beliefs", \
#      dat3 index 0 u 1:12 w l lw 5 lc 'black' dt (1,1) title "9 day lagged beliefs"

# set title "hours of susceptible (days since t_{0})"
# set key t l Right
# plot dat1 index 0 u 1:($4) w l lw 5 lc 'black' notitle "rational beliefs", \
#      dat2 index 0 u 1:($4) w l lw 5 lc 'black' dt (2,2) notitle "rational beliefs", \
#      dat3 index 0 u 1:($4) w l lw 5 lc 'black' dt (1,1) notitle "rational beliefs"

unset multiplot
