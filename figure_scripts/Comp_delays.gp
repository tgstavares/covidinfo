set term dumb size 235,50
#set term postscript eps enhanced color round size 18cm,6cm #font 13
#set output "figure_scripts/fig_comp_Delays_dynamic.eps"

dat1 = "data/Epi_nofrictions.txt"
dat2 = "data/Epi_delays_3.txt"
dat3 = "data/Epi_delays_4.txt"

set multiplot layout 1,2

#set xtics in #scale 0.00001
set ytics in
set xtics nomirror in
set xtics 50
set grid front

set xrange[10:249]
set title "log diff forecast error infected (days since t_{0})"
set key b r Right
plot dat2 index 0 u 1:(log($10)-log($9)) w l lw 5 dt (1,1) lc 'red'  title "delays as Queretaro", \
     dat3 index 0 u 1:(log($10)-log($9)) w l lw 5 dt (3,3) lc 'blue' title "delays as Tamaulipas"

#set xrange[30:499]
set title "reporting delay (days since t_{0})"
set key b r Right
plot dat2 index 0 u 1:11 w l lw 5  dt (1,1) lc 'red'  notitle "nofrictions", \
     dat3 index 0 u 1:11 w l lw 5  dt (3,3) lc 'blue' notitle "nofrictions"


unset multiplot
