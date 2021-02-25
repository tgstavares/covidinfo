#set term postscript eps enhanced color round size 17cm,6cm #font 13
#set output "figure_scripts/fig_sim_deaths.eps"
set term dumb size 235,50

dat1 = "data/Epi_nofrictions.txt"
dat2 = "data/Epi_delays.txt"

set multiplot layout 1,4

set xtics in scale 0.00001
set ytics in
set xtics nomirror out

set xrange[2:449]
set title "Transmissability (days since begining of epidemic)"
set key b r Left
plot dat2 index 0 u 1:9 w l lw 5 lc 'black' title "real", \
     dat2 index 0 u 1:10 w l lw 5 lc 'black' title "forecast"

#set xrange[2:499]
set title "Delays (days since begining of epidemic)"
set key b r Right
plot dat2 index 0 u 1:11 w l lw 5 lc 'black' notitle "nofrictions"

set xrange[2:449]
set title "Total deaths (days since begining of epidemic)"
set key t l Right
plot dat2 index 0 u 1:4  w l lw 5 lc 'black' title "occurred", \
     dat2 index 0 u 1:12 w l lw 5 lc 'black' title "reported"

#set xrange[2:199]
set title "Daily deaths (days since begining of epidemic)"
set key t r Right
plot dat2 index 0 u 1:5  w l lw 5 lc 'black' title "occurred", \
     dat2 index 0 u 1:13 w l lw 5 lc 'black' title "reported"

unset multiplot
