#set term dumb size 235,50
set term postscript eps enhanced color round size 18cm,6cm #font 13
set output "predictions.eps"

set multiplot layout 1,2

#set xtics in scale 0.00001
set ytics in
set xtics nomirror out


set datafile separator comma
dat0  = "get_data/Data_Reported_all_both.csv"
dat1  = "model1_Gompertz/export_predictions_model1.csv"
dat21 = "model2_SEIRD/export_predictions_reported_model2.csv"
dat22 = "model2_SEIRD/export_predictions_occurred_model2.csv"

set yrange[0:150000]

set title "Total deaths using data as occurred - Mexico"
set xdata time
set timefmt "%Y-%m-%d"
set xrange["2020-04-12":"2020-10-01"]
set key t l samplen 1
plot dat0  using 2:4  w p pt 2 ps 0.25 lc 'black' title "data as occurred", \
     dat1  using 2:10 w l lw 5 lc 'black' dt(1,1) title "prdiction model (1)", \
     dat22 using 1:3  w l lw 5 lc 'black' title "prediction model (2)"

set title "Total deaths using data as reported - Mexico"
set xdata time
set timefmt "%Y-%m-%d"
set xrange["2020-04-12":"2020-10-01"]
set key t l samplen 1
plot dat0  using 2:6  w p pt 2 ps 0.25 lc 'red' title "data as reported", \
     dat1  using 2:9  w l lw 5 lc 'red' dt(1,1) title "prediction model (1)", \
     dat21 using 1:3  w l lw 5 lc 'red' title "prediction model (2)"


# , \
#      dat2 using 2:6 w l lw 5 dt (1,1) lc 'red' title "reported"

# set title "Total cases - Mexico"
# set xdata time
# set timefmt "%Y-%m-%d"
# set xrange["2020-04-12":"2020-10-01"]
# set key t l
# plot dat2 using 2:3 w l lw 5 lc 'black' title "occurred", \
#      dat2 using 2:5 w l lw 5 dt (1,1) lc 'red' title "reported"

unset multiplot
