## Description of programs for the `Covidinfo` project

#### Program `main.f90`

Entry in the makefile is `main`. This is the main program of the code. Includes the baseline model and calibration which has to be set manually. The main routines in this program are from `equilibrium.f90` and `values.f90`. The later collects intermediate routines and the former runs an equilibrium. The program outputs statistics and time series simulations.

The sources `main_time.f90` and `main_tec.f90` are variations of the the baseline model and have specific entries in the makefile. 

#### Gnuplot scripts: inside folder `./figure_scripts`

Scripts to generate figures. `Epidynamics.gp` is for a time series of the epidemic; `Delaysdynamics.gp` is for time series related with delays; `Histograms_delays.gp` plots an histogram of delays from the data.






