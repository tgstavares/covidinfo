clear all
global HOME "/Users/tgst/Dropbox/PhD_Economics/research/my_projects/3rd_year_project_3rd_year_project/007_CovidInfo/To_github/EqModel/data"
cd $HOME

* No delays
import delimited "Epi_nofrictions.txt", delimiter(space, collapse) encoding(ISO-8859-1)
drop v1
ren v2  t
ren v3  Infected_pop
ren v4  Susceptible_share
ren v5  Deaths_pop
ren v6  NewDeaths_pop
ren v7  TransmissibilityRisk
ren v8  Hours_wrtSS
save Epi_nofrictions.dta, replace


* No Delays
clear all
import delimited "Epi_delays.txt", delimiter(space, collapse) encoding(ISO-8859-1)
drop v1
ren v2  t
ren v3  Infected_pop
ren v4  Susceptible_share
ren v5  Deaths_pop
ren v6  NewDeaths_pop
ren v7  TransmissibilityRisk
ren v8  Hours_wrtSS
ren v9  aux
ren v10 Infected_share
ren v11 InfectedBelief_share
ren v12 Delay
ren v13 DeathsReported_pop
ren v14 NewDeathsReported_pop
save Epi_delays.dta, replace

/*
* Some figs
twoway (line Deaths_pop t) (line DeathsReported_pop t) , name(f1)
twoway (line NewDeaths_pop t) (line NewDeathsReported_pop t) , name(f2)
twoway (line Infected_share t) (line InfectedBelief_share t) , name(f3)
twoway (line Delay t), name(f4) // after the vaccine no precision on this measure
*/
