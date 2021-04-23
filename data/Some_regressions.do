clear all
global HOME "/Users/tgst/Dropbox/PhD_Economics/research/my_projects/3rd_year_project_3rd_year_project/007_CovidInfo/EqModel/data"
cd $HOME

use Epi_delays.dta

tsset t
gen t2=t^2
gen t3=t^3
gen logd = log(Deaths_pop)
gen logdr = log(DeathsReported_pop)
gen logi = log(Infected_share)
gen logib = log(InfectedBelief_share)
gen lognewdeaths = log(NewDeaths_pop)
gen lognewdeathsr = log(NewDeathsReported_pop)
gen Infected_dif = Infected_share-InfectedBelief_share
gen logsus = log(Susceptible_share)
gen lh = log(Hours_wrtSS)
gen li = log(Infected_share)
gen lt = log(TransmissibilityRisk)
gen lib = log(InfectedBelief_share)
gen logdiff_infected = log(Infected_share)-log(InfectedBelief_share)
gen logdiff_deaths = log(Deaths_pop)-log(DeathsReported_pop)
gen logpi = log(TransmissibilityRisk)

reg NewDeaths_pop l2.Infected_pop l16.NewDeathsReported_pop l16.NewDeaths_pop if t<200
reg lognewdeaths l16.lognewdeathsr l16.lognewdeaths if t<200

reg logsus l.logsus l.Hours_wrtSS l.Infected_share l.InfectedBelief_share t t2 t3 if t<200
reg lt lh li lib if t <250

reg s.logd lh li lib if t <250
reg s.logd l5.s.lh l6.s.lh l6.lognewdeaths l6.lognewdeathsr t t2 t3 if t < 200
reg s.logd l6.lognewdeaths l6.lognewdeathsr t t2 t3 if t < 200
gen newcases = l.Susceptible_share - Susceptible_share
gen logNew = log(newcases)

* Reg
reg TransmissibilityRisk logi logdiff_infected if t < 200 & t>20
reg TransmissibilityRisk logi logdiff_deaths if t < 200 & t>20
reg s.lh l1.s.TransmissibilityRisk l1.logdiff_infected  if t < 200 & t>20
reg s.lh l1.s.TransmissibilityRisk l1.logdiff_deaths if t < 200 & t>20

reg logNew l3.logdr l3.logd if t<200 & t>20
reg logNew l3.logdr l3.logd l.lh if t<200 & t>20

reg s.logsus l3.s.logdr l3.s.logd l.s.lh if t<200 & t>20
reg s.logsus l3.s.logdiff_deaths if t<200 & t>20
