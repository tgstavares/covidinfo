clear all
global HOME "/Users/tgst/Dropbox/PhD_Economics/research/my_projects/3rd_year_project_3rd_year_project/007_CovidInfo/To_github/EqModel/data"
cd $HOME

use Epi_delays.dta

tsset t
gen t2=t^2
gen t3=t^3
gen t4=t^4
gen t5=t^5
gen t6=t^6
gen t7=t^7
gen t8=t^8
gen t9=t^9
gen t10=t^10
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

/*
reg NewDeaths_pop l2.Infected_pop l16.NewDeathsReported_pop l16.NewDeaths_pop if t<200
reg lognewdeaths l16.lognewdeathsr l16.lognewdeaths if t<200

reg logsus l.logsus l.Hours_wrtSS l.Infected_share l.InfectedBelief_share t t2 t3 if t<200
reg lt lh li lib if t <250

reg s.logd lh li lib if t <250
reg s.logd l5.s.lh l6.s.lh l6.lognewdeaths l6.lognewdeathsr t t2 t3 if t < 200
reg s.logd l6.lognewdeaths l6.lognewdeathsr t t2 t3 if t < 200
*/
gen newcases = l.Susceptible_share - Susceptible_share
gen logNew = log(newcases)

/*
* Reg
reg TransmissibilityRisk logi logdiff_infected if t < 200 & t>20
reg TransmissibilityRisk logi logdiff_deaths if t < 200 & t>20
reg s.lh l1.s.TransmissibilityRisk l1.logdiff_infected  if t < 200 & t>20
reg s.lh l1.s.TransmissibilityRisk l1.logdiff_deaths if t < 200 & t>20

reg logNew l3.logdr l3.logd if t<200 & t>20
reg logNew l3.logdr l3.logd l.lh if t<200 & t>20

reg s.logsus l3.s.logdr l3.s.logd l.s.lh if t<200 & t>20
reg s.logsus l3.s.logdiff_deaths if t<200 & t>20

* More
*/
gen timew3 = floor(t/5)
tab timew3 , gen(tt)

/*
reg f10.s10.Susceptible_share tt* l5.logdr l5.logd l5.s5.logd if t<150 & t >50
*/

gen lognsus = log(1-Susceptible_share)

/*
reg logdr l.logd l2.logd l3.logd l4.logd

** More More - These seem to work!!
reg s.lognsus tt* l.s.lognsus l2.Infected_share l2.InfectedBelief_share if t>60 & t<180
*/

gen cases=1-Susceptible_share 
gen grcases1 = (cases[_n]-cases[_n-1])/cases[_n-1]

reg f.s.lognsus l.s.lognsus l.logdr l.logd l.s.logd if t>30 & t<120
reg f.s.lognsus l.s.lognsus l.logdr l.logd l.s.logd if t>90 & t<250
reg f.s.lognsus l.s.lognsus l.logdr l.logd l.s.logd if t>30 & t<270


reg f.grcases1                               l.logd l.logdr if t>60 & t<200
reg f.grcases1 l.grcases1                    l.logd l.logdr if t>50 & t<200
reg f.grcases1 l.grcases1                    l.logd l.logdr if t>30 & t<200
reg f.grcases1 l.s.logd l.s.logdr l.grcases1 l.logd l.logdr if t>95 & t<200


