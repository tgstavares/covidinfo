
clear all
global HOME "/Users/tgst/Dropbox/PhD_Economics/research/my_projects/3rd_year_project_3rd_year_project/007_CovidInfo/To_github/EqModel/StandardEpiModelsGov/model2_SEIRD"
cd $HOME

import delimited "../get_data/Data_Reported_all_both.csv", encoding(ISO-8859-1)
generate t = date(date, "YMD")
format t %td
order index date t

* Extrapolate data
gen lognr = log(casesr)
gen logno = log(caseso)
gen logdr = log(deathsr)
gen logdo = log(deathso)

* Extrapolate
local a=_N+1
set obs `a'
replace index = - 42 in `a'
sort index 
tsset index
tsfill, full
gen aux = -_n
sort aux 
replace t=t[_n-1]-1 if t[_n]==.
sort index 

nl (lognr = {b0=10} + {b2=-6}*exp({b3=-0.2}*index)) if index<=30, iterate(20)
predict temp, yhat
replace temp=exp(temp)
replace casesr=floor(temp) if index<0
drop temp

nl (logno = {b0=10} + {b2=-6}*exp({b3=-0.2}*index)) if index<=30, iterate(20)
predict temp, yhat
replace temp=exp(temp)
replace caseso=floor(temp) if index<0
drop temp

nl (logdr = {b0=10} + {b2=-6}*exp({b3=-0.2}*index)) if index<=30, iterate(20)
predict temp, yhat
replace temp=exp(temp)
replace deathsr=floor(temp) if index<0
drop temp

nl (logdo = {b0=10} + {b2=-6}*exp({b3=-0.2}*index)) if index<=30, iterate(20)
predict temp, yhat
replace temp=exp(temp)
replace deathso=floor(temp) if index<0
drop temp

tsset t
drop if index>175
gen dcaseso=s.caseso 
gen ddeathso=s.deathso 
gen dcasesr=s.casesr
gen ddeathsr=s.deathsr

preserve
*drop if index>82
drop if index>49
keep ddeathsr dcasesr
order ddeathsr dcasesr
drop if ddeathsr == .
format %20.0e ddeathsr
format %20.0e dcasesr
export delimited using "dataR.csv", delimiter(" ") datafmt novarnames replace
restore

preserve
*drop if index>82
drop if index>49
keep ddeathso dcaseso
order ddeathso dcaseso
drop if ddeathso == .
format %20.0e ddeathso
format %20.0e dcaseso
export delimited using "dataO.csv", delimiter(" ") datafmt novarnames replace
restore

format t %tdCY-N-D
export delimited using "data_raw.csv", replace
