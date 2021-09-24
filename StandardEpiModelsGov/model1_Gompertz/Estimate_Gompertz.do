/*
INFO ON THE MODEL
https://salud.conacyt.mx/coronavirus/investigacion/proyectos/gompertz.html
*/

clear all
global HOME "/Users/tgst/Dropbox/PhD_Economics/research/my_projects/3rd_year_project_3rd_year_project/007_CovidInfo/To_github/EqModel/StandardEpiModelsGov/model1_Gompertz"
cd $HOME

import delimited "../get_data/Data_Reported_all_both.csv", encoding(ISO-8859-1)
generate t = date(date, "YMD")
format t %td
order index date t
gen year=year(t)
gen month=month(t)
*drop if index>82
drop if index>49


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

local a=_N+1
set obs `a'
replace index = 600 in `a'
sort index 
tsset index
tsfill, full
replace t=t[_n-1]+1 if t[_n]==.
sort index 


* Get regression coefficients of Gompertz
gen logdr = log(deathsr)
gen logdo = log(deathso)

nl (logdr = {b0=10} + {b2=-6}*exp({b3=-0.1}*index)) , iterate(20)
predict yr, yhat
gen drp = exp(yr)
nl (logdo = {b0=10} + {b2=-4}*exp({b3=-0.3}*index)) , iterate(20)
predict yo, yhat
gen dop = exp(yo)


gen lognr = log(casesr)
gen logno = log(caseso)

nl (lognr = {b0=10} + {b2=-6}*exp({b3=-0.1}*index)) , iterate(20)
predict yr2, yhat
gen nrp = exp(yr2)
nl (logno = {b0=10} + {b2=-4}*exp({b3=-0.3}*index)) , iterate(20)
predict yo2, yhat
gen nop = exp(yo2)

keep index-deathsr drp dop nrp nop

format t %tdCY-N-D
generate date_text2 = string(t, "%tdCY-N-D")
order index t date date_text2 

export delimited using "export_predictions_model1.csv", replace
