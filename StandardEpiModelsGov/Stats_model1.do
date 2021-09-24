clear all
global HOME "/Users/tgst/Dropbox/PhD_Economics/research/my_projects/3rd_year_project_3rd_year_project/007_CovidInfo/To_github/EqModel/StandardEpiModelsGov"
cd $HOME

import delimited "model1_Gompertz/export_predictions_model1.csv", encoding(ISO-8859-1)
drop t
generate t = date(date_text2, "YMD")
format t %td
gen ddrp = drp-drp[_n-1]
gen ddop = dop-dop[_n-1]

preserve
drop if nrp<200
gen i=_n
egen aux=max(ddrp)
gen aaux = 1 if aux == ddrp
sum i if aaux==1
sum ddrp if aaux==1
sum drp if i==120
sum drp if i==200
sum drp if i==300
restore

preserve
drop if nop<140
gen i=_n
egen aux=max(ddop)
gen aaux = 1 if aux == ddop
sum i if aaux==1
sum ddop if aaux==1
sum dop if i==120
sum dop if i==200
sum dop if i==300
restore

