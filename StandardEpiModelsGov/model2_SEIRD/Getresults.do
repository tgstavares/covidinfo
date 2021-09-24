
clear all
global HOME "/Users/tgst/Dropbox/PhD_Economics/research/my_projects/3rd_year_project_3rd_year_project/007_CovidInfo/To_github/EqModel/StandardEpiModelsGov/model2_SEIRD"
cd $HOME

clear
import delimited "Results/reported/9-01_D_long.csv", encoding(ISO-8859-1)
gen t = mdy(mes , dia , aão)
format t %tdCY-N-D
generate date_text2 = string(t, "%tdCY-N-D")
gen i=_n
replace i = i + mdy(4,1,2020)- mdy(3,19,2020)
order t date_text2 q_50 datum 
keep i t date_text2 q_50 datum 
replace datum=. if datum==-1

ren q_50 drp
ren datum drdata
export delimited using "export_predictions_reported_model2.csv", replace

clear
import delimited "Results/occurred/9-01_D_long.csv", encoding(ISO-8859-1)
gen t = mdy(mes , dia , aão)
format t %tdCY-N-D
generate date_text2 = string(t, "%tdCY-N-D")
gen i=_n
replace i = i + mdy(4,1,2020)- mdy(3,10,2020)
order t date_text2 q_50 datum 
keep i t date_text2 q_50 datum 
replace datum=. if datum==-1

ren q_50 drp
ren datum drdata
export delimited using "export_predictions_occurred_model2.csv", replace

