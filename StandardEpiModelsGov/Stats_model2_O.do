clear all
global HOME "/Users/tgst/Dropbox/PhD_Economics/research/my_projects/3rd_year_project_3rd_year_project/007_CovidInfo/To_github/EqModel/StandardEpiModelsGov"
cd $HOME

import delimited "model2_SEIRD/export_predictions_occurred_model2.csv", encoding(ISO-8859-1)


drop t
generate t = date(date_text2, "YMD")
format t %td


gen ddrp = drp-drp[_n-1]

egen aux=max(ddrp)
gen aaux = 1 if aux == ddrp
sum i if aaux==1
sum ddrp if aaux==1
sum drp if i==120
sum drp if i==200
sum drp if i==300
