clear all
global HOME "/Users/tgst/Dropbox/PhD_Economics/research/my_projects/3rd_year_project_3rd_year_project/007_CovidInfo/To_github/EqModel/StandardEpiModelsGov"
cd $HOME

import delimited "get_data/Data_Reported_all_both.csv", encoding(ISO-8859-1)

generate t = date(date, "YMD")
format t %td
gen i=_n + mdy(4,12,2020)- mdy(3,19,2020)

gen ddeathso = deathso-deathso[_n-1]
egen aux=max(ddeathso) if i < 200

gen aaux = 1 if aux == ddeathso
sum i if aaux==1 &        i < 200
sum ddeathso if aaux==1 & i < 200
sum deathso if i==120
sum deathso if i==200
sum deathso if i==300
