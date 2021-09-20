clear all
global HOME "/Users/tgst/Dropbox/PhD_Economics/research/my_projects/3rd_year_project_3rd_year_project/007_CovidInfo/To_github/EqModel/data"
cd $HOME


*use set1.dta
*use set11.dta
*use set12.dta
use set2.dta

*reg f.grcases1                               l.logd l.logdr if t>60 & t<200
*reg f.grcases1 l.grcases1                    l.logd l.logdr if t>50 & t<200
*reg f.grcases1 l.grcases1                    l.logd l.logdr if t>30 & t<200
*reg f.grcases1 l.s.logd l.s.logdr l.grcases1 l.logd l.logdr if t>95 & t<200
*reg f.grcases1 l.s.logd l.grcases1           l.logd l.logdr if t>30 & t<250

reg f.grcases1 t t2-t10 l.s.logd l.grcases1 l.logd l.logdr if t>30 & t<240
reg f.grcases1 t t2 t3  l.s.logd l.grcases1 l.logd l.logdr if t>30 & t<120
