/*
	this do-file analyzes the effects of the treatment in the survey 
			
*/

clear
set more off
set scheme s1color
grstyle clear
grstyle init
*grstyle anglestyle vertical_tick horizontal
grstyle color p1 black
grstyle color p2 black

/*
global covid "/Users/adrian/Dropbox/COVID_reporting_delay/"
global covidout "/Users/adrian/Dropbox/COVID_reporting_delay/text/figs_tables_behavior_delays/"
*/

global covid "C:\Users\emili\Dropbox\COVID reporting delay\"

global covidout "C:\Users\emili\Dropbox\results_may_2021\"



use "${covid}survey_results/survey_for_analysis.dta", clear


/* ++++++++++++++++++++++++++++++++++++++++++
			0. Data prep
++++++++++++++++++++++++++++++++++++++++++ */

*keep relevant observations 
keep if treat!=.
label variable treat "Information by date occurred"

*controls
global controls "female aged18_22 aged23_29 aged30_49 aged50_plus works school workschool other_occ cdmx apartment noyard withyard hhm_onetwo hhm_two hhm_three hhm_fourp hhm_over70 hhm_60_70 hhm_50_60 sick_nothing sick_self"

*get date of survey
split timestamp
gen date_survey=date(timestamp1,"YMD")
format date_survey %tdMon/DD
drop timestamp1 timestamp2
order id timestamp date_survey

*histogram with dates of survey 
histogram date_survey, discret color(blue) fcolor(none) xti(" ") xscale(r(22063 22074)) xlab(22063(1)22074, angle(30))
graph export "${covidout}hist_survey_date.pdf", replace

*define low prior group based on cases 
gen low_prior=preg28<=2

*define complement and full sample
gen high_prior=1-low_prior
gen full_samp=1

*definne low prior group bases on deaths 
gen low_prior_deaths=preg29<=2

*construct first outcome variable: are we doing worse than sweden

gen muchfaster=preg32==0
label variable muchfaster "The Epidemic is evolving much faster than in Sweden"

gen faster=preg32<=1
label variable faster "The Epidemic is evolving faster than in Sweden"


*construct indicators of perceived toll
gen totcas=50000 if preg36==0
replace totcas=125000 if preg36==1
replace totcas=200000 if preg36==2
replace totcas=375000 if preg36==3
replace totcas=750000 if preg36==4
replace totcas=1500000 if preg36==5
replace totcas=3000000 if preg36==6
gen logtotalcas=log(totcas)

label variable logtotalcas "Log of predicted total cases"

gen totdeaths=5000 if preg39==0
replace totdeaths=12500 if preg39==1
replace totdeaths=20000 if preg39==2
replace totdeaths=37500 if preg39==3
replace totdeaths=75000 if preg39==4
replace totdeaths=150000 if preg39==5
replace totdeaths=300000 if preg39==6
gen logtotaldeaths=log(totdeaths)

label variable logtotaldeaths "Log of predicted total deaths"


*construct number of days leaving home

*continuous
gen leaveshome=0 if preg45==0
replace leaveshome=1 if preg45==1
replace leaveshome=2 if preg45==2
replace leaveshome=3.5 if preg45==3
replace leaveshome=5 if preg45==4

label variable leaveshome "Number of days leaving home in four weeks"
*dummy

gen dleaves4=leaveshome>=3
label variable dleaves4 "Will leave house three or more times in four weeks"



*get indicators for times left the house last week 
tab preg31, gen(out_last_wk_)



*indicators for each category of the outcome variables 
tab preg32, gen(pc32)
tab preg36b, gen(pc36)
tab preg39b, gen(pc39)
tab preg45, gen(pcc45)

*fix some labels 
label variable other_occ "Other occupation/employment status"
label variable hhm_over70 "Has HH members over 70 years old"
label variable hhm_60_70 "Has HH members 60-70 years old"
label variable hhm_50_60 "Has HH members 50-60 years old"
label variable sick_nothing "Does not seek healthcare when sick"


/* ++++++++++++++++++++++++++++++++++++++++++
			1. Balance tables
++++++++++++++++++++++++++++++++++++++++++ */

balancetable treat $controls using ${covidout}balance_full.tex, varlabels ///
	ctitles("Deaths by date reported" "Deaths by date occurred" "Difference in means") replace

balancetable treat $controls if low_prior==1 using ${covidout}balance_low.tex, varlabels ///
	ctitles("Deaths by date reported" "Deaths by date occurred" "Difference in means") replace

balancetable treat $controls if low_prior==0 using ${covidout}balance_high.tex, varlabels ///
	ctitles("Deaths by date reported" "Deaths by date occurred" "Difference in means") replace

	
	
/* ++++++++++++++++++++++++++++++++++++++++++
			2. Histograms 
++++++++++++++++++++++++++++++++++++++++++ */

***		questions on priors	***

histogram preg28, percent addl  lc(gs4) fc(black) fi(20)  w(1) barw(0.8)  ///
	start(-0.5) yscale(r(0 50)) ylab(0(10)50) addlabop(yvarf(%9.1f)) xscale(r(-0.5 6.5)) ///
	xla(0 "<10K" 1 `""10K-" "25K""' 2 `""25K-" "50K""' 3 `""50K-" "75K""' 4 `""75K-" "100K""' 5 `""100K-" "150K""' 6 ">150K") ///
	xtitle("Prior of total COVID-19 cases as of May 20", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}prior_cases_may20.pdf, replace

histogram preg29, percent addl  lc(gs4) fc(black) fi(20)  w(1) barw(0.8)  ///
	start(-0.5) yscale(r(0 50)) ylab(0(10)50) addlabop(yvarf(%9.1f)) xscale(r(-0.5 6.5)) ///
	xla(0 "<1K" 1 `""1K-" "2.5K""' 2 `""2.5K-" "5K""' 3 `""5K-" "7.5K""' 4 `""7.5K-" "10K""' 5 `""10K-" "15K""' 6 ">15K") ///
	xtitle("Prior of total COVID-19 deaths as of May 20", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}prior_deaths_may20.pdf, replace


***		outcomes all participants	***

histogram preg32, percent addl addlabop(yvarf(%9.1f)) lc(gs4) fc(black) fi(20) w(1) barw(0.8)  ///
	start(-0.5)  yscale(r(0 50)) ylab(0(10)50) xscale(r(-0.5 4)) ///
	xla(0 `""Much" "faster""' 1 `"Faster"' 2 "Similar" 3 "Slower" 4 `""Much" "Slower""') ///
	xtitle("Epidemic's evolution compared to Sweden", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}sweden.pdf, replace


histogram preg36b, percent addl addlabop(yvarf(%9.1f)) lc(gs4) fc(black) fi(20) w(1) barw(0.8) start(0.5)  ///
	yscale(r(0 50)) ylab(0(10)50) xscale(r(0.5 7)) ///
	xla(1 2 `""150K-" "250K""' 3 `""250K-" "500K""' 4 5 6, valuelabel) ///
	xtitle("Beliefs about total COVID-19 cases", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}beliefs_cases.pdf, replace
	
histogram preg39b, percent addl addlabop(yvarf(%9.1f)) lc(gs4) fc(black) fi(20) w(1) barw(0.8) start(0.5)  ///
	yscale(r(0 50)) ylab(0(10)50) xscale(r(0.5 7)) ///
	xla(1/3 4 `""50K-" "100K""' 5 `""100K-" "200K""' 6, valuelabel) ///
	xtitle("Beliefs about total COVID-19 deaths", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}beliefs_deaths.pdf, replace

  
histogram preg45, percent addl  lc(gs4) fc(black) fi(20)  w(1) barw(0.8)  start(-0.5)  ///
	yscale(r(0 50)) ylab(0(10)50) xscale(r(-0.5 5)) ///
	xla(0 "None" 1 "One" 2 "Two" 3 `""Three" "or four""' 4 `""More than" "four""') ///
	xtitle("Number of times will leave home in 4 weeks", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}daysout_4w.pdf, replace




***		outcomes by informational treatments	***

twoway (histogram preg32 if treat==0, percent  lc(gs4) fc(black) fi(20) w(1) barw(0.8)  ///
	start(-0.5)) (histogram preg32 if treat==1, percent  lc(blue) fc(none) fi(20) w(1) ///
	barw(0.8) lw(medthick) start(-0.5)), legend(order(1 "Info by date reported" 2 "Info by date occurred") ///
	ring(0) position(11) bmargin(small) region(lwidth(none) fc(none)) col(1)) ///
	yscale(r(0 60)) ylab(0(10)60) xscale(r(-0.5 4)) ///
	xla(0 `""Much" "faster""' 1 `"Faster"' 2 "Similar" 3 "Slower" 4 `""Much" "Slower""') ///
	xtitle("Epidemic's evolution compared to Sweden", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}hist_sweden_treat.pdf, replace

twoway (histogram preg36b if treat==0, percent  lc(gs4) fc(black) fi(20) w(1) barw(0.8)  ///
	start(0.5)) (histogram preg36b if treat==1, percent  lc(blue) fc(none) fi(20) w(1) ///
	barw(0.8) lw(medthick) start(0.5)), legend(order(1 "Info by date reported" 2 "Info by date occurred") ///
	ring(0) position(11) bmargin(small) region(lwidth(none) fc(none)) col(1)) ///
	yscale(r(0 60)) ylab(0(10)60) xscale(r(0.5 7)) ///
	xla(1 2 `""150K-" "250K""' 3 `""250K-" "500K""' 4 5 6, valuelabel) /// 
	xtitle("Beliefs about total COVID-19 cases", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}hist_cases_treat.pdf, replace

twoway (histogram preg39b if treat==0, percent  lc(gs4) fc(black) fi(20) w(1) barw(0.8)  ///
	start(0.5)) (histogram preg39b if treat==1, percent  lc(blue) fc(none) fi(20) w(1) ///
	barw(0.8) lw(medthick) start(0.5)), legend(order(1 "Info by date reported" 2 "Info by date occurred") ///
	ring(0) position(11) bmargin(small) region(lwidth(none) fc(none)) col(1)) ///
	yscale(r(0 60)) ylab(0(10)60) xscale(r(0.5 7)) ///
	xla(1/3 4 `""50K-" "100K""' 5 `""100K-" "200K""' 6, valuelabel) ///
	xtitle("Beliefs about total COVID-19 deaths", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}hist_deaths_treat.pdf, replace


twoway (histogram preg45 if treat==0, percent  lc(gs4) fc(black) fi(20) w(1) barw(0.8)  ///
	start(-0.5)) (histogram preg45 if treat==1, percent  lc(blue) fc(none) fi(20) w(1) ///
	barw(0.8) lw(medthick) start(-0.5)), legend(order(1 "Info by date reported" 2 "Info by date occurred") ///
	ring(0) position(11) bmargin(small) region(lwidth(none) fc(none)) col(1)) ///
	yscale(r(0 60)) ylab(0(10)60) xscale(r(-0.5 5)) ///
	xla(0 "None" 1 "One" 2 "Two" 3 `""Three" "or four""' 4 `""More than" "four""') ///
	xtitle("Number of times will leave home in 4 weeks", height(5)) ytitle("Percentage", height(5))
graph export ${covidout}hist_out_4w_treat.pdf, replace
*/

/* ++++++++++++++++++++++++++++++++++++++++++
			3. Main regressions
++++++++++++++++++++++++++++++++++++++++++ */

*program
cap prog drop mainregs
prog def mainregs

	syntax, outcome(namelist) controls(string) sample(namelist) saving(string) ap(string)
	
	reg `outcome' treat `controls' if `sample'==1, robust
	
	qui sum `outcome' if e(sample)==1, det
	local mdv=r(mean)

	outreg2 treat using "${covidout}\`saving'", keep(treat) nocons tex sdec(3) adec(2) ///
		addstat(Mean dependent variable, `mdv') addtext("Sample", `sample') label nonotes `ap'
	
end

*run program 
mainregs, outcome(faster) controls($controls) sample(full_samp) saving(regs_sweden) ap(replace)
mainregs, outcome(faster) controls($controls) sample(low_prior) saving(regs_sweden) ap(append)
mainregs, outcome(faster) controls($controls) sample(high_prior) saving(regs_sweden) ap(append)
mainregs, outcome(muchfaster) controls($controls) sample(full_samp) saving(regs_sweden) ap(append)
mainregs, outcome(muchfaster) controls($controls) sample(low_prior) saving(regs_sweden) ap(append)
mainregs, outcome(muchfaster) controls($controls) sample(high_prior) saving(regs_sweden) ap(append)

mainregs, outcome(logtotalcas) controls($controls) sample(full_samp) saving(regs_toll) ap(replace)
mainregs, outcome(logtotalcas) controls($controls) sample(low_prior) saving(regs_toll) ap(append)
mainregs, outcome(logtotalcas) controls($controls) sample(high_prior) saving(regs_toll) ap(append)
mainregs, outcome(logtotaldeaths) controls($controls) sample(full_samp) saving(regs_toll) ap(append)
mainregs, outcome(logtotaldeaths) controls($controls) sample(low_prior) saving(regs_toll) ap(append)
mainregs, outcome(logtotaldeaths) controls($controls) sample(high_prior) saving(regs_toll) ap(append)

mainregs, outcome(leaveshome) controls($controls) sample(full_samp) saving(regs_behavior) ap(replace)
mainregs, outcome(leaveshome) controls($controls) sample(low_prior) saving(regs_behavior) ap(append)
mainregs, outcome(leaveshome) controls($controls) sample(high_prior) saving(regs_behavior) ap(append)
mainregs, outcome(dleaves4) controls($controls) sample(full_samp) saving(regs_behavior) ap(append)
mainregs, outcome(dleaves4) controls($controls) sample(low_prior) saving(regs_behavior) ap(append)
mainregs, outcome(dleaves4) controls($controls) sample(high_prior) saving(regs_behavior) ap(append)



/* ++++++++++++++++++++++++++++++++++++++++++
			4. Regressions by category 
++++++++++++++++++++++++++++++++++++++++++ */

*program 
cap prog drop regscat
prog def regscat 

	syntax, sample(namelist) color(string) marker(string) ymin(numlist) ymax(numlist) saving(string)

	
	
	forval i=1(1)5 {
		reg pc32`i' treat $controls, robust
		preserve
			parmest, fast
			keep if parm=="treat"
			keep parm estimate stderr min max
			gen preg32=`i'
			save ${covidout}\cat_`i'.dta, replace
		restore
	}

	preserve
		clear
		forval i=1(1)5 {
			append using ${covidout}\cat_`i'.dta
		}	
		gen min90= estimate-1.64*stderr
		gen max90= estimate+1.64*stderr
		twoway (scatter estimate preg32 , mc(`color') m(`marker')) (rcap max90 min90 preg32, lc(`color')) (rspike max95 min95 preg32, lc(`color')), ///
			legend(off) yline(0,lc(gs10) lp(dash)) yscale(r(`ymin' `ymax') )  xscale(r(0.5 5.5))  ///
			xla(1 `""Much" "Faster""' 2 "Faster" 3 "Similar" 4 "Slower" 5 `""Much" "slower""') ///
			ytitle("Diff. between info by date occurred vs reported", height(5)) ///
			xtitle("Speed of the epidemic's evolution compared to Sweden", height(5))
		graph export ${covidout}diff_sweden_treat_`saving'.pdf, replace
	restore
	forval i=1(1)4 {
		rm ${covidout}\cat_`i'.dta



	* 2. total cases 
	
	forval i=1(1)6 {
		reg pc36`i' treat $controls if `sample'==1, robust
		preserve
			parmest, fast
			keep if parm=="treat"
			keep parm estimate stderr min max
			gen preg36=`i'
			save ${covidout}\cat_`i'.dta, replace
		restore
	}

	preserve
		clear
		forval i=1(1)6 {
			append using ${covidout}\cat_`i'.dta
		}
		gen min90= estimate-1.64*stderr
		gen max90= estimate+1.64*stderr
		twoway (scatter estimate preg36 , mc(`color') m(`marker')) (rcap max90 min90 preg36, lc(`color')) (rspike max95 min95 preg36, lc(`color')), ///
			legend(off) yline(0,lc(gs10) lp(dash)) yscale(r(`ymin' `ymax')) ylab(`ymin'(0.05)`ymax') xscale(r(0.5 6.5)) ///
			xlab(1 "<150K" 2 `""150K-" "250K""' 3 `""250K-" "500K""' 4 "500K-1M" 5 "1M-2M" 6 ">2M") ///
			ytitle("Diff. between info by date occurred vs reported", height(5)) ///
			xtitle("Beliefs about total number of COVID-19 cases", height(5))
		graph export ${covidout}diff_cases_treat_`saving'.pdf, replace
	restore

	forval i=1(1)6 {
		rm ${covidout}\cat_`i'.dta
	}
	
	* 3. total deaths
	
	forval i=1(1)6 {
		reg pc39`i' treat $controls if `sample'==1, robust
		preserve
			parmest, fast
			keep if parm=="treat"
			keep parm estimate stderr min max
			gen preg39=`i'
			save ${covidout}\cat_`i'.dta, replace
		restore
	}

	preserve
		clear
		forval i=1(1)6 {
			append using ${covidout}\cat_`i'.dta
		}
		gen min90= estimate-1.64*stderr
		gen max90= estimate+1.64*stderr
		twoway (scatter estimate preg39 , mc(`color') m(`marker')) (rcap max90 min90 preg39, lc(`color')) (rspike max95 min95 preg39, lc(`color')), ///
			legend(off) yline(0,lc(gs10) lp(dash)) yscale(r(`ymin' `ymax')) ylab(`ymin'(0.05)`ymax') xscale(r(0.5 6.5)) ///
			xlab(1 "<15K" 2 "15K-25K" 3 "25K-50K" 4 `""50K-" "100K""' 5 `""100K-" "200K""' 6 ">200K") ///
			ytitle("Diff. between info by date occurred vs reported", height(5)) ///
			xtitle("Beliefs about total number of COVID-19 deaths", height(5))
		graph export ${covidout}diff_deaths_treat_`saving'.pdf, replace
	restore

	forval i=1(1)6 {
		rm ${covidout}\cat_`i'.dta
	}
	

	* 6. behavior in 4 weeks 
	
	forval i=1(1)5 {
		reg pcc45`i' treat $controls if `sample'==1, robust
		preserve
			parmest, fast
			keep if parm=="treat"
			keep parm estimate stderr min max
			gen preg45=`i'
			save ${covidout}\cat_`i'.dta, replace
		restore
	}

	preserve
		clear
		forval i=1(1)5 {
			append using ${covidout}\cat_`i'.dta
		}	
		gen min90= estimate-1.64*stderr
		gen max90= estimate+1.64*stderr	
		twoway (scatter estimate preg45 , mc(`color') m(`marker')) (rcap max90 min90 preg45, lc(`color')) (rspike max95 min95 preg45, lc(`color')), ///
			legend(off) yline(0,lc(gs10) lp(dash)) yscale(r(`ymin' `ymax')) ylab(`ymin'(0.05)`ymax') xscale(r(0.5 5.5)) ///
			xlab(1 "None" 2 "One" 3 "Two" 4 `""Three" "or four""' 5 `""More than" "four""') ///
			ytitle("Diff. between info by date occurred vs reported", height(5)) ///
			xtitle("Number of times will leave home in 4 weeks", height(5))
		graph export ${covidout}diff_daysout4_treat_`saving'.pdf, replace
	restore

	forval i=1(1)5 {
		rm ${covidout}\cat_`i'.dta
	}
	
end 

*run program
regscat,  sample(full_samp) color(blue) marker(O) ymin(-0.15) ymax(0.1) saving(full)
regscat,  sample(low_prior) color(green) marker(Dh) ymin(-0.2) ymax(0.2) saving(low)
regscat,  sample(high_prior) color(purple) marker(Sh) ymin(-0.2) ymax(0.2) saving(high)


grstyle clear
