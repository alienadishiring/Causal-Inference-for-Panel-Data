
**************************************************************************************************************************************************************************************************** Replicate Exercise 1 with a different RQ: what is the effect of having a University Degree on employment status?
**************************************************************************************************************************************************************************************************

*Set the directory

cd "c:/Users/`c(username)'/YOUR DIRECTORY"
*`' This symbol is under the Esc button

*Open the data
use "longitudinal_td.dta", clear

*Variables overview
desc

br
keep if wave < 4

*Declare panel structure
xtset pidp wave
*The panel is unbalanced 

drop indinus_lw_4-indscus_lw_9

keep if age_dv > 15 & age_dv < 66

sum age_dv

numlabel, add
tab hiqual_dv

gen uni_degree = 1
replace uni_degree = 0 if hiqual_dv > 1
replace uni_degree = . if hiqual_dv == .

tab jbstat

*I create a simple dummy taking the value 1 if the individual is either employed or self employed and 0 otherwise
gen employed = 0
replace employed = 1 if jbstat < 3
replace employed = . if jbstat == .

tab jbstat
tab employed

*Let's do some descriptive statistics 
bysort uni_degree: tab employed [aweight = indinus_lw_3]
proportion employed [pweight = indinus_lw_3], over(uni_degree)

*Let's do some correlations
corr employed uni_degree
*Here I want to know if the correlation is significant
pwcorr employed uni_degree, sig

corr employed uni_degree [aweight = indinus_lw_3] 
pwcorr employed uni_degree [aweight = indinus_lw_3], sig

ttest employed, by(uni_degree)

reg employed uni_degree
reg employed uni_degree [pweight = indinus_lw_3]


label define in_employment 1 "In employment" 0 "Not in employment" 
label values employed in_employment

label define uni_degree 1 "University Degree" 0 "Lower qualification" 
label values uni_degree uni_degree



set scheme cblind1

cibar employed, over(uni_degree) /// syntax:  cibar Y, over(X1) blfmt(%4.2f) blsize(small) blposition(swest) blcolor(white) ///
level(95) /// specifies the CI level to be 95% (this is the default)
bargap(10) /// places a big gap between the bars (otherwise there will be no gap)
graphopts( /// start of graphopts option
ytitle(Probabilty of being employed) /// titles y-axis
ylab(0(1)1, glpattern(solid)) /// y-axis label options and horizontal gridline options
xsize(6.5) ysize(4.5) graphregion(margin(vsmall)) /// graph dimensions and margin b/w plot and edge 
legend(pos(6) ring(1) col(1) size(small)) /// legend options
) // Note this last parenthesis! closes graphopts


cibar employed [pw = indinus_lw_3], over(uni_degree) /// syntax:  cibar Y, over(X1) blfmt(%4.2f) blsize(small) blposition(swest) blcolor(white) ///
level(95) /// specifies the CI level to be 95% (this is the default)
bargap(10) /// places a big gap between the bars (otherwise there will be no gap)
graphopts( /// start of graphopts option
ytitle(Probabilty of being employed) /// titles y-axis
ylab(0(1)1, glpattern(solid)) /// y-axis label options and horizontal gridline options
xsize(6.5) ysize(4.5) graphregion(margin(vsmall)) /// graph dimensions and margin b/w plot and edge 
legend(pos(6) ring(1) col(1) size(small)) /// legend options
) // Note this last parenthesis! closes graphopts



//Let's do some regressions using individual controls

tab sex_dv
sum age_dv
tab hiqual_dv
tab bornuk_dv
tab nchild_dv
tab ethn_dv
tab mstat_dv

*Simple OLS, playing with the SE
reg employed uni_degree, vce(robust)
reg employed uni_degree, vce(cluster pidp)

*Panel data regressions
xtreg employed uni_degree, vce(robust)
xtreg employed uni_degree, vce(cluster pidp)
*Let's include individual FEs
xtreg employed uni_degree, fe vce(cluster pidp)
*TWFE
xtreg employed uni_degree i.wave, fe vce(cluster pidp)

*I add some controls
reg employed uni_degree sex_dv age_dv i.mstat_dv bornuk_dv nchild_dv i.ethn_dv, vce(robust)
reg employed uni_degree sex_dv age_dv i.mstat_dv bornuk_dv nchild_dv i.ethn_dv, vce(cluster pidp)

*And use xtreg
xtreg employed uni_degree sex_dv age_dv i.mstat_dv bornuk_dv nchild_dv i.ethn_dv, fe vce(robust)
xtreg employed uni_degree sex_dv age_dv i.mstat_dv bornuk_dv nchild_dv i.ethn_dv, fe vce(cluster pidp)
xtreg employed uni_degree sex_dv age_dv i.mstat_dv bornuk_dv nchild_dv i.ethn_dv i.wave, fe vce(cluster pidp)

xtreg employed uni_degree sex_dv age_dv mstat_dv bornuk_dv nchild_dv ethn_dv, i.pidp
*Should give you the same, but it would take a lifetime to run


*And now I weight the regressions
xtreg employed uni_degree sex_dv age_dv i.mstat_dv bornuk_dv nchild_dv i.ethn_dv [pweight = indinus_lw_3], fe vce(robust)
xtreg employed uni_degree sex_dv age_dv i.mstat_dv bornuk_dv nchild_dv i.ethn_dv [pweight = indinus_lw_3], fe vce(cluster pidp)
xtreg employed uni_degree sex_dv age_dv i.mstat_dv bornuk_dv nchild_dv i.ethn_dv [pweight = indinus_lw_3] i.wave, fe vce(cluster pidp)


* I install the command that allows me to include high-dimensional FEs
ssc install reghdfe

reghdfe employed uni_degree age_dv i.mstat_dv nchild_dv [pweight = indinus_lw_3], absorb(pidp) vce(cluster pidp)
reghdfe employed uni_degree age_dv i.mstat_dv nchild_dv [pweight = indinus_lw_3], absorb(pidp) vce(robust)
reghdfe employed uni_degree age_dv i.mstat_dv nchild_dv [pweight = indinus_lw_3], absorb(pidp wave gor) vce(robust)

reghdfe employed uni_degree age_dv i.mstat_dv nchild_dv [pweight = indinus_lw_3], absorb(pidp) vce(robust) keepsingleton
reghdfe employed uni_degree age_dv i.mstat_dv nchild_dv [pweight = indinus_lw_3], absorb(pidp gor_dv) vce(robust)

*Heterogeneity by sex
reghdfe employed uni_degree age_dv i.mstat_dv nchild_dv [pweight = indinus_lw_3] if sex_dv == 1, absorb(pidp gor_dv) vce(robust)
reghdfe employed uni_degree age_dv i.mstat_dv nchild_dv [pweight = indinus_lw_3] if sex_dv == 2, absorb(pidp gor_dv) vce(robust)
