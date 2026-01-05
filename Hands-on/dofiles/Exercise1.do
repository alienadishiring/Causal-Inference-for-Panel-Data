
// It is always good pratcice to do any kind of analysis using a do file
// It will help remembering what you did exactly, even if you are only doing a preliminary analysis


*Set the directory
*Directory = place where my data are stored
*Go to your data, right click > Properties > General > Location
*You can either copy and paste the location and write the command as below

cd "C:\Users\C:\Users\vgdi1n20\Dropbox\Causal inference for Panel Data\Data\UKDA-8715-stata\stata\stata13"

*Or you can do it in this different way
*Quite useful if you are sharing the folder with your co-authors/collaborators!

cd "c:/Users/`c(username)'/Dropbox/Causal inference for Panel Data/Data/UKDA-8715-stata/stata/stata13"
*`' This symbol is under the Esc button

*Open the data
use "longitudinal_td.dta", clear

*Variables overview
desc

*Declare panel structure
xtset pidp wave
*This command not only declares the panel structure in Stata, but also helps us understanding our panel structure

br
*This command will let us browse our variables and the dataset. Our data are in a long format... perfect!
*It's the format I want to analyse panel dataset
*If the sata are in a wide format we can use the command reshape

*To keep it simple, we only keep the first 3 waves
keep if wave < 4

xtset pidp wave
*The panel will still be unbalanced anyways

**IMPORTANT!!! We need to use weights in our analysis. We don't need weights for waves > 3

drop indinus_lw_4-indscus_lw_9

*I want to investigate the effect of being married on the probability of being employed
*First, I will restrict the sample to the working age individuals (16 to 64)

keep if age_dv > 15 & age_dv < 66

*Sum is handy for continuous variables
sum age_dv

*While tab for categorical variables
tab mstat_dv

*I create a dummy that takes the value 1 if the individuals is married and 0 otherwise
*But... wait! How do I do this if I don't have any idea of how this variable has been encoded?

numlabel, add
tab mstat_dv 

*Much better now!

gen married = 0
replace married = 1 if mstat_dv == 2

*Am I done?
*Let's compare the number of observations we have with the two variables

tab mstat_dv
tab married

*Though I have the same number of people with a University Degree in both variables, I have a overall higher number of observations with the generated variables

replace married = . if mstat_dv == .
tab married

*Much better now!

tab jbstat

*I create a simple dummy taking the value 1 if the individual is either employed or self employed and 0 otherwise
gen employed = 0
replace employed = 1 if jbstat < 3
replace employed = . if jbstat == .

tab jbstat
tab employed

*Let's do some descriptive statistics 
*I tab employed by marital status
bysort married: tab employed
*Wait, I need to weight the data!
bysort married: tab employed [aweight = indinus_lw_3]
*This is not ideal. The weights used in US are probability weights
proportion employed [pweight = indinus_lw_3], over(married)
*This gives me the same results, so I guess that using aweight is fine!

*Let's do some correlations
corr employed married
*Here I want to know if the correlation is significant
pwcorr employed married, sig

corr employed married [aweight = indinus_lw_3] 
pwcorr employed married [aweight = indinus_lw_3], sig

ttest employed, by(married)

reg employed married
reg employed married [pweight = indinus_lw_3]


label define in_employment 1 "In employment" 0 "Not in employment" 
label values employed in_employment

label define married 1 "Married" 0 "Not married" 
label values married married



set scheme cblind1

cibar employed, over(married) /// syntax:  cibar Y, over(X1) blfmt(%4.2f) blsize(small) blposition(swest) blcolor(white) ///
level(95) /// specifies the CI level to be 95% (this is the default)
bargap(10) /// places a big gap between the bars (otherwise there will be no gap)
graphopts( /// start of graphopts option
ytitle(Probabilty of being employed) /// titles y-axis
ylab(0(1)1, glpattern(solid)) /// y-axis label options and horizontal gridline options
xsize(6.5) ysize(4.5) graphregion(margin(vsmall)) /// graph dimensions and margin b/w plot and edge 
legend(pos(6) ring(1) col(1) size(small)) /// legend options
) // Note this last parenthesis! closes graphopts


// With weights is normally the correct one, even if sometimes things do not change much!!

cibar employed [pw = indinus_lw_3], over(married) /// syntax:  cibar Y, over(X1) blfmt(%4.2f) blsize(small) blposition(swest) blcolor(white) ///
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


*Simple OLS, playing with the SE
reg employed married, vce(robust)
reg employed married, vce(cluster pidp)

*Panel data regressions
xtreg employed married, vce(robust)
xtreg employed married, vce(cluster pidp)
*Let's include individual FEs
xtreg employed married, fe vce(cluster pidp)
*TWFE
xtreg employed married i.wave, fe vce(cluster pidp)

*I add some controls
reg employed married sex_dv age_dv i.hiqual_dv bornuk_dv nchild_dv i.ethn_dv, vce(robust)
reg employed married sex_dv age_dv i.hiqual_dv bornuk_dv nchild_dv i.ethn_dv, vce(cluster pidp)

*And use xtreg
xtreg employed married sex_dv age_dv i.hiqual_dv bornuk_dv nchild_dv i.ethn_dv, fe vce(robust)
xtreg employed married sex_dv age_dv i.hiqual_dv bornuk_dv nchild_dv i.ethn_dv, fe vce(cluster pidp)
xtreg employed married sex_dv age_dv i.hiqual_dv bornuk_dv nchild_dv i.ethn_dv i.wave, fe vce(cluster pidp)

xtreg employed married sex_dv age_dv hiqual_dv bornuk_dv nchild_dv ethn_dv, i.pidp
*Should give you the same, but it would take a lifetime to run


*And now I weight the regressions
xtreg employed married sex_dv age_dv i.hiqual_dv bornuk_dv nchild_dv i.ethn_dv [pweight = indinus_lw_3], fe vce(robust)
xtreg employed married sex_dv age_dv i.hiqual_dv bornuk_dv nchild_dv i.ethn_dv [pweight = indinus_lw_3], fe vce(cluster pidp)
xtreg employed married sex_dv age_dv i.hiqual_dv bornuk_dv nchild_dv i.ethn_dv [pweight = indinus_lw_3] i.wave, fe vce(cluster pidp)


* I install the command that allows me to include high-dimensional FEs
ssc install reghdfe

reghdfe employed married age_dv i.hiqual_dv nchild_dv [pweight = indinus_lw_3], absorb(pidp) vce(cluster pidp)
reghdfe employed married age_dv i.hiqual_dv nchild_dv [pweight = indinus_lw_3], absorb(pidp) vce(robust)
reghdfe employed married age_dv i.hiqual_dv nchild_dv [pweight = indinus_lw_3], absorb(pidp wave gor) vce(robust)

reghdfe employed married age_dv i.hiqual_dv nchild_dv [pweight = indinus_lw_3], absorb(pidp) vce(robust) keepsingleton
reghdfe employed married age_dv i.hiqual_dv nchild_dv [pweight = indinus_lw_3], absorb(pidp gor_dv) vce(robust)

*Heterogeneity by sex
reghdfe employed married age_dv i.hiqual_dv nchild_dv [pweight = indinus_lw_3] if sex_dv == 1, absorb(pidp gor_dv) vce(robust)
reghdfe employed married age_dv i.hiqual_dv nchild_dv [pweight = indinus_lw_3] if sex_dv == 2, absorb(pidp gor_dv) vce(robust)
