
** This exercise in borrowed from: https://www.jonathandroth.com/did-resources/
** The data and code are essentially the same, just a bit simplified

** We first start by installing all the packages we need
* reghdfe
ssc install reghdfe
* honestdid
net install honestdid, from("https://raw.githubusercontent.com/mcaceresb/stata-honestdid/main") replace
honestdid _plugin_check
* csdid 
net install csdid, from ("https://raw.githubusercontent.com/friosavila/csdid_drdid/main/code/") replace


*Set the directory
cd "c:/Users/`c(username)'/Dropbox/Causal inference for Panel Data/To share/Hands-on/data"
*Load the data
use "data_ex2", clear

br
*We have:
*Treat : a dummy taking the value 1 if the state is in the treatment group, 0 otherwise
*Post : a dummy taking the value 1 if the year is in the post treatment period, 0 otherwise
*We generate the main DiD variable

gen treat_post = Treat*Post


* Run the TWFE spec
reghdfe dins treat_post, absorb(stfips year) cluster(stfips) 

xtset stfips year
xtreg dins treat_post i.year, fe vce(cluster stfips) 

* I want to test parallel trends
* First, I do a simple plot 
collapse dins, by (Treat year) 

twoway (line dins year if Treat==1, lwidth(medthick)) ///
       (line dins year if Treat==0, lpattern(dash) lwidth(medthick)), ///
	   xline(2013, lcolor(gs8) lpattern(dash)) ///
	   xscale(range(2008 2015)) ///
       xlabel(2008(1)2015) ///
       legend(label(1 "Treated") label(2 "Control")) ///
       ytitle("dins") xtitle("Year")
	   
* Event study estimation and plot

use "data_ex2", clear

* # is the operator for the interaction term in Stata

reghdfe dins b2013.year##Treat, absorb(stfips) cluster(stfips) noconstant

* For the coefplot, I am telling Stata to only keep the interaction terms (it avoids to include the year FEs)
coefplot, keep(*#*) vertical yline(0) ciopts(recast(rcap)) xlabel(,angle(45)) ytitle("Estimate and 95% Conf. Int.") title("Effect on dins")

* Let's play with the honest did package
* This command allows you to test the sensitivity of your estimates to an eventual violation of parallel trends
* pre () specifies how many time-unit you have in the pre treatment period
* post () specifies how many time-unit you have in the post treatment period 
* period 6 is omitted because is the year of the treatment
* mvec() it specifies how much violation you want to allow. In this case, we are testing between 0.5 and 2 (half or double the max violation of parallel trends we observe in the pre treatment period)

honestdid, pre(1/5) post(7/8) mvec(0.5(0.5)2) coefplot xtitle("M") ytitle("95% Robust CI")
honestdid, pre(1/5) post(7/8) mvec(0.5(0.5)3) coefplot xtitle("M") ytitle("95% Robust CI")
honestdid, pre(1/5) post(7) mvec(0.5(0.5)3) coefplot xtitle("M") ytitle("95% Robust CI")
honestdid, pre(1/5) post(7) mvec(1(1)3) coefplot xtitle("M") ytitle("95% Robust CI")

*Placebo test

*I generate an alternative treatment variable
gen Post2 = 0
replace Post2 = 1 if year > 2009
replace Post2 = . if year > 2012

gen treat_post2 = Treat*Post2

reghdfe dins treat_post2, absorb(stfips year) cluster(stfips) 
