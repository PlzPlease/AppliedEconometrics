// ECON 613 Assignment 5 
// Name: Promvarat Pradit
clear
set more off, permanently
set type double, permanently
estimates clear
cd "C:\Users\promv\Dropbox\Master Course work\Spring 2019\Econ 613 Applied econometric micro\Assignment 5"
log close _all
log using "Assignment 5.smcl", replace name("Assignment 5")


***********************************************************************
************* Redo Assignment 2 : OLS and Discrete Choice *************
***********************************************************************
****** Exercise 1 : Data creation ******
// Set random seed
set seed 12345
// Set number of observation
set obs 10000
// Generate X1: vector of 10,000 draws from a uniform distribution with range 1:3
gen X1 = runiform(1,3)
// Generate X2: vector of 10,000 draws from a gamma distribution with shape 3 and scale 2
gen X2 = rgamma(3,2)
// Generate X3: vector of 10,000 draws from a binomial distribution with probability 0.3
gen X3 = rbinomial(1,0.3)
// Generate eps: vector of 10,000 draws from a normal distribution with mean 2 and sd 1
gen eps = rnormal(2,1)
// create Y variable: Y = 0.5 + 1.2 X1 + -0.9 X2 + 0.1 X3 + eps
gen Y = 0.5 + 1.2*X1 + (-0.9)*X2 + 0.1*X3 + eps
// create Y_dum: Y_dum = 1 if Y > mean(Y), =0 otherwise
egen Y_bar = mean(Y)
gen Y_dum = 0
replace Y_dum = 1 if Y > Y_bar
drop Y_bar

****** Exercise 2 : OLS ******
// Calculate the correlation between Y and X1. How different is it from 1.2?
corr Y X1
// We are interested in the outcome of the regression of Y on X where X = (1,X1,X2,X3)
// Calculate the coefficients on this regression
// Calculate the standard errors
eststo closed_form: reg Y X1 X2 X3									//Using the standard formulas of the OLS
eststo bootstrap_49: reg Y X1 X2 X3, vce(bootstrap, rep(49))			//Using bootstrap with 49 replications
eststo bootstrap_499: reg Y X1 X2 X3, vce(bootstrap, rep(499))		//Using bootstrap with 499 replications
esttab *, se(3) title(OLS Table: Continuous Y) mlabels(,titles)
estimates clear

****** Exercise 4 : Discrete choice ******
// We consider the determinants of ydum
// Probit model
eststo probit: probit Y_dum X1 X2 X3
// Logit Model
eststo logit: logit Y_dum X1 X2 X3
// Linear probability model
eststo linear_probability: reg Y_dum X1 X2 X3	
esttab *, se(3) title(Discrete Choice Table: Y dummy)  mlabels(,titles)
estimates clear

****** Exercise 5 : Marginal Effects ******
// Compute the marginal effect of X on Y according to Probit and Logit models
// then Compute the standard deviations using 1) The delta method 2) Bootstrap
// Note: margins command calculate average matginal effect and SE (can change to ME at mean by option atmeans)
// Probit 
quietly probit Y_dum X1 X2 X3
margins, dydx(X1 X2 X3)  						
// Logit models
quietly logit Y_dum X1 X2 X3
margins, dydx(X1 X2 X3) 


*******************************************************************
************* Redo Assignment 3 : Multinomial Choices *************
*******************************************************************
clear
****** Exercise 1 : Data Description ******
// Average and dispersion in product characteristics
import delimited "product.csv"
summarize p*
// Market share, and market share by product characteristics
//// Market share by product (can look at Percent column)
tab choice
//// Market share of each brand by type (Stick / Tub)
tab choice if choice <=6	//stick choice 1-6
tab choice if choice >6		//tub choice 7-10
//// Market share of each brand
gen brand = "pk"
replace brand = "bb" if choice == 2
replace brand = "fl" if choice == 3 | choice == 9
replace brand = "hse" if choice == 4 | choice == 10
replace brand = "gen" if choice == 5
replace brand = "imp" if choice == 6
replace brand = "ss" if choice == 7
tab brand
// Mapping between observed attributes and choices.
tempfile product
save `product'
import delimited "demos.csv", clear
tempfile demos
save `demos'
use `product', clear
merge m:1 hhid using `demos', nogen
//// Market share of each product by income
tab income choice, cell nofreq
//// Market share of each product by family size
tab fam_size choice, cell nofreq
//// Market share of each product by college degree
tab college choice, cell nofreq
//// Market share of each product by job type
tab whtcollar choice, cell nofreq
//// Market share of each product by retirement status
tab whtcollar choice, cell nofreq

****** Exercise 2 : First Model ******
// reshape data from wide to long
rename (p*) (p_1 p_2 p_3 p_4 p_5 p_6 p_7 p_8 p_9 p_10)
reshape long p_, i(v1) j(product)
// create dependent variable for conditional logit
gen choice_clogit = 0
replace choice_clogit = 1 if choice==product
rename p_ price
// Conditional logit (Alternative specific conditional logit)
eststo Conditional_logit: asclogit choice_clogit price, case(v1) alternatives(product) base(1)

****** Exercise 3 : Second Model ******
// create dependent variable for multinomial logit
gen choice_mlogit = 0
replace choice_mlogit = choice if choice==product
// Multinomial Logit
eststo Multinomial_logit: mlogit choice_mlogit income fs3_4 fs5 college whtcollar retired, base(1)
*esttab *, se(3) title(Multi-Choice Table) 	//Note this line was commented out because both model reports doesn't fits well, so I reported it separately
estimates clear

****** Exercise 4 : Marginal Effects ******
// Marginal effect of conditional logit
quietly asclogit choice_clogit price, case(v1) alternatives(product) base(1)
estat mfx 					//asclogit cannot use margins
// Marginal effect of multinomial logit
quietly mlogit choice_mlogit income fs3_4 fs5 college whtcollar retired, base(1)
margins, dydx(income) 		// This took some time, but it will run

****** Exercise 5 : IIA ******
// Mixed logit
eststo Mixed_full: asmixlogit choice_clogit price, case(v1) casevars(income fs3_4 fs5 college whtcollar retired) alternatives(product) base(1)
// drop choice 1 data
drop if choice == 1
drop if product == 1
// run Mixed logit again to get restriced model
eststo Mixed_drop: asmixlogit choice_clogit price, case(v1) casevars(income fs3_4 fs5 college whtcollar retired) alternatives(product) base(2)
lrtest ( Mixed_full) ( Mixed_drop), stats force  //reject null so IIA do not hold
estimates clear

*****************************************************************
************* Redo Assignment 4 : Linear Panel Data *************
*****************************************************************
clear
****** Exercise 1 : Data ******
// Represent the panel dimension of wages for 5 randomly selected individuals.
// Load data
import delimited "Koop-Tobias.csv"
xtset personid timetrnd
//NOTE: I don't think we need to restrict to just 5 individual to represent panel dimension
//So, run the command in the next line will just show a pattern of panel dimension for all individuals
xtdescribe, patterns(20)
// However, if we wnat to select just 5 individuals then the following code should do the job
//// Create a list of person id to keep
collapse logwage , by(personid)
keep personid
gen sortorder = runiform()
sort sortorder
gen order = _n
keep if order <= 5
keep personid
tempfile tagid
save `tagid'
//// merge tag back to main data
import delimited "Koop-Tobias.csv", clear
merge m:1 personid using `tagid'
keep if _m == 3
keep personid logwage timetrnd
xtset personid timetrnd
xtdescribe, patterns(5)

****** Exercise 2 : Random Effects ******
import delimited "Koop-Tobias.csv", clear
xtset personid timetrnd
eststo RE: xtreg logwage educ potexper, re

****** Exercise 3 : Fixed Effects Model ******
// Between estimator
eststo Between: xtreg logwage educ potexper, be
// Within Estimator
eststo Within: xtreg logwage educ potexper, fe
// First time dfference Estimator
//// Drop all skipping years version
eststo FD_no_time_skip: reg d.logwage d.educ d.potexper		
//// Keep skipping year version
sort personid timetrnd
by personid: gen dlogwage = logwage - logwage[_n-1]
by personid: gen deduc = educ - educ[_n-1]
by personid: gen dpotexper = potexper - potexper[_n-1]
eststo FD_time_skip: reg dlogwage deduc dpotexper	
esttab *, se(3) title(Panel Regression Table) mtitle
estimates clear


*****************************************************************

log close _all
