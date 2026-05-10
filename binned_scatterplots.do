* Install packages
ssc install gtools, replace
ssc install ivreg2, replace
net install ftools, from("https://raw.githubusercontent.com/sergiocorreia/ftools/master/src/") replace
net install require, from("https://raw.githubusercontent.com/sergiocorreia/stata-require/master/src/") replace
net install reghdfe, from("https://raw.githubusercontent.com/sergiocorreia/reghdfe/master/src/") replace
net install ivreghdfe, from("https://raw.githubusercontent.com/sergiocorreia/ivreghdfe/master/src/") replace
net install binscatter2, from("https://raw.githubusercontent.com/mdroste/stata-binscatter2/master/") replace
net install rdlocrand, from("https://raw.githubusercontent.com/rdpackages/rdlocrand/master/stata") replace
net install rdrobust, from("https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata") replace

* Set seed
set seed 1234

* Not any scatterplot

webuse auto, clear
scatter price weight

scatter mpg weight

clear
set obs 100
drawnorm w x z
gen y = x^2
scatter y x

webuse nlsw88, clear
scatter wage tenure

binscatter2 wage tenure

* Residualized binned scatterplot

webuse nlsw88, clear
reg wage industry occupation
predict wage_r, residuals
reg tenure industry occupation
predict tenure_r, residuals
binscatter2 wage_r tenure_r

binscatter2 wage tenure, controls(industry occupation)

sum wage wage_r tenure tenure_r

binscatter2 wage tenure, controls(industry occupation) absorb(race age)

binscatter2 wage tenure, controls(industry occupation) absorb(race age) xti("Job tenure in years") yti("Hourly wage") title("Effect of job tenure on wage", c(black))

binscatter2 wage tenure, controls(industry occupation) absorb(race age) xti("Job tenure in years") yti("Hourly wage") title("Effect of job tenure on wage", c(black)) ylab(, nogrid) xlab(, nogrid) mc(gs12) lc(black)

binscatter2 wage tenure, controls(industry occupation) absorb(race age) xti("Job tenure in years") yti("Hourly wage") title("Effect of job tenure on wage", c(black)) xlab(, nogrid) ylab(, nogrid) mc(gs12) lc(black) n(30)

reghdfe wage tenure industry occupation, absorb(race age)
local my_beta: display %5.3f _b[tenure]
local my_se: display %5.3f _se[tenure]
binscatter2 wage tenure, controls(industry occupation) absorb(race age) xti("Job tenure in years") yti("Hourly wage") title("Effect of job tenure on wage", c(black)) xlab(, nogrid) ylab(, nogrid) mc(gs12) lc(black) n(30) note("Regression coefficient: `my_beta'" "Standard error: `my_se'")

reghdfe wage tenure industry occupation, absorb(race age)
local my_beta: display %5.3f _b[tenure]
local my_se: display %5.3f _se[tenure]
local my_pval: display 2*ttail(e(df_r), abs(_b[tenure]/_se[tenure]))
local addstars = ""
if `my_pval' <= 0.1 & `my_pval' > 0.05 {
	local addstars = "*"
}
if `my_pval' <= 0.05 & `my_pval' > 0.01 {
	local addstars = "**"
}
if `my_pval' <= 0.01 {
	local addstars = "***"
}
binscatter2 wage tenure, controls(industry occupation) absorb(race age) xti("Job tenure in years") yti("Hourly wage") title("Effect of job tenure on wage", c(black)) xlab(, nogrid) ylab(, nogrid) mc(gs12) lc(black) n(30) note("Regression coefficient: `my_beta'`addstars'" "Standard error: `my_se'")

reghdfe wage tenure industry occupation, absorb(race age)
local my_beta: display %5.3f _b[tenure]
local my_se: display %5.3f _se[tenure]
local my_pval: display 2*ttail(e(df_r), abs(_b[tenure]/_se[tenure]))
local addstars = ""
if `my_pval' <= 0.1 & `my_pval' > 0.05 {
	local addstars = "*"
}
if `my_pval' <= 0.05 & `my_pval' > 0.01 {
	local addstars = "**"
}
if `my_pval' <= 0.01 {
	local addstars = "***"
}
binscatter2 wage tenure, controls(industry occupation) absorb(race age) xti("Job tenure in years") yti("Hourly wage") title("Effect of job tenure on wage", c(black)) xlab(, nogrid) ylab(, nogrid) mc(gs12) lc(black) n(30) text(6.5 12.5 "Regression coefficient: `my_beta'`addstars'" 6.0 12.5 "Standard error: `my_se'", place(e) size(small))

* Binned scatterplots for regressions with instrumental variables

webuse hsng, clear
drop state
set obs 2050
gen obs_num = _n
foreach x in pop popgrow popden pcturban faminc hsng hsnggrow hsngval rent {
    sum `x'
    scalar min_`x' = r(min)
    scalar max_`x' = r(max)
    replace `x' = runiform(min_`x', max_`x') if obs_num > 50
}
foreach x in popgrow pcturban hsnggrow {
	replace `x' = round(`x', 0.1)
}
foreach x in pop popden faminc hsng hsngval rent {
	replace `x' = round(`x', 1.0)
}
replace division = runiformint(1,9) if obs_num > 50
replace region = runiformint(1,4) if obs_num > 50

ivreghdfe rent (hsngval = faminc) pcturban popgrow, absorb(region division)
local my_beta: display %5.3f _b[hsngval]
local my_se: display %5.3f _se[hsngval]
local my_pval: display 2*ttail(e(df_r), abs(_b[hsngval]/_se[hsngval]))
local addstars = ""
if `my_pval' <= 0.1 & `my_pval' > 0.05 {
	local addstars = "*"
}
if `my_pval' <= 0.05 & `my_pval' > 0.01 {
	local addstars = "**"
}
if `my_pval' <= 0.01 {
	local addstars = "***"
}
reghdfe hsngval faminc pcturban popgrow, absorb(region division)
predict hsngval_hat, xb

binscatter2 rent hsngval_hat, controls(pcturban popgrow) absorb(region division) xti("Housing value") yti("Gross rent") title("Effect of house prices on rents", c(black)) xlab(, nogrid) ylab(, nogrid) mc(gs12) lc(black) n(30) note("Regression coefficient: `my_beta'`addstars'" "Standard error: `my_se'")

binscatter2 rent hsngval_hat, controls(pcturban popgrow) absorb(region division) xti("Housing value") yti("Gross rent") title("Effect of house prices on rents", c(black)) xlab(, format(%9.0gc) nogrid) ylab(, nogrid) mc(gs12) lc(black) n(30) note("Regression coefficient: `my_beta'`addstars'" "Standard error: `my_se'")

* Binned scatterplots for regression discontinuity design

clear
set obs 100
gen id_school = _n
gen time = runiform(30,1000)
replace time = round(time,1.0)
tempfile schools
save `schools'
clear
set obs 2500
gen id_teach = _n
gen id_school = 1
forvalues i = 1(1)99 {
	replace id_school = `i'+1 if id_teach > 25*`i'
}
merge m:1 id_school using `schools', nogen
gen treat_time = (time > 120)
gen female_drop = runiform()
gen female = 0
replace female = 1 if female_drop > 0.4
drop female_drop
gen uni_degree_drop = runiform()
gen uni_degree = 0
replace uni_degree = 1 if uni_degree_drop > 0.7
drop uni_degree_drop
gen wage = .
replace wage = runiform(2000,2500) if treat_time == 0
replace wage = runiform(2000,3500) if treat_time == 1
replace wage = round(wage,1.0)
gen competence_score = .
replace competence_score = rnormal(100,25) if treat_time == 0
replace competence_score = rnormal(120,25) if treat_time == 1
replace competence_score = round(competence_score,1.0)

binscatter2 wage time, rd(120) xla(, format(%9.0gc) nogrid) yla(, format(%9.0gc) nogrid) xti("Travel time by car from main city in region") yti("Wage")

binscatter2 wage time, by(treat_time) msymbol(o o) xline(120) xla(, format(%9.0gc) nogrid) yla(, format(%9.0gc) nogrid) legend(pos(6) col(2) order(1 "Low wage bonus" 2 "High wage bonus")) xti("Travel time by car from main city in region") yti("Wage")

rdwinselect time female uni_degree, cutoff(120) wmass
local window_left = r(w_left)
local window_right = r(w_right)

binscatter2 competence_score time, rd(120) xline(`window_left' `window_right') xla(, format(%9.0gc) nogrid) yla(, nogrid) xti("Travel time by car from main city in region") yti("Competence score")

binscatter2 competence_score time, by(treat_time) msymbol(o o) xline(`window_left' 120 `window_right') xla(, format(%9.0gc) nogrid) yla(, nogrid) legend(pos(6) col(2) order(1 "Low wage bonus" 2 "High wage bonus")) xti("Travel time by car from main city in region") yti("Competence score")

rdplot competence_score time, c(120) masspoints(off) graph_options(xla(, format(%9.0gc) nogrid) yla(, nogrid) xti("Travel time by car from main city in region") yti("Competence score") title("") legend(off))
