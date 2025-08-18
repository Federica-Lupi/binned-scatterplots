* Install packages
ssc install gtools, replace
net install binscatter2, from("https://raw.githubusercontent.com/mdroste/stata-binscatter2/master/") replace
cap ado uninstall ftools
net install ftools, from("https://raw.githubusercontent.com/sergiocorreia/ftools/master/src/")
net install reghdfe, from("https://raw.githubusercontent.com/sergiocorreia/reghdfe/master/src/") replace
ssc install ivreg2, replace
net install ivreghdfe, from("https://raw.githubusercontent.com/sergiocorreia/ivreghdfe/master/src/") replace

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
