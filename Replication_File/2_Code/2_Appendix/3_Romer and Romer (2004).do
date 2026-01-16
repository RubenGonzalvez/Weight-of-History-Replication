
*==============================================================================*
* Replication files from ICPSR
* Authors: Daniel F. Romero and Rubén Gonzálvez
* Date: January 2026
* This code adapt the results from Jordá using the following codes: 

**** Romer and Romer (2004) shocks updated by Wieland and Yang (2019)
**** Available from ICPSR

**** "resid" are the original Romer-Romer (2004) shocks
**** "resid_romer" are the shocks based on the original Romer-Romer (2004)
**** regression
**** "resid_full" are the shocks based on running the Romer-Romer(2004)
**** regression on the full 1969-2007 sample
*==============================================================================*

*------------------------------------------------------------------------------*
**# Import data
*------------------------------------------------------------------------------*

clear all

use "$Folder/1_Data/RR_monetary_shock_quarterly.dta", clear

**** Next, merge the unemployment rate and gdp from the St. Louis Fred

merge 1:1 date using "$Folder/1_Data/lpiv_15Mar2022.dta"
drop _merge

**** Keep only nonmissing observations in resid_full i.e. 1969m1 - 2007m12
keep if resid_full != .

**** tsset to use time series commands
tsset date

* Choose impulse response horizon
local hmax = 16

/* Generate LHS variables for the LPs */
* Cumulative
forvalues h = 0/`hmax' {
	gen ur`h' = f`h'.UNRATE 
	 
}

forvalues h = 0/`hmax' {
	gen gdp`h' = f`h'.log_rgdp - l.log_rgdp
	 
}

forvalues h = 0/`hmax' {
	gen DFF`h' = f`h'.DFF
	 
}

*------------------------------------------------------------------------------*
**# Unemployment response
*------------------------------------------------------------------------------*

* LP-OLS
eststo clear
cap drop b_ls u_ls d_ls Quarters Zero
gen Quarters = _n-1 if _n<=`hmax'
gen Zero =  0     if _n<=`hmax'
gen b_ls=0
gen u_ls=0
gen d_ls=0
qui forv h = 0/`hmax' {
	 newey ur`h' DFF l(1/4).DFF l(1/4).UNRATE l(1/4).log_rgdp, lag(`h')
replace b_ls = _b[DFF]                    if _n == `h'+1
replace u_ls = _b[DFF] + 1.645* _se[DFF]  if _n == `h'+1
replace d_ls = _b[DFF] - 1.645* _se[DFF]  if _n == `h'+1
eststo
}
*** Use this command if you want a summary of the LP coefficients
nois esttab , se nocons keep(DFF)

* LP-IV
eststo clear
cap drop b_iv u_iv d_iv u_iv1 d_iv1 

gen b_iv=0
gen u_iv=0
gen d_iv=0
gen u_iv1=0
gen d_iv1=0
forv h = 0/`hmax' {
	  ivregress gmm ur`h' l(1/4).DFF l(1/4).UNRATE l(1/4).log_rgdp (DFF = resid_full), vce(hac nwest)
replace b_iv = _b[DFF]                    if _n == `h'+1
replace u_iv = _b[DFF] + 1* _se[DFF]  if _n == `h'+1
replace d_iv = _b[DFF] - 1* _se[DFF]  if _n == `h'+1
replace u_iv1 = _b[DFF] + 1.645* _se[DFF]  if _n == `h'+1
replace d_iv1 = _b[DFF] - 1.645* _se[DFF]  if _n == `h'+1
eststo
}
*** Use this command if you want a summary of the LP coefficients
nois esttab , se nocons keep(DFF)

twoway ///
	(rarea u_iv d_iv  Quarters,  ///
	fcolor(gs9%70) lcolor(gs9%70) lw(none) lpattern(solid)) ///
	(rarea u_iv1 d_iv1 Quarters,  ///
	fcolor(gs13%50) lcolor(gs13%50) lw(none) lpattern(solid)) ///
	(line b_iv Quarters, lcolor(black) lpattern(solid) lwidth(mthick)), ///
	yline(0, lcolor(black) lpattern("-")) ///
	legend(off) ///
	xsc(r(0(1)15)) ///
	xlabel(0(1)15) ///
	title("(a) Response of the unemployment rate to monetary shock", color(black) size(med)) ///
	subtitle("Romer and Romer (2004) shocks", color(black) size(small)) ///
	ytitle("Percentage points", size(medsmall)) xtitle("Horizon (Quarters)", size(medsmall)) ///
	graphregion(color(white)) plotregion(color(white))

gr rename fig_unemployment, replace

*------------------------------------------------------------------------------*
**# Average Decomposition UNRATE
*------------------------------------------------------------------------------*

preserve 

gen quarter_id = _n  // Create identifier for merging weights by quarter

forvalues i = 0/16 {
	
	* IV regression
	ivregress gmm ur`i' l(1/4).DFF l(1/4).UNRATE l(1/4).log_rgdp (DFF = resid_full), vce(hac nwest)

	* FWL residualization
	reg ur`i' l(1/4).DFF l(1/4).UNRATE l(1/4).log_rgdp
	predict y_resid`i', resid

	reg DFF l(1/4).DFF l(1/4).UNRATE
	predict x_resid`i', resid

	reg resid_full l(1/4).DFF l(1/4).UNRATE l(1/4).log_rgdp
	predict z_resid`i', resid

	* First stage: x_hat
	reg x_resid`i' z_resid`i', nocons
	predict xhat_resid`i', xb

	* Second stage
	reg y_resid`i' xhat_resid`i', nocons
	scalar beta_hat`i' = _b[xhat_resid`i']

	* Contribution
	gen contrib`i' = xhat_resid`i' * y_resid`i'
	egen total_contrib`i' = total(contrib`i')
	gen rel_weight`i' = contrib`i' / total_contrib`i'
}

egen rel_weight_avg = rowmean(rel_weight0 rel_weight1 rel_weight2 rel_weight3 rel_weight4 ///
                              rel_weight5 rel_weight6 rel_weight7 rel_weight8 rel_weight9 ///
                              rel_weight10 rel_weight11 rel_weight12 rel_weight13 rel_weight14 ///
                              rel_weight15 rel_weight16)

	
twoway (tsline rel_weight_avg, lcolor(blue%70)), ///
		 legend(off) ///
		 ytitle("") ///
		 title("(a) Weights for unemployment rate", color(black) size(med)) xtitle("Quarters") ///
		 name(avg_weight_ur, replace)

graph export "$Folder/3_Output/Graphs/Romers_2004/UR_weight.pdf", as(pdf)  replace	
		 
twoway (tsline resid_full, lcolor(red%30)), ///
		legend(off) ///
		name(rr04_shocks, replace)
	
graph export "$Folder/3_Output/Graphs/Romers_2004/rr04_shocks.pdf", as(pdf)  replace	

restore
*------------------------------------------------------------------------------*

*------------------------------------------------------------------------------*
**# GDP response IRF
*------------------------------------------------------------------------------*

* LP-OLS
eststo clear
cap drop b_ls u_ls d_ls Quarters Zero
gen Quarters = _n-1 if _n<=`hmax'
gen Zero =  0     if _n<=`hmax'
gen b_ls=0
gen u_ls=0
gen d_ls=0
qui forv h = 0/`hmax' {
	 newey gdp`h' DFF l(1/4).DFF l(1/4).UNRATE l(1/4).log_rgdp, lag(`h')
replace b_ls = _b[DFF]                    if _n == `h'+1
replace u_ls = _b[DFF] + 1.645* _se[DFF]  if _n == `h'+1
replace d_ls = _b[DFF] - 1.645* _se[DFF]  if _n == `h'+1
eststo
}
*** Use this command if you want a summary of the LP coefficients
nois esttab , se nocons keep(DFF)

* LP-IV
eststo clear
cap drop b_iv u_iv d_iv u_iv1 d_iv1 

gen b_iv=0
gen u_iv=0
gen d_iv=0
gen u_iv1=0
gen d_iv1=0
forv h = 0/`hmax' {
	  ivregress gmm gdp`h' l(1/4).DFF l(1/4).UNRATE l(1/4).log_rgdp (DFF = resid_full), vce(hac nwest)
replace b_iv = _b[DFF]                    if _n == `h'+1
replace u_iv = _b[DFF] + 1* _se[DFF]  if _n == `h'+1
replace d_iv = _b[DFF] - 1* _se[DFF]  if _n == `h'+1
replace u_iv1 = _b[DFF] + 1.645* _se[DFF]  if _n == `h'+1
replace d_iv1 = _b[DFF] - 1.645* _se[DFF]  if _n == `h'+1
eststo
}
*** Use this command if you want a summary of the LP coefficients
nois esttab , se nocons keep(DFF)

twoway ///
	(rarea u_iv d_iv  Quarters,  ///
	fcolor(gs9%70) lcolor(gs9%70) lw(none) lpattern(solid)) ///
	(rarea u_iv1 d_iv1 Quarters,  ///
	fcolor(gs13%50) lcolor(gs13%50) lw(none) lpattern(solid)) ///
	(line b_iv Quarters, lcolor(black) lpattern(solid) lwidth(mthick)), ///
	yline(0, lcolor(black) lpattern("-")) ///
	legend(off) ///
	xsc(r(0(1)15)) ///
	xlabel(0(1)15) ///
	title("(b) Response of GDP to monetary shock", color(black) size(med)) ///
	subtitle("Romer and Romer (2004) shock", color(black) size(small)) ///
	ytitle("Percent", size(medsmall)) xtitle("Horizon (Quarters)", size(medsmall)) ///
	graphregion(color(white)) plotregion(color(white))

gr rename fig_gdp, replace

*------------------------------------------------------------------------------*
**# Average Decomposition GDP
*------------------------------------------------------------------------------*


preserve 

gen quarter_id = _n  // Create identifier for merging weights by quarter

forvalues i = 0/16 {
	
	* IV regression
	ivregress gmm gdp`i' l(1/4).DFF l(1/4).UNRATE l(1/4).log_rgdp (DFF = resid_full), vce(hac nwest)

	* FWL residualization
	reg gdp`i' l(1/4).DFF l(1/4).UNRATE l(1/4).log_rgdp
	predict y_resid`i', resid

	reg DFF l(1/4).DFF l(1/4).UNRATE l(1/4).log_rgdp
	predict x_resid`i', resid

	reg resid_full l(1/4).DFF l(1/4).UNRATE l(1/4).log_rgdp
	predict z_resid`i', resid

	* First stage: x_hat
	reg x_resid`i' z_resid`i', nocons
	predict xhat_resid`i', xb

	* Second stage
	reg y_resid`i' xhat_resid`i', nocons
	scalar beta_hat`i' = _b[xhat_resid`i']

	* Contribution
	gen contrib`i' = xhat_resid`i' * y_resid`i'
	egen total_contrib`i' = total(contrib`i')
	gen rel_weight`i' = contrib`i' / total_contrib`i'
}

egen rel_weight_avg = rowmean(rel_weight0 rel_weight1 rel_weight2 rel_weight3 rel_weight4 ///
                              rel_weight5 rel_weight6 rel_weight7 rel_weight8 rel_weight9 ///
                              rel_weight10 rel_weight11 rel_weight12 rel_weight13 rel_weight14 ///
                              rel_weight15 rel_weight16)

twoway (tsline rel_weight_avg, lcolor(blue%70)), ///
		 legend(off) ///
		 ytitle("") ///
		 title("(b) Weights for GDP", color(black) size(med)) xtitle("Quarters") ///
		 name(avg_weight_gdp, replace)
		 
graph export "$Folder/3_Output/Graphs/Romers_2004/GDP_Weights.pdf", as(pdf)  replace	

restore
*------------------------------------------------------------------------------*

*------------------------------------------------------------------------------*
**# Interest rate response response
*------------------------------------------------------------------------------*

* LP-IV
eststo clear
cap drop b_iv u_iv d_iv u_iv1 d_iv1 

gen b_iv=0
gen u_iv=0
gen d_iv=0
gen u_iv1=0
gen d_iv1=0
forv h = 0/`hmax' {
	  ivregress gmm DFF`h' l(1/4).DFF l(1/4).UNRATE l(1/4).log_rgdp resid_full, vce(hac nwest)
replace b_iv = _b[resid_full]                    if _n == `h'+1
replace u_iv = _b[resid_full] + 1* _se[resid_full]  if _n == `h'+1
replace d_iv = _b[resid_full] - 1* _se[resid_full]  if _n == `h'+1
replace u_iv1 = _b[resid_full] + 1.645* _se[resid_full]  if _n == `h'+1
replace d_iv1 = _b[resid_full] - 1.645* _se[resid_full]  if _n == `h'+1
eststo
}
*** Use this command if you want a summary of the LP coefficients
nois esttab , se nocons keep(resid_full)

twoway ///
	(rarea u_iv d_iv Quarters,  ///
	fcolor(gs9%70) lcolor(gs9%70) lw(none) lpattern(solid)) ///
	(rarea u_iv1 d_iv1 Quarters,  ///
	fcolor(gs13%50) lcolor(gs13%50) lw(none) lpattern(solid)) ///
	(line b_iv Quarters, lcolor(black) lpattern(solid) lwidth(mthick)), ///
	yline(0, lcolor(black) lpattern("-")) ///
	legend(off) ///
	xsc(r(0(1)15)) ///
	xlabel(0(1)15) ///
	ysc(r(-1(0.5)2)) ///
	ylabel(-1(0.5)2) ///
	title("(c) Response of interest rate to monetary shock", color(black) size(med)) ///
	subtitle("Romer and Romer (2004) shock", color(black) size(small)) ///
	ytitle("Percent", size(medsmall)) xtitle("Horizon (Quarters)", size(medsmall)) ///
	graphregion(color(white)) plotregion(color(white))

gr rename fig_rate, replace

*------------------------------------------------------------------------------*
**# Combine Graphs
*------------------------------------------------------------------------------*


gr combine fig_unemployment fig_gdp, cols(2) altshrink ysize(2.5) imargin(medlarge)

gr rename irf_output_RR2004, replace

graph export "$Folder/3_Output/Graphs/irf_output_RR2004.pdf", ///
	as(pdf) name("irf_output_RR2004") replace
	
	
gr combine avg_weight_ur avg_weight_gdp, cols(2) altshrink ysize(2.5) imargin(medlarge)

gr rename weights_RR04, replace

graph export "$Folder/3_Output/Graphs/weights_RR04.pdf", ///
	as(pdf) name("weights_RR04") replace

