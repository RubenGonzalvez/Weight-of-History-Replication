*==============================================================================*
* Replication files from ICPSR
* Authors: Daniel F.Romero and Rubén Gonzálvez
* Date: January 2026
* This code adapt the results from Gertler and Karadi (2015) 
* using local projections
*==============================================================================*

*------------------------------------------------------------------------------*
**# Replication Jarocinski and Kaeadi (2020)
*------------------------------------------------------------------------------*

clear all

import excel "$Folder/1_Data/Gertler-Karadi-2015-data_VAR.xlsx", ///
	sheet("VAR_data") firstrow clear

	
gen time = ym(year,month)
format time %tm
order time

tsset time

save "$Folder/1_Data/Gertler-Karadi-2015-data_VAR.dta" , replace


**# Import shocks

import excel "$Folder/1_Data/Gertler-Karadi-2015-shocks.xlsx", ///
	sheet("factor_data") firstrow clear

gen time = ym(year,month)
format time %tm
order time

tsset time

merge 1:1 time using "$Folder/1_Data/Gertler-Karadi-2015-data_VAR.dta"

order time
sort time


drop _merge


gen mon_shock_month = ff4_tc*100
*tsline mon_shock_month

tsset time

*------------------------------------------------------------------------------*
**# VAR 12 lags and a constant
*------------------------------------------------------------------------------*

gen dgs1 = d.gs1


var logip logcpi dgs1 ebp, lags(1/12)

predict u_r, eq(dgs1) residuals


reg u_r ff4_tc
*F-statistic: 21.517022
reg u_r ff4_tc, vce(robust)
*F-statistic: 17.583008





* Choose impulse response horizon
local hmax = 11
gen logip_new = log(ip)
gen logcpi_new = log(cpi)

* Cumulative
 foreach var of varlist logip_new logcpi_new  {
	qui forvalues h = 0/`hmax' {
		gen `var'_`h' = (f`h'.`var' - l.`var')*100
	}
}


* Cumulative
 foreach var of varlist gs1 ebp  {
	qui forvalues h = 0/`hmax' {
		gen `var'_`h' = (f`h'.`var' - l.`var')
	}
}

/* Run the LPs */


eststo clear
cap drop b u d Years Zero
gen Years = _n-1 if _n<=`hmax'
gen Zero =  0    if _n<=`hmax'


foreach var of varlist gs1 logcpi logip ebp {
gen b_`var'=0
gen u_`var'=0
gen d_`var'=0
gen u1_`var'=0
gen d1_`var'=0
}


forv h = 0/`hmax' {

	 reg gs1_`h' ff4_tc l(1/8).dgs1 l(1/8).logcpi_new l(1/8).logip_new l(1/8).ebp, vce(robust)
	 

replace b_gs1 = _b[ff4_tc]                    if _n == `h'+1
replace u_gs1 = _b[ff4_tc] + 1* _se[ff4_tc]  if _n == `h'+1
replace d_gs1 = _b[ff4_tc] - 1* _se[ff4_tc]  if _n == `h'+1
replace u1_gs1 = _b[ff4_tc] + 1.645* _se[ff4_tc]  if _n == `h'+1
replace d1_gs1 = _b[ff4_tc] - 1.645* _se[ff4_tc]  if _n == `h'+1
eststo
}

forv h = 0/`hmax' {

	 ivreg2 logip_new_`h' (dgs1 = ff4_tc) l(1/8).gs1 l(1/8).logcpi_new l(1/8).logip_new l(1/8).ebp, gmm2s bw(auto)
	 

replace b_logip = _b[dgs1]                    if _n == `h'+1
replace u_logip = _b[dgs1] + 1* _se[dgs1]  if _n == `h'+1
replace d_logip = _b[dgs1] - 1* _se[dgs1]  if _n == `h'+1
replace u1_logip = _b[dgs1] + 1.645* _se[dgs1]  if _n == `h'+1
replace d1_logip = _b[dgs1] - 1.645* _se[dgs1]  if _n == `h'+1
eststo
}



twoway ///
	(rarea u_gs1 d_gs1  Years,  ///
	fcolor(gs9%70) lcolor(gs9%70) lw(none) lpattern(solid)) ///
	(rarea u1_gs1 d1_gs1  Years,  ///
	fcolor(gs13%50) lcolor(gs13%50) lw(none) lpattern(solid)) ///
	(line b_gs1 Years, lcolor(black) ///
	lpattern(solid) lwidth(mthick)), legend(off) ///
	title("(a) One-year rate", color(black) size(medsmall)) ///
	subtitle("Gertler and Karadi (2015) shock", color(black) size(small)) ///
	ytitle("Percentage points", size(medsmall)) ///
	xtitle("Horizon (Months)", size(medsmall)) ///
	ysc(r(-1(1)6)) ///
	ylabel(-1(1)6) ///
	xsc(r(0(1)10)) ///
	xlabel(0(1)10) ///
	yline(0, lcolor(black) lpattern("-")) ///
	graphregion(color(white)) plotregion(color(white))

gr rename fig_gs1, replace

twoway ///
	(rarea u_logip d_logip  Years,  ///
	fcolor(gs9%70) lcolor(gs9%70) lw(none) lpattern(solid)) ///
	(rarea u1_logip d1_logip  Years,  ///
	fcolor(gs13%50) lcolor(gs13%50) lw(none) lpattern(solid)) ///
	(line b_logip Years, lcolor(black) ///
	lpattern(solid) lwidth(mthick)), legend(off) ///
	title("(b) Industrial Production", color(black) size(medsmall)) ///
	ytitle("Percent", size(medsmall)) ///
	subtitle("Gertler and Karadi (2015) shock", color(black) size(small)) ///
	xtitle("Horizon (Months)", size(medsmall)) ///
	ysc(r(-10(2)5)) ///
	ylabel(-10(2)5) ///
	xsc(r(0(1)10)) ///
	xlabel(0(1)10) ///
	yline(0, lcolor(black) lpattern("-")) ///
	graphregion(color(white)) plotregion(color(white))

gr rename fig_logip, replace

*------------------------------------------------------------------------------*
**# Average Decomposition
*------------------------------------------------------------------------------*


preserve 

gen quarter_id = _n  // Create identifier for merging weights by quarter

forvalues i = 0/11 {
	
	* IV regression
	ivreg2 logip_new_`i' (dgs1 = ff4_tc) l(1/8).gs1 l(1/8).logcpi_new l(1/8).logip_new l(1/8).ebp, gmm2s bw(auto)

	* FWL residualization
	reg logip_new_`i' l(1/8).gs1 l(1/8).logcpi_new l(1/8).logip_new l(1/8).ebp
	predict y_resid`i', resid

	reg dgs1 l(1/8).gs1 l(1/8).logcpi_new l(1/8).logip_new l(1/8).ebp
	predict x_resid`i', resid

	reg ff4_tc l(1/8).gs1 l(1/8).logcpi_new l(1/8).logip_new l(1/8).ebp
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
                              rel_weight10 rel_weight11)
							  
drop if time <= 359

twoway (tsline ff4_tc, lcolor(red%30)), ///
		 legend(off) ///
		 ytitle("") ///
		 xsc(r(360(24)629)) ///
		 xlabel(360(24)629) ///
		 title("(a) Gertler and Karadi (2015) shocks", color(black) size(med)) xtitle("Months") ///
		 name(gk15_shocks, replace)
		 
graph export "$Folder/3_Output/Graphs/GK_2015/gk15_shocks.pdf", as(pdf) replace	

twoway (tsline rel_weight_avg, lcolor(blue%70)), ///
		 legend(off) ///
		 ytitle("") ///
		 xsc(r(360(24)629)) ///
		 xlabel(360(24)629) ///
		 title("(b) Weights for Industrial Production", color(black) size(med)) xtitle("Months") ///
		 name(avg_weight_output, replace)
		 
graph export "$Folder/3_Output/Graphs/GK_2015/IP_Weights.pdf", as(pdf) replace	

restore
*------------------------------------------------------------------------------*

*------------------------------------------------------------------------------*
**# Combine graphs
*------------------------------------------------------------------------------*


gr combine fig_gs1 fig_logip, cols(2) altshrink ysize(2) imargin(medlarge)

gr rename irf_output_GK2015

graph export "$Folder/3_Output/Graphs/GK_2015/irf_output_GK2015.pdf", ///
	as(pdf) name("irf_output_GK2015") replace
	
gr combine gk15_shocks avg_weight_output, cols(2) altshrink ysize(2) imargin(medlarge)

gr rename weights_GK15

graph export "$Folder/3_Output/Graphs/GK_2015/weights_GK15.pdf", ///
	as(pdf) name("weights_GK15") replace

