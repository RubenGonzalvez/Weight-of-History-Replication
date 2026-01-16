*==============================================================================*
* Replication files from ICPSR
* Authors: Daniel F. Romero and Rubén Gonzálvez
* Date: January 2026
* This code adapt the results from Romer and Romer (2010) 
* using local projections
*==============================================================================*

clear all

*------------------------------------------------------------------------------*
**# Import tax data
*------------------------------------------------------------------------------*

import excel "$Folder/1_Data/Romer-RomerFiscalData.xlsx", ///
	sheet("Tax Measures") cellrange(A12:AT264) firstrow clear
	
**drop variables that are not captured

drop AR AS AT

*The variable relevant is EXOGENRRATIO


**# Generate time variable

rename A date
gen time = tq(1945q1) + _n-1

format time %tq
order time
drop date



tsset time

save "$Folder/1_Data/tax_measures_RR", replace

clear all

*------------------------------------------------------------------------------*
**# Import other variables data
*------------------------------------------------------------------------------*


import excel "$Folder/1_Data/Romer-RomerFiscalData.xlsx", ///
	sheet("Other Variables") cellrange(A19:BN274) firstrow clear
	
**drop variables that are not captured

drop AA AB AC AD AE AF AG AH AI AJ AK AL AM AN AO AP AQ AR AS AT AU AV AW AX AY ///
	AZ BA BB BC BD BE BF BG BH BI BJ BK BL BM BN

	
**# Replace NAs to . (missing data)



foreach var of varlist DRCARA GDP PGDP PCGDP1 PCE DUR NONDUR SER GPDI FI ///
	NONRES RES EX IM ROMER RESID FYFF CM10 REP TE IP LCFC POIL {
		
		replace `var' = "." if `var' == "NA"
		
	}
	
**# Destring the variables


destring _all, replace

**# Generate time variables

rename A date
gen time = tq(1945q1) + _n-1
*** drop missing quarter data
drop if time >= 192

format time %tq
order time

tsset time

**# Merge with the other dataset

merge 1:1 time using "$Folder/1_Data/tax_measures_RR"
drop _merge

**# Save final dataset

save "$Folder/1_Data/dataset_RR", replace


***The variable relevant is PCGDP1 for GDP

*------------------------------------------------------------------------------*
**# Replicate Figure 4 of Romer and Romer (2010)
*------------------------------------------------------------------------------*

use "$Folder/1_Data/dataset_RR", clear


tsset time

*Compute to match Romer and Romer (2010)
gen EXOGER = DEFICNR + LONGRNR
gen EXOGERRATIO = (EXOGER/NOMGDP)*100
gen LNGDP  = log(GDP)
gen PCGDP1_new = (LNGDP - L.LNGDP)*100


gen dtax_old = D.EXOGERRATIO
gen dtax = D.EXOGENRRATIO

preserve
drop if time < -40 ///-40 is equivalent to 1950Q1

reg PCGDP1_new L(0/12).dtax
reg PCGDP1_new L(0/12).dtax L(1/12).PCGDP1_new

restore

* Choose impulse response horizon
local hmax = 13


* Cumulative
forvalues h = 0/`hmax' {
	gen PCGDP1_new`h' = (f`h'.LNGDP - l.LNGDP)*100
}


/* Run the LPs */

drop if time < -40 ///-40 is equivalent to 1950Q1

*------------------------------------------------------------------------------*
**# OLS
*------------------------------------------------------------------------------*

eststo clear
cap drop b u d u1 d1 Years Zero
gen Years = _n-1 if _n<=`hmax'
gen Zero =  0    if _n<=`hmax'
gen b=0
gen u=0
gen d=0
gen u1=0
gen d1=0
forv h = 0/`hmax' {

	 newey PCGDP1_new`h' dtax l(1/12).dtax, lag(`h')
	 

replace b = _b[dtax]                    if _n == `h'+1
replace u = _b[dtax] + 1* _se[dtax]  if _n == `h'+1
replace d = _b[dtax] - 1* _se[dtax]  if _n == `h'+1
replace u1 = _b[dtax] + 1.645* _se[dtax]  if _n == `h'+1
replace d1 = _b[dtax] - 1.645* _se[dtax]  if _n == `h'+1
eststo
}

twoway ///
	(rarea u d  Years,  ///
	fcolor(gs9%70) lcolor(gs9%70) lw(none) lpattern(solid)) ///
	(rarea u1 d1  Years,  ///
	fcolor(gs13%50) lcolor(gs13%50) lw(none) lpattern(solid)) ///
	(line b Years, lcolor(black) ///
	lpattern(solid) lwidth(mthick)), legend(off) ///
	title("(a) Impulse response of Tax Increase of 1 Percent of GDP on GDP", color(black) size(medsmall)) ///
	ytitle("Percent", size(medsmall)) ///
	subtitle("Romer and Romer (2010) shock", color(black) size(small)) ///
	xtitle("Horizon (Quarters)", size(medsmall)) ///
	xsc(r(0(1)12)) ///
	xlabel(0(1)12) ///
	yline(0, lcolor(black) lpattern("-")) ///
	graphregion(color(white)) plotregion(color(white))

gr rename fig_rr_ols, replace


*------------------------------------------------------------------------------*
**# IV
*------------------------------------------------------------------------------*

eststo clear
cap drop b u d u1 d1 Years Zero
gen Years = _n-1 if _n<=`hmax'
gen Zero =  0    if _n<=`hmax'
gen b=0
gen u=0
gen d=0
gen u1=0
gen d1=0
forv h = 0/`hmax' {

	 ivregress gmm PCGDP1_new`h' (DRCARA = dtax) l(1/12).dtax l(1/12).DRCARA, vce(hac nwest)
	 

replace b = _b[DRCARA]                    if _n == `h'+1
replace u = _b[DRCARA] + 1* _se[DRCARA]  if _n == `h'+1
replace d = _b[DRCARA] - 1* _se[DRCARA]  if _n == `h'+1
replace u1 = _b[DRCARA] + 1.645* _se[DRCARA]  if _n == `h'+1
replace d1 = _b[DRCARA] - 1.645* _se[DRCARA]  if _n == `h'+1
eststo
}

twoway ///
	(rarea u d  Years,  ///
	fcolor(gs9%70) lcolor(gs9%70) lw(none) lpattern(solid)) ///
	(rarea u1 d1  Years,  ///
	fcolor(gs13%50) lcolor(gs13%50) lw(none) lpattern(solid)) ///
	(line b Years, lcolor(black) ///
	lpattern(solid) lwidth(mthick)), legend(off) ///
	title("(b) Impulse response of Tax Increase of 1 Percent of GDP on GDP", color(black) size(medsmall)) ///
	ytitle("Percent", size(medsmall)) ///
	subtitle("Romer and Romer (2010) shocks", color(black) size(small)) ///
	xtitle("Horizon (Quarters)", size(medsmall)) ///
	xsc(r(0(1)12)) ///
	xlabel(0(1)12) ///
	yline(0, lcolor(black) lpattern("-")) ///
	graphregion(color(white)) plotregion(color(white))

gr rename fig_rr_iv, replace


*------------------------------------------------------------------------------*
**# Average Decomposition
*------------------------------------------------------------------------------*

preserve 

gen quarter_id = _n  // Create identifier for merging weights by quarter

forvalues i = 0/13 {
	
	* IV regression
	ivregress gmm PCGDP1_new`i' l(1/12).dtax l(1/12).DRCARA (DRCARA = dtax), vce(hac nwest)

	* FWL residualization
	reg PCGDP1_new`i' l(1/12).dtax l(1/12).DRCARA
	predict y_resid`i', resid

	reg DRCARA l(1/12).dtax l(1/12).DRCARA
	predict x_resid`i', resid

	reg dtax l(1/12).dtax l(1/12).DRCARA
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
                              rel_weight10 rel_weight11 rel_weight12 rel_weight13)
							 
twoway (tsline dtax, lcolor(red%30)), ///
		 legend(off) ///
		 ytitle("") ///
		 title("(a) Romer and Romer (2010) shock", color(black) size(med)) xtitle("Quarters") ///
		 name(rr10_shocks, replace)
		 
graph export "$Folder/3_Output/Graphs/Romers_2010/rr10_shocks.pdf", as(pdf) replace	

twoway (tsline rel_weight_avg, lcolor(blue%70)), ///
		 legend(off) ///
		 ytitle("") ///
		 title("(b) Weights for Output", color(black) size(med)) xtitle("Quarters") ///
		 name(avg_weight_output, replace)
		 
graph export "$Folder/3_Output/Graphs/Romers_2010/GDP_Weights.pdf", as(pdf) replace	

restore
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
**# Combine Graphs
*------------------------------------------------------------------------------*

gr combine fig_rr_ols fig_rr_iv, cols(2) altshrink ysize(2) imargin(medlarge)

gr rename irf_output_RR2010

graph export "$Folder/3_Output/Graphs/irf_output_RR2010.pdf", ///
	as(pdf) name("irf_output_RR2010") replace
	
gr combine rr10_shocks avg_weight_output, cols(2) altshrink ysize(2) imargin(medlarge)

gr rename weights_RR10

graph export "$Folder/3_Output/Graphs/weights_RR10.pdf", ///
	as(pdf) name("weights_RR10") replace

