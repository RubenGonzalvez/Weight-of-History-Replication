*==============================================================================*
* Title: Time-series simulation with LP-IV and weight decomposition
* Author: OpenAI ChatGPT
* Date: 2026-05-01
*==============================================================================*

*------------------------------*
* 1. Simulate X and Y (uncorrelated)
*------------------------------*
clear all 

set obs 1000

gen t = _n
tsset t

gen x = .
gen y = .

replace x = rnormal() in 1
replace y = rnormal() in 1

forvalues i = 2/`=_N' {
    replace x = 0.6 * x[`=`i'-1'] + rnormal() in `i'
    replace y = 0.6 * y[`=`i'-1'] + rnormal() in `i'
}

*------------------------------*
* 2. Simulate Z as an instrument for X
*    Make correlation depend on a single outlier
*------------------------------*

gen z = rnormal()

* Create a single influential observation
replace x = 30 in 100
replace z = 30 in 100
replace y = 30 in 100

*------------------------------*
* 3. Show correlation with and without the outlier
*------------------------------*

display "Correlation between X and Z with outlier"
pwcorr x z, sig

preserve
    display "Correlation between X and Z without outlier"
    pwcorr x z, sig
restore

* Optional: show first-stage regression for intuition

display "First-stage regression with outlier"
regress x z

preserve
	drop in 100
    display "First-stage regression without outlier"
    regress x z
restore

*------------------------------*
* 4. OLS check
*------------------------------*

display "OLS regression (full sample)"
regress y x

*-------------------------------------------------*
* 5. Local projection IV (LP-IV) with the Outlier
*-------------------------------------------------*

local hmax = 20

tempfile irf_data
tempname irf_post
postfile `irf_post' h irf irf_se using "`irf_data'", replace

forvalues h = 0/`hmax' {
    display "LP-IV horizon `h' with outlier"
    ivregress 2sls F`h'.y (x = z) L(1/4).y L(1/4).x
    post `irf_post' (`h') (_b[x]) (_se[x])
}

postclose `irf_post'

preserve
    use "`irf_data'", clear
    gen irf_ub = irf + 1.96 * irf_se
    gen irf_lb = irf - 1.96 * irf_se
    twoway ///
        (line irf_ub h, lcolor(red) lwidth(medthin) lpattern(dash)) ///
        (line irf_lb h, lcolor(red) lwidth(medthin) lpattern(dash)) ///
        (scatter irf h, c(l) clcolor(red) clwidth(medium) clpattern(solid) ///
        msymbol(circle) mcolor(red) msize(small)), ///
        title("") ///
        ytitle("Coefficient and 95% CI") xtitle("Horizon") ///
        graphregion(color(white)) legend(off) ///
        name(irf_plot, replace)
		
graph export "$Folder/3_Output/Graphs/simulation_IRF.pdf", as(pdf)  replace	

restore

*------------------------------*
* 6. IV regression with and without the outlier
*------------------------------*

local hmax = 20

preserve

drop in 100

tempfile irf_data
tempname irf_post
postfile `irf_post' h irf irf_se using "`irf_data'", replace

forvalues h = 0/`hmax' {
    display "LP-IV horizon `h' with outlier"
    ivregress 2sls F`h'.y (x = z) L(1/4).y L(1/4).x
    post `irf_post' (`h') (_b[x]) (_se[x])
}

postclose `irf_post'

restore

preserve
    use "`irf_data'", clear
    gen irf_ub = irf + 1.96 * irf_se
    gen irf_lb = irf - 1.96 * irf_se
    twoway ///
        (line irf_ub h, lcolor(red) lwidth(medthin) lpattern(dash)) ///
        (line irf_lb h, lcolor(red) lwidth(medthin) lpattern(dash)) ///
        (scatter irf h, c(l) clcolor(red) clwidth(medium) clpattern(solid) ///
        msymbol(circle) mcolor(red) msize(small)), ///
        title("") ///
        ytitle("Coefficient and 95% CI") xtitle("Horizon") ///
	    graphregion(color(white)) legend(off) ///
        name(irf_plot, replace)
		
graph export "$Folder/3_Output/Graphs/simulation_IRF_Drop100.pdf", as(pdf)  replace	
		
restore

*------------------------------*
* 7. Weight decomposition (LP-IV average over 0-20)
*------------------------------*

preserve

    gen quarter_id = _n

    forvalues i = 0/20 {
        ivregress 2sls F`i'.y (x = z) L(1/4).y L(1/4).x

        reg F`i'.y L(1/4).y L(1/4).x
        predict y_resid`i', resid

        reg x L(1/4).y L(1/4).x
        predict x_resid`i', resid

        reg z L(1/4).y L(1/4).x
        predict z_resid`i', resid

        reg x_resid`i' z_resid`i', nocons
        predict xhat_resid`i', xb

        reg y_resid`i' xhat_resid`i', nocons
        scalar beta_hat`i' = _b[xhat_resid`i']

        gen contrib`i' = xhat_resid`i' * y_resid`i'
        egen total_contrib`i' = total(contrib`i')
        gen rel_weight`i' = contrib`i' / total_contrib`i'
    }

    egen rel_weight_avg = rowmean(rel_weight0 rel_weight1 rel_weight2 rel_weight3 rel_weight4 ///
                                  rel_weight5 rel_weight6 rel_weight7 rel_weight8 rel_weight9 ///
                                  rel_weight10 rel_weight11 rel_weight12 rel_weight13 rel_weight14 ///
                                  rel_weight15 rel_weight16 rel_weight17 rel_weight18 rel_weight19 rel_weight20)

    tsline rel_weight_avg, ///
        title("") ///
        ytitle("Average Relative Weight") xtitle("t") ///
        name(weight_plot, replace)
		
	graph export "$Folder/3_Output/Graphs/Simulation_Decomposition.pdf", as(pdf)  replace	
		
restore