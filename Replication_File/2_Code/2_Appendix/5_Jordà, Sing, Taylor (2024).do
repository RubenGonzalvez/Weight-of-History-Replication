*********************************************
**** PREAMBLE  ****
*********************************************
clear all

*********************************************
**** SET PATHS  ****
*********************************************
* Create Path Directories for Storing Results
* set the LP plot horizon
 
global horizon 12
*********************************************
**** DATA  ****

clear all

use "$Folder/1_Data/main_data_JST24.dta", clear

*********************************************
**** SET PARAMETERS  ****
*********************************************

*** increase the plot horizon 
	local horizon $horizon

*** title
	global title 

*** add any extra variables to control set?
	global fe dlsumgdp

*** drop any observations?
	cap drop when* 
	gen when=0
	
*** Set periods 

	* Sample period years
	global c1  (year>1899 & year<=1912 & when==0  ) | (year>1918 & year<=1938 & when==0  ) | ( year>1946 & when==0)  
	global c2 year >  1946 & when==0
	global c3  (year>1899 & year<=1912 & when==0  ) | (year>1918 & year<=1938 & when==0  ) |  (year>1946 & year<=2007 & when==0) 
	global c4 year >  1946 & year<=2007 & when==0

	* Sample period names
	global p1 full
	global p2 post
	global p3 full_noGR
	global p4 post_noGR

	* Sample period titles
	global t1 "Full sample"
	global t2 "Post-WW2"
	global t3 "1900 - 2007"
	global t4 "1947 - 2007"
	
*** key vars LHS & RHS

	* y -- LHS -- ! if changing this don't forget to adjust controls below
	global response lrgdp 
	
	* z 
	global instr_OLSIV dibpegF 

	* x
	global impulse stir
	
	* controls for LHS stir
	cap drop sevs
	gen sevs = cond(year >= 1973 & year <= 1980, 1, 0)
	* this conditioning fixes price puzzle in JST (JME 2019
	
	foreach v in  dlrcon dlrgdp dlcpi dlriy dstir dltrate dloansgdp dlhpreal dlspreal{
		cap drop `v'1 `v'2
		gen `v'1=`v'*sevs
		gen `v'2=`v'*(1-sevs)
		}

	foreach v in   dlrcon  dlrgdp dlcpi dlriy dstir dltrate dloansgdp dlhpreal dlspreal{
		forvalues i=1/2{
			
			cap drop l`v'1 l`v'2 
			cap drop l2`v'1 l2`v'2
			gen l`v'1 = l.`v'1
			gen l2`v'1 = l2.`v'1
			gen l`v'2 = l.`v'2
			gen l2`v'2 = l2.`v'2
			
			local lags`v' l`v'1 l2`v'1 l`v'2 l2`v'2
			
			}
		}

	* define the control set
	global rhslrgdp  dlrcon1 dlrcon2 `lagsdlrcon' ///
									`lagsdlrgdp' ///
					  dlcpi1 dlcpi2 `lagsdlcpi' ///
					  dlriy1 dlriy2 `lagsdlriy' ///
					  `lagsdstir' dltrate1 dltrate2 `lagsdltrate' ///
					  dloansgdp1 dloansgdp2 `lagsdloansgdp' ///
					  dlhpreal1 dlhpreal2 `lagsdlhpreal' ///
					  dlspreal1 dlspreal2 `lagsdlspreal'
					  
	global rhsstir    	dlrcon1 dlrcon2 `lagsdlrcon'  ///
						 dlrgdp1 dlrgdp2 `lagsdlrgdp' ///
						dlcpi1 dlcpi2 `lagsdlcpi' ///
						dlriy1 dlriy2 `lagsdlriy'  ///
						`lagsdstir' `lagsdltrate' ///
						dloansgdp1 dloansgdp2 `lagsdloansgdp' ///
						dlhpreal1 dlhpreal2 `lagsdlhpreal' ///
						dlspreal1 dlspreal2 `lagsdlspreal' 

	global rhslcpi      dlrcon1 dlrcon2 `lagsdlrcon'  ///
						  dlrgdp1 dlrgdp2 `lagsdlrgdp' /// 
							`lagsdlcpi' ///
						dlriy1 dlriy2 `lagsdlriy' `lagsdstir' ///
						dltrate1 dltrate2 `lagsdltrate' ///
						dloansgdp1 dloansgdp2 `lagsdloansgdp' ///
						dlhpreal1 dlhpreal2 `lagsdlhpreal' ///
						dlspreal1 dlspreal2 `lagsdlspreal' 	
	
					
* SPECIFY the standard error calculation method
 global semethod dkraay(4) // cluster(ccode) for Cluster
 * SPECIFY the wider error band for plotting
 global errorband 1.96 // 1.645	for 90%, 1.96 for 95%, or 2	
 
 
 
 * Figure 1: PLOT IV
 set more off
 	preserve
	gen GBR = dibaseF if peg_base=="GBR"
	gen USA = dibaseF if peg_base=="USA"
	gen DEU = dibaseF if peg_base=="DEU"
	gen HYBRID = dibaseF if peg_base=="HYBRID"
	collapse GBR USA DEU HYBRID , by(year)
	drop if year<1900
	line GBR USA DEU HYBRID year , cmiss(no) ///
		lc(blue red black green) lp(longdash solid shortdash dash_dot) lw(medthick..) ///
		ytit("Residualized base country change in interest rate") xtit("") ///
		xlab(1900(20)2020) xsize(6) ysize(3) plotreg(lp(blank)) ///
		legend(ring(0) pos(11) col(1) reg(col(none)) ///
			order(1 "United Kingdom" 4 "Hybrid (U.K., U.S., France)" 2 "United States" 3 "Germany" ))
	restore

	gr export "$Folder/3_Output/Graphs/JST_2024/Fig_1.pdf" , replace
	

* 13. Figures 2a, 2b + Online Appendix for 3 variable system (A.13, A.14)
* 		NOTE: The control variables are changed inside this file. 
	

	
foreach v in   dlrcon  dlrgdp dlcpi dlriy dstir dltrate dloansgdp dlhpreal dlspreal{
			
				local lags`v' l`v'1 l2`v'1 l`v'2 l2`v'2
			
}
			
	*** 3 var here	
	global rhslrgdp  	`lagsdlrgdp' ///
					  dlcpi1 dlcpi2  `lagsdlcpi' ///
					  `lagsdstir'  
					  
					 				  		
 
* force sort tsset again
sort ccode year
tsset ccode year, yearly

				


****************** LPs ******************************************************
*****************************************************************************


local semethod $semethod
** increase the plot horizon 
local horizon $horizon

* extra variables
local fe dlsumgdp

* x
local impulse $impulse
* y 
local response lrgdp ///ltfp ltfp_jst  lNEmp lK lutil_tfp lcomp_gap lKadj lLadj 


*instrument
local instr $instr_OLSIV

* controls for LHS stir
local rhslrgdp $rhslrgdp


cap drop dstir
gen dstir = d.stir 

cap drop l1dlsumgdp  l1dlgdpbaseF
	gen l1dlsumgdp = l.dlsumgdp
	gen l1dlgdpbaseF = l.dlgdpbaseF

cap drop l2dlsumgdp l2dlgdpbaseF 	
	gen l2dlsumgdp = l2.dlsumgdp
	gen l2dlgdpbaseF = l2.dlgdpbaseF



cap drop Years

gen Years = _n-1 if _n <= `horizon'+1




*** Sample period years (full-sample and post-ww2)
local c1 $c1
local c2 $c2


local semethod $semethod

** increase the plot horizon 
local horizon $horizon
* extra variables
local fe dlsumgdp
* x
local impulse $impulse
* y 
local response $response
* controls for LHS stir
local rhslrgdp    $rhslrgdp

foreach y of local response {

	* controls enter 
	local cont2`y' `rhs`y'' `fe'
	

}

forvalues i=0/`horizon' {
	foreach y of local response {
	
	cap drop `y'`i'
	gen `y'`i' = (f`i'.`y' - l.`y') 

	
	}
}

cap drop dstir
gen dstir = d.stir 

cap drop l1dlsumgdp  l1dlgdpbaseF
	gen l1dlsumgdp = l.dlsumgdp
	gen l1dlgdpbaseF = l.dlgdpbaseF

cap drop l2dlsumgdp l2dlgdpbaseF 	
	gen l2dlsumgdp = l2.dlsumgdp
	gen l2dlgdpbaseF = l2.dlgdpbaseF
	
foreach y of local response {
				
local globalcycle  l1dlsumgdp l2dlsumgdp dlsumgdp
local basecycle l1dlgdpbaseF l2dlgdpbaseF dlgdpbaseF

local rhsvars `cont2`y'' `globalcycle'  `basecycle' 
cap drop _peg _float
gen _peg = cond( (peg * l.peg)==1,1,0)
gen _float = 1-_peg
cap drop ___*
	foreach v in `rhsvars'	{
		cap drop ___`v'_peg
		cap drop ___`v'_float
		gen ___`v'_peg=`v'*_peg
		gen ___`v'_float=`v'*_float
	}	
}

cap drop b*_iv se*_iv u*_iv d*_iv u*_iv1 d*_iv1
gen b1_iv = 0
gen se1_iv = 0
gen u1_iv=0
gen d1_iv=0
gen u1_iv1=0
gen d1_iv1=0

gen b2_iv = 0
gen se2_iv = 0
gen u2_iv=0
gen d2_iv=0
gen u2_iv1=0
gen d2_iv1=0


forvalues i=0/`horizon' {
	
			
	* IV 
	ivreghdfe lrgdp`i' (dstir =  ivpeg) ___* ///
		if (year>1899 & year<=1912 & when==0) | (year>1918 & year<=1938 & when==0) | ( year>1946 		& when==0)   & coresample==1 ,  absorb(ccode)  dkraay(4) robust ///
		partial(___*)		
					 
				replace b1_iv = _b[dstir] if _n==`i'+1
				replace se1_iv = _se[dstir] if _n==`i'+1
				replace u1_iv = _b[dstir] + 1* _se[dstir]  if _n == `i'+1
				replace d1_iv = _b[dstir] - 1* _se[dstir]  if _n == `i'+1
				replace u1_iv1 = _b[dstir] + 1.645* _se[dstir]  if _n == `i'+1
				replace d1_iv1 = _b[dstir] - 1.645* _se[dstir]  if _n == `i'+1
	
				*Average Decomposition
				* FWL residualization
			ivreghdfe lrgdp`i' ___* ///
				if (year>1899 & year<=1912 & when==0) | (year>1918 & year<=1938 & when==0) ///
				| ( year>1946 & when==0) & coresample==1, absorb(ccode) dkraay(4) ///
				robust residuals(y_resid`i')

				ivreghdfe dstir ___* ///
				if (year>1899 & year<=1912 & when==0) | (year>1918 & year<=1938 & when==0) ///
				| ( year>1946 & when==0) & coresample==1, absorb(ccode) dkraay(4) ///
				robust residuals(x_resid`i')

				ivreghdfe ivpeg ___* ///
				if (year>1899 & year<=1912 & when==0) | (year>1918 & year<=1938 & when==0) ///
				| ( year>1946 & when==0) & coresample==1, absorb(ccode) dkraay(4) ///
				robust residuals(z_resid`i')

				* First stage: x_hat
				xtreg x_resid`i' z_resid`i', fe
				predict xhat_resid`i', xb

				* Second stage
				xtreg y_resid`i' xhat_resid`i', fe
				scalar beta_hat`i' = _b[xhat_resid`i']
	
				* Contribution
				gen contrib`i' = xhat_resid`i' * y_resid`i'
				egen total_contrib`i' = total(contrib`i')
				gen rel_weight`i' = contrib`i' / total_contrib`i'
		
}



forvalues i=0/`horizon' {
	
		* IV 
		ivreghdfe lrgdp`i' (dstir =  ivpeg) ___* ///
			if year > 1946 & when==0 & coresample==1 ,  absorb(ccode) dkraay(4) robust ///
			partial(___*)
					 
				replace b2_iv = _b[dstir] if _n==`i'+1
				replace se2_iv = _se[dstir] if _n==`i'+1
				replace u2_iv = _b[dstir] + 1* _se[dstir]  if _n == `i'+1
				replace d2_iv = _b[dstir] - 1* _se[dstir]  if _n == `i'+1
				replace u2_iv1 = _b[dstir] + 1.645* _se[dstir]  if _n == `i'+1
				replace d2_iv1 = _b[dstir] - 1.645* _se[dstir]  if _n == `i'+1
				
				*Average Decomposition
				
				* FWL residualization
				ivreghdfe lrgdp`i' ___* ///
				if year > 1946 & when==0 & coresample==1 , absorb(ccode) dkraay(4) ///
				robust residuals(y_resid`i'_post)

				ivreghdfe dstir ___* ///
				if year > 1946 & when==0 & coresample==1 , absorb(ccode) dkraay(4) ///
				robust residuals(x_resid`i'_post)

				ivreghdfe ivpeg ___* ///
				if year > 1946 & when==0 & coresample==1 , absorb(ccode) dkraay(4) ///
				robust residuals(z_resid`i'_post)

				* First stage: x_hat
				xtreg x_resid`i'_post z_resid`i'_post, fe
				predict xhat_resid`i'_post, xb

				* Second stage
				xtreg y_resid`i'_post xhat_resid`i'_post, fe
				scalar beta_hat`i'_post = _b[xhat_resid`i'_post]
	
				* Contribution
				gen contrib`i'_post = xhat_resid`i'_post * y_resid`i'_post
				egen total_contrib`i'_post = total(contrib`i'_post)
				gen rel_weight`i'_post = contrib`i'_post / total_contrib`i'_post
}
			


*Instrument drop: ivfloat




****************************************************************
*** graphs
****************************************************************


		twoway (rarea u1_iv1 d1_iv1  Years if Years<=12,  ///
		fcolor(gs13%50) lcolor(gs1z3%50) lw(none) lpattern(solid))  ///
		(rarea u1_iv d1_iv Years if Years<=12,  ///
		fcolor(gs9%70) lcolor(gs9%70) lw(none) lpattern(solid)) ///
		(line b1_iv Years if Years<=12, lcolor(black) ///
		lpattern(solid) lwidth(mthick)), ///
		yline(0, lcolor(black) lpattern("-")) ///
		title("(a) Full sample: 1900–2015. With 3 domestic controls", ///
		color(black) size(medium)) ///
		ytitle("Percent", size(medsmall)) xtitle("Horizon (Years)", size(medsmall)) ///
		graphregion(color(white)) plotregion(color(white)) ///
		legend(off) ///		
		name(lrgdp_full, replace) ylab(-8(2)2) xlab(0 4 8 12) xsc(r(0 12)) 
		
		* post ww2
		twoway (rarea u2_iv1 d2_iv1  Years if Years<=12,  ///
		fcolor(gs13%50) lcolor(gs13%50) lw(none) lpattern(solid))  ///
		(rarea u2_iv d2_iv Years if Years<=12,  ///
		fcolor(gs9%70) lcolor(gs9%70) lw(none) lpattern(solid)) ///
		(line b2_iv Years if Years<=12, lcolor(black) ///
		lpattern(solid) lwidth(mthick)), ///
		yline(0, lcolor(black) lpattern("-")) ///
		title("(b) Post-WW2 sample: 1948–2015. With 3 domestic controls", ///
		color(black) size(medium)) ///
		ytitle("Percent", size(medsmall)) xtitle("Horizon (Yeas)", size(medsmall)) ///
		graphregion(color(white)) plotregion(color(white)) ///
		legend(off) ///		
		name(lrgdp_postww, replace) ylab(-8(2)2) xlab(0 4 8 12) xsc(r(0 12)) 
		
	egen rel_weight_avg = rowmean(rel_weight0 rel_weight1 rel_weight2 rel_weight3 rel_weight4 ///
        rel_weight5 rel_weight6 rel_weight7 rel_weight8 rel_weight9 ///
        rel_weight10 rel_weight11 rel_weight12)
							  
	egen rel_weight_avg_post = rowmean(rel_weight0_post rel_weight1_post ///
		rel_weight2_post rel_weight3_post rel_weight4_post rel_weight5_post ///
		rel_weight6_post rel_weight7_post rel_weight8_post rel_weight9_post ///
		rel_weight10_post rel_weight11_post rel_weight12_post)
		
drop if country == "USA"

twoway(line rel_weight_avg year if year>=1900, cmiss(no) color(blue)), ///
	legend(off) xtitle("Years") xlabel(1900(20)2015) ///
	ytitle("") ///
	by(country, note("") iyaxes ixaxes)	
gr rename avg_weight, replace
graph export "$Folder/3_Output/Graphs/JST_2024/avg_weight.pdf", as(pdf)  replace	
twoway(line rel_weight_avg_post year if year>=1950, cmiss(no) color(blue)), ///
	legend(off) xtitle("Years") xlabel(1950(20)2015) ///
	ytitle("") ///
	by(country, note("") iyaxes ixaxes)
	
gr rename avg_weight_post, replace
		 

graph export "$Folder/3_Output/Graphs/JST_2024/avg_weight_post.pdf", as(pdf)  replace	
		

* Replication Figure 2A and 2B
gr combine lrgdp_full lrgdp_postww, cols(2) altshrink ysize(2.5) imargin(medlarge)
gr rename irf_output_JST24, replace

graph save   "$Folder/3_Output/Graphs/JST_2024/irf_output_JST24.gph" , replace
graph export "$Folder/3_Output/Graphs/JST_2024/irf_output_JST24.pdf", as(pdf) replace	


gr combine avg_weight avg_weight_post, cols(2) altshrink ysize(2.5) imargin(medlarge)

gr rename weights_JST24, replace
graph export "$Folder/3_Output/Graphs/JST_2024/weights_JST24.pdf", as(pdf) replace	




	