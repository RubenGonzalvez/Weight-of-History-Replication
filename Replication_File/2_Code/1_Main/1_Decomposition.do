
*==============================================================================*
* Title: Ramey Weights decompsition
* Author: Rubén Gonzálvez & Daniel F. Romero
* based on replication codes by Ramey and Zubairy
* Date: 5/1/2026
*==============================================================================*

*------------------------------------------------------------------------------*
**# Directories and preamble: 
*------------------------------------------------------------------------------*

drop _all
clear all

set more 1
set matsize 800

capture log close
*log using jordagkirfs_results.log, replace;

set scheme Modern

/*******************************************************************************
  SET PARAMETERS THAT GOVERN SPECIFICATION
*******************************************************************************/

local sample = 1  /*1 = full sample, 2 = post-WWII */

local omit nomit  /*either nomit(don't omit subsample) or wwii (omit WWII) */

local state slack  /* slack or zlb or recession or ag*/

local shock newsy /* shock identification: either newsy or bp */

local p = 4 /*number of lags of control variables*/

local trends = 0 /*0 = no trends, 1 = trends */ // Trends=0 para que sea tabla 1. 

local tax = 0 /*0 = exclude taxes, 1 = include taxes */


*******************************************************************************;
** RAW DATA IMPORTATION AND DATA SETUP;
*******************************************************************************;
import excel "$Folder/1_Data/rzdat.xlsx", sheet("rzdat") firstrow


*insheet using rzdatnew.csv;

drop if quarter<1889

*keep if quarter>=1952

gen qdate = q(1889q1) + _n-1
tsset qdate, q

/* World War II rationing sample.  Start in 1941q3 because of constraints on
     auto production.  Most rationing ended in Aug/Sept 1945, a few items in
	 Nov/Dec 1945 */

gen wwii = quarter>=1941.5 & quarter<1946

gen nomit = 0  /* indicator for no omit */


*** DEFINE QUARTIC TREND;

gen t = _n
gen t2 = t^2
gen t3 = t^3
gen t4 = t^4

*** DEFINE STATE VARIABLE;

gen slack = unemp >= 6.5  /* unemployment state with fixed threshold */
gen slack8 = unemp>=8
gen slackhp = unemp>=hpunemp_split  /* unemployment state with hp threshold */

gen zlb = zlb_dummy  /*  zlb state */
gen zlb5 = tbill<=0.5 /*tbill rate less than 0.5*/
gen both =unemp >= 6.5 & zlb_dummy>=1 /* consider slack and ZLB state together*/

*** NORMALIZATION;

/* choice of potential GDP for normalization:

   rgdp_potCBO (cubic trend early, CBO late) or rgdp_pott6 (6th degree for full),
   both fitted excluding Great Depression, WWII:  quarter>=1930 & quarter<1947*/

local ynorm rgdp_pott6 /* rgdp_pott6 or rgdp_potcbo */

* BASIC VARIABLES;

gen newsy = news/(L.`ynorm'*L.pgdp)
gen rgov = ngov/pgdp
gen rtax = nfedcurrreceipts_nipa/pgdp
gen taxy = nfedcurrreceipts_nipa/ngdp
gen debty = pubfeddebt_treas/L.ngdp
gen lpgdp = ln(pgdp)
gen ly = ln(rgdp)

gen infl = 400*D.lpgdp

* normalize variables and shorten names;

gen y = rgdp/`ynorm'
gen g = rgov/`ynorm'
 
gen bp = g /* Blanchard-Perotti shock is just orthogonalized current g */

*** AG DEFINITION OF STATE:  ag = 1 is extreme recession, ag = 0 is extreme expansion;

gen z = 100*(F3.ly - L4.ly)/7  /* AG definition of state */

*The mean of z is approx. 0.8 and std is 0.5.  AG specically use those numbers so we do too;

gen znorm = (z - 0.8)/0.5

gen fznorm = exp(-1.5*znorm)/(1 + exp(-1.5*znorm))

gen ag = fznorm

*******************************************************************************;
** CUMULATIVE VARIABLES;
*******************************************************************************;

gen cumuly = 0
gen cumulg = 0
 
forvalues i = 0/20 {

   gen f`i'cumuly = F`i'.y + cumuly
   gen f`i'cumulg = F`i'.g + cumulg
   
   gen recf`i'cumulg = f`i'cumulg*L.`state'
   gen expf`i'cumulg = f`i'cumulg*(1-L.`state')
   
   replace cumuly = f`i'cumuly
   replace cumulg = f`i'cumulg
   
}


*******************************************************************************;
**  INTERACTION OF SHOCKS WITH STATE;
*******************************************************************************;

 foreach var in newsy bp { 
 
   gen rec0`var' = `var'*L.`state'
   gen exp0`var' = `var'*(1-L.`state')
 
 }

*******************************************************************************;
** CREATE LISTS;
*******************************************************************************;

   if `sample'==1 {

         gen h = t - 1  /* h is the horizon for the irfs */
         global trendlist t t2 t3 t4
   }

    else {
         drop if quarter<1947
         gen h = t - 232 - 1
         global trendlist t t2
     }
	 
forvalues i = 1/`p' {

  foreach var in newsy y g taxy debty infl{

    gen rec`var'`i' = L`i'.`var'*L.`state'
    gen exp`var'`i' = L`i'.`var'*(1-L.`state')
 
  }
}

  if `trends'==0 {
  
    if `tax'==0 {
  
      global newsylinxlist L(1/`p').newsy L(1/`p').y L(1/`p').g 
      global bplinxlist L(1/`p').y L(1/`p').g 
	  global newsynlxlist L.`state' recnewsy? expnewsy? recy? expy? recg? expg? 
      global bpnlxlist L.`state' recy? expy? recg? expg? 
	
	}
	
	else {
	
	  global newsylinxlist L(1/`p').newsy L(1/`p').y L(1/`p').g L(1/`p').taxy /*L(1/`p').infl;*/
      global bplinxlist L(1/`p').y L(1/`p').g L(1/`p').taxy /* L(1/`p').infl;*/
	  global newsynlxlist L.`state' recnewsy? expnewsy? recy? expy? recg? expg? rectaxy? exptaxy? /* expinfl? recinfl?;*/
      global bpnlxlist L.`state' recy? expy? recg? expg? rectaxy? exptaxy? /* expinfl? recinfl?;*/
	
    }
  }
  
  else {
  
    if `tax'==0 {
	
      global newsylinxlist L(1/`p').newsy L(1/`p').y L(1/`p').g $trendlist
      global bplinxlist L(1/`p').y L(1/`p').g $trendlist
      global newsynlxlist L.`state' recnewsy? expnewsy? recy? expy? recg? expg? $trendlist
      global bpnlxlist L.`state' recy? expy? recg? expg? $trendlist
	
	}
	
	else {
	
	global newsylinxlist L(1/`p').newsy L(1/`p').y L(1/`p').g L(1/`p').taxy $trendlist
    global bplinxlist L(1/`p').y L(1/`p').g L(1/`p').taxy $trendlist
	global newsynlxlist L.`state' recnewsy? expnewsy? recy? expy? recg? expg? rectaxy? exptaxy? $trendlist
    global bpnlxlist L.`state' recy? expy? recg? expg? rectaxy? exptaxy? $trendlist
	
    }
	
}


global newsylinshock newsy
global newsynlshock rec0newsy exp0newsy

global bplinshock bp
global bpnlshock rec0bp exp0bp


** INITIALIZE SUM OF EFFECTS TO 0 AND PARAMETERS SERIES TO MISSING;

gen sumliny = 0 
gen sumling = 0
gen sumexpy = 0 
gen sumexpg = 0
gen sumrecy = 0 
gen sumrecg = 0

foreach var in bylin byexp byrec bglin bgexp bgrec up95bylin up95byexp up95byrec up95bglin up95bgexp up95bgrec ///
  lo95bylin lo95byexp lo95byrec lo95bglin lo95bgexp lo95bgrec seylin seyexp seyrec seglin segexp segrec ///
  multlin multexp multrec {
  
  quietly gen `var' = .
  
}


*******************************************************************************;
** ESTIMATION OF IRFS
*******************************************************************************;

preserve 

ivreg2 F0.y L(0/4).newsy L(1/4).y  L(1/4).g t t2 t3 t4, robust bw(auto)

reg F0.y L(0/4).newsy L(1/4).y  L(1/4).g t t2 t3 t4 

* Step 1: Residualize F0.y
reg F0.y L1.newsy L2.newsy L3.newsy L4.newsy ///
         L(1/4).y L(1/4).g t t2 t3 t4
		 
predict y_resid, resid

* Step 2: Residualize L0.newsy
reg L0.newsy L1.newsy L2.newsy L3.newsy L4.newsy ///
             L(1/4).y L(1/4).g t t2 t3 t4
predict x_resid, resid

* Step 3: Get regression coefficient
reg y_resid x_resid, nocons
scalar beta_hat = _b[x_resid]

* Step 4: Compute raw contributions
gen contrib = x_resid * y_resid

* Step 5: Scale to sum to 1
egen total_contrib = total(contrib)
gen rel_weight = contrib / total_contrib

* Step 6: Confirm it sums to 1
sum rel_weight
di "Sum of weights (should be 1): " r(sum)

* Optional: check it reproduces beta
gen beta_from_weights = rel_weight * beta_hat
sum beta_from_weights
di "Sum of weighted beta parts: " r(sum)
di "Original beta: " beta_hat

tsline beta_from_weights, ///
    title("Contribution to β on L0.newsy by quarter") ///
    ytitle("Contribution to β") xtitle("Quarter") ///
    name(weight_plot, replace)
	
	
tsline rel_weight, ///
    title("Contribution to β on L0.newsy by quarter") ///
    ytitle("Contribution to β") xtitle("Quarter") ///
    name(weight_plot, replace)

	
restore



forvalues i = 0/0 { /*Must treat horizon = 0 different in case the shock is BP*/

  ivreg2 F`i'.y L(0/4).newsy L(1/4).y  L(1/4).g t t2 t3 t4, robust bw(auto)

  gen bylinh`i' = _b[newsy]
  
  gen seylinh`i' = _se[newsy]
  
  ivreg2 F`i'.y exp0newsy rec0newsy L.slack recnewsy* expnewsy*  recy* expy* recg* expg* t t2 t3 t4, robust bw(auto)

  gen byexph`i' = _b[exp0newsy]
  gen byrech`i' = _b[rec0newsy]
  
  gen seyexph`i' = _se[exp0newsy]
  gen seyrech`i' = _se[rec0newsy]
  
  

  ivreg2 F`i'.g L(0/4).newsy L(1/4).y  L(1/4).g t t2 t3 t4, robust bw(auto)

  gen bglinh`i' = _b[newsy]
  gen seglinh`i' = _se[newsy]

  ivreg2 F`i'.g exp0newsy rec0newsy L.slack recnewsy* expnewsy*  recy* expy* recg* expg* t t2 t3 t4, robust bw(auto)	

  gen bgexph`i' = _b[exp0newsy] 
  gen bgrech`i' = _b[rec0newsy]
  
  gen segexph`i' = _se[exp0newsy]
  gen segrech`i' = _se[rec0newsy] 


replace sumliny = bylinh`i' + sumliny
  replace sumling = bglinh`i' + sumling
  
  replace sumexpy = byexph`i' + sumexpy
  replace sumexpg = bgexph`i' + sumexpg
  
  replace sumrecy = byrech`i' + sumrecy
  replace sumrecg = bgrech`i' + sumrecg
  
   gen multlinh`i' = sumliny/sumling
   gen multexph`i' = sumexpy/sumexpg
   gen multrech`i' = sumrecy/sumrecg
  
  foreach var in bylin byexp byrec bglin bgexp bgrec multlin multexp multrec {
  
    quietly replace `var' = `var'h`i' if h==`i'
	
  }
  
  foreach var in ylin glin yexp gexp yrec grec {
  
    quietly replace up95b`var' = b`var'h`i' + 1.96*se`var'h`i' if h==`i'
	quietly replace lo95b`var' = b`var'h`i' - 1.96*se`var'h`i' if h==`i'
	
  }

}


forvalues i = 1/20 {


ivreg2 F`i'.y L(0/4).newsy L(1/4).y  L(1/4).g t t2 t3 t4, robust bw(auto)

  gen bylinh`i' = _b[newsy]
  gen seylinh`i' = _se[newsy]
  
ivreg2 F`i'.g L(0/4).newsy L(1/4).y  L(1/4).g t t2 t3 t4, robust bw(auto)

  gen bglinh`i' = _b[newsy]
  gen seglinh`i' = _se[newsy]

ivreg2 F`i'.y exp0newsy rec0newsy L.slack recnewsy* expnewsy*  recy* expy* recg* expg* t t2 t3 t4, robust bw(auto)

  gen byexph`i' = _b[exp0newsy]
  gen byrech`i' = _b[rec0newsy]
  
  gen seyexph`i' = _se[exp0newsy]
  gen seyrech`i' = _se[rec0newsy]

ivreg2 F`i'.g exp0newsy rec0newsy L.slack recnewsy* expnewsy*  recy* expy* recg* expg* t t2 t3 t4, robust bw(auto)

  gen bgexph`i' = _b[exp0newsy]
  gen bgrech`i' = _b[rec0newsy]
  
  gen segexph`i' = _se[exp0newsy]
  gen segrech`i' = _se[rec0newsy]
  
  
  replace sumliny = bylinh`i' + sumliny
  replace sumling = bglinh`i' + sumling
  
  replace sumexpy = byexph`i' + sumexpy
  replace sumexpg = bgexph`i' + sumexpg
  
  replace sumrecy = byrech`i' + sumrecy
  replace sumrecg = bgrech`i' + sumrecg
  
   gen multlinh`i' = sumliny/sumling
   gen multexph`i' = sumexpy/sumexpg
   gen multrech`i' = sumrecy/sumrecg
  
  foreach var in bylin byexp byrec bglin bgexp bgrec multlin multexp multrec seyexp seyrec segexp segrec {
  
    quietly replace `var' = `var'h`i' if h==`i'
	
  }
  
  foreach var in ylin glin yexp gexp yrec grec {
  
    quietly replace up95b`var' = b`var'h`i' + 1.96*se`var'h`i' if h==`i'
	quietly replace lo95b`var' = b`var'h`i' - 1.96*se`var'h`i' if h==`i'
	
  }

  
}



display as text "MULTIPLIERS:  2 STEP"

rename multlin multlin2
rename multexp multexp2
rename multrec multrec2

/*
*outsheet h multlin2 multexp2 multrec2 using junk2step.csv if h<=20, comma replace

*outsheet h byexp byrec bgexp bgrec seyexp seyrec segexp segrec using junk2stepirfs.csv if h<=20, comma replace

label var bglin "Gov, linear model"
label var bylin "GDP, linear model"
label var bgexp "GOV, expansion"
label var byexp "GDP, expansion"
label var bgrec "Gov, recession"
label var byrec "GDP, recession"


tw (rarea up95bglin lo95bglin h, bcolor(gs12) clw(medthin medthin)) ///
  (scatter bglin h, c(l ) clp(l ) ms(i ) clc(black) mc(black) clw(medthick)) if h<=20, ///
  saving(junkg.gph,replace)

tw (rarea up95bylin lo95bylin h, bcolor(gs12) clw(medthin medthin)) ///
  (scatter bylin h, c(l ) clp(l ) ms(i ) clc(black) mc(black) clw(medthick)) if h<=20, ///
  saving(junky.gph,replace)
  
  
 tw (rarea up95bgrec lo95bgrec h, bcolor(gs12) clw(medthin medthin)) ///
    (scatter up95bgexp bgexp lo95bgexp bgrec h, clw(medthin medthick medthin medthick) ///
  c(l l l l l) clp(- l - l) clc(red red red blue ) ms(i o i i i i) mc(red red red blue)) if h<=20, ///
  saving(junkgnl.gph,replace)

tw (rarea up95byrec lo95byrec h, bcolor(gs12) clw(medthin medthin)) ///
    (scatter up95byexp byexp lo95byexp byrec h,  clw(medthin medthick medthin medthick) ///
  c(l l l l l) clp(- l - l) clc(red red red blue ) ms(i o i i i i) mc(red red red blue)) if h<=20, ///
  saving(junkynl.gph,replace)
  
graph combine junkg.gph junky.gph junkgnl.gph junkynl.gph, col(2) iscale(0.5) 
gr rename irf_ramey, replace

*/


*****************************************************************************;

drop mult???h*

drop sey*


foreach var in multlin1 multexp1 multrec1 Fkplin Fkpexp Fkprec seylin seyexp seyrec ptestdiff Fdifflin Fdiffexp Fdiffrec{
  
  quietly gen `var' = .
  
}


*******************************************************************************;
** ESTIMATION OF CUMULATIVE;
*******************************************************************************;

**# Cleaner IRF

ivreg2 f0cumuly (f0cumulg = newsy) L(1/4).newsy L(1/4).y L(1/4).g t t2 t3 t4, robust bw(auto)


forvalues i=0/20 {

	ivreg2 f`i'cumuly (f`i'cumulg = newsy) L(1/4).newsy L(1/4).y L(1/4).g t t2 t3 t4, robust bw(auto)
	capture drop Fkplinh`i' Fdifflinh`i' multlinh`i' seylinh`i'
	gen Fkplinh`i'= e(widstat) /* Kleibergen-Paap rk Wald F statistic*/
	gen Fdifflinh`i'= Fkplinh`i'- 23.1085 
	gen multlinh`i' = _b[f`i'cumulg]
	gen seylinh`i' = _se[f`i'cumulg]

}

**# Single Decomposition

preserve 

ivreg2 f0cumuly (f0cumulg = newsy) L(1/4).newsy L(1/4).y L(1/4).g t t2 t3 t4, robust bw(auto)

** FWL del outcome endógena e instrumenta
reg f0cumuly L(1/4).newsy L(1/4).y L(1/4).g t t2 t3 t4
predict y_resid, resid

reg f0cumulg L(1/4).newsy L(1/4).y L(1/4).g t t2 t3 t4
predict x_resid, resid

reg newsy L(1/4).newsy L(1/4).y L(1/4).g t t2 t3 t4
predict z_resid, resid

** FWL del outcome endógena e instrumenta

* First stage projection: 
reg x_resid z_resid, nocons
predict xhat_resid, xb

* Second stage projection
reg y_resid xhat_resid, nocons
gen beta_hat = _b[xhat_resid]

gen contrib = xhat_resid * y_resid
egen total_contrib = total(contrib)
gen rel_weight = contrib / total_contrib

gen beta_piece = rel_weight * beta_hat
sum beta_piece
di "Sum of beta components: " r(sum)
di "Original beta: " beta_hat

	
tsline beta_piece, ///
    title("Contribution to β on L0.newsy by quarter") ///
    ytitle("Contribution to β") xtitle("Quarter") ///
    name(weight_plot, replace)

restore

**# Average Decomposition

preserve 
gen quarter_id = _n  // Create identifier for merging weights by quarter

forvalues i = 0/20 {
	
	* IV regression
	ivreg2 f`i'cumuly (f`i'cumulg = newsy) L(1/4).newsy L(1/4).y L(1/4).g t t2 t3 t4, robust bw(auto)

	* FWL residualization
	reg f`i'cumuly L(1/4).newsy L(1/4).y L(1/4).g t t2 t3 t4
	predict y_resid`i', resid

	reg f`i'cumulg L(1/4).newsy L(1/4).y L(1/4).g t t2 t3 t4
	predict x_resid`i', resid

	reg newsy L(1/4).newsy L(1/4).y L(1/4).g t t2 t3 t4
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
                              rel_weight15 rel_weight16 rel_weight17 rel_weight18 rel_weight19 rel_weight20)

tsline rel_weight_avg, ///
    ytitle("Average Relative Weight") xtitle("Quarter") ///
    name(avg_weight_plot, replace)
	
graph export "$Folder/2_Code/3_Output/Graphs/avg_weight_plot.pdf", as(pdf)  replace	


*** Overlap the instrument: 

twoway (tsline rel_weight_avg, lcolor(navy%70)) /// 
	(tsline newsy, lcolor(red%30)), /// 
	legend(label(1 "Average Relative Weight") /// 
	label(2 "Military News Shock")) /// 
	ytitle("") xtitle("Quarter") ///
	name(avg_weight_plot, replace) 		 
	
corr rel_weight_avg newsy

graph export "$Folder/2_Code/3_Output/Graphs/avg_weight_plot_overlap.pdf", as(pdf)  replace	

*** HHIs and weights on the positive part: 
*drop if rel_weight_avg<0

gen rel_weight_avg_abs=abs(rel_weight_avg)

egen total_weight = total(rel_weight_avg_abs)
gen share = rel_weight_avg_abs / total_weight
replace share =share*100
gen share_sq = share^2
egen HHI = total(share_sq) // 

** Compute the ratio of weights: 
drop total_weight 
gen rel_weight_avg_pos = rel_weight_avg
replace rel_weight_avg_pos=. if rel_weight_avg<0
egen total_weight = total(rel_weight_avg_pos)
order rel_weight_avg_pos 
gsort -rel_weight_avg_pos
gen rank = _n
summ rel_weight_avg_pos if rank <= 10
scalar top10_sum = r(sum)
scalar ratio = top10_sum / total_weight
display ratio // 0.69815308

* Step 1: absolute value of weights
gen abs_w = abs(rel_weight_avg)
* Step 2: normalize so weights sum to 1
egen TOTAL_weights=sum(abs_w)
gen abs_w_norm = abs_w / TOTAL_weights
replace abs_w_norm=abs_w_norm*100
* Step 3: square normalized weights
gen abs_w_norm_sq = abs_w_norm^2
* Step 4: compute Herfindahl index
egen HHI_norm = total(abs_w_norm_sq)
* Display result
su HHI_norm // 443.3972

*** Compute the share of absolute weights in the pre 1952 period: 
keep if rel_weight_avg>0
egen SUM_pre52=sum(rel_weight_avg) if quarter<1952
egen SUM_total=sum(rel_weight_avg)
gen ratio_weights=SUM_pre52/SUM_total // 96.19028
su ratio_weights

** Cumulative Share of ToTal Weight
keep if rel_weight_avg > 0
gsort -rel_weight_avg_pos
gen rank2 = _n
gen cum_share = sum(rel_weight_avg_pos) / total_weight * 100

twoway (line cum_share rank2 if rank2 <= 50, lcolor(navy)), ///
    ytitle("Cumulative Share of Total Weight (%)") ///
    xtitle("Number of Top-Weighted Quarters") ///
    yline(50 90, lpattern(dash) lcolor(gray)) ///
    xlabel(0(10)50) ylabel(0(20)100) ///
    name(variance_decomp, replace)

restore

**# Average decomposition good times

ivreg2 f0cumuly (expf0cumulg = exp0newsy) L.slack recnewsy* expnewsy* recy* expy* recg* expg* t t2 t3 t4, robust bw(auto) first

preserve 

gen quarter_id = _n  // Create identifier for merging weights by quarter

forvalues i = 0/20 {
	
	* IV regression
	ivreg2 f`i'cumuly (expf`i'cumulg = exp0newsy) L.slack recnewsy* expnewsy* recy* expy* recg* expg* t t2 t3 t4, robust bw(auto) 

	* FWL residualization
	reg f`i'cumuly L.slack recnewsy* expnewsy* recy* expy* recg* expg* t t2 t3 t4
	predict y_resid`i', resid

	reg expf`i'cumulg L.slack recnewsy* expnewsy* recy* expy* recg* expg* t t2 t3 t4
	predict x_resid`i', resid

	reg exp0newsy L.slack recnewsy* expnewsy* recy* expy* recg* expg* t t2 t3 t4
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
                              rel_weight15 rel_weight16 rel_weight17 rel_weight18 rel_weight19 rel_weight20)


tsline rel_weight_avg, ///
    ytitle("Average Relative Weight") xtitle("Quarter") ///
    name(avg_weight_plot, replace)

graph export "$Folder/2_Code/3_Output/Graphs/exp_weight_plot.pdf", as(pdf)  replace	

twoway (tsline rel_weight_avg, lcolor(navy%70)) ///
		(tsline newsy, lcolor(red%30)), ///
		 legend(label(1 "Average Relative Weight") ///
         label(2 "Military News Shock")) ///
		 ytitle("Average Relative Weight") xtitle("Quarter") ///
		 name(avg_weight_plot, replace)
corr rel_weight_avg newsy		

graph export "$Folder/2_Code/3_Output/Graphs/exp_weight_plot_overlap.pdf", as(pdf)  replace	

*** HHIs ***
gen rel_weight_avg_abs=abs(rel_weight_avg)

egen total_weight = total(rel_weight_avg_abs)
gen share = rel_weight_avg_abs / total_weight
replace share =share*100
gen share_sq = share^2
egen HHI = total(share_sq) // 

** Compute the ratio of weights: 
drop total_weight 
replace rel_weight_avg=. if rel_weight_avg<0
egen total_weight = total(rel_weight_avg)
order rel_weight_avg 
gsort -rel_weight_avg
gen rank = _n
summ rel_weight_avg if rank <= 10
scalar top10_sum = r(sum)
scalar ratio = top10_sum / total_weight
display ratio // 0.68780179

* Step 1: absolute value of weights
gen abs_w = abs(rel_weight_avg)
* Step 2: normalize so weights sum to 1
egen TOTAL_weights=sum(abs_w)
gen abs_w_norm = abs_w / TOTAL_weights
replace abs_w_norm=abs_w_norm*100
* Step 3: square normalized weights
gen abs_w_norm_sq = abs_w_norm^2
* Step 4: compute Herfindahl index
egen HHI_norm = total(abs_w_norm_sq)
* Display result
su HHI_norm // 661.289

keep if rel_weight_avg>0
egen SUM_pre52=sum(rel_weight_avg) if quarter<1952
egen SUM_total=sum(rel_weight_avg)
gen ratio_weights=SUM_pre52/SUM_total // 88.10069  
su ratio_weights

restore

**# Average decomposition crisis: 

preserve 
gen quarter_id = _n  // Create identifier for merging weights by quarter

forvalues i = 0/20 {
	
	* IV regression
	ivreg2 f`i'cumuly (recf`i'cumulg = rec0newsy) L.slack recnewsy* expnewsy* recy* expy* recg* expg* t t2 t3 t4, robust bw(auto)

	* FWL residualization
	reg f`i'cumuly L.slack recnewsy* expnewsy* recy* expy* recg* expg* t t2 t3 t4
	predict y_resid`i', resid

	reg recf`i'cumulg L.slack recnewsy* expnewsy* recy* expy* recg* expg* t t2 t3 t4
	predict x_resid`i', resid

	reg rec0newsy L.slack recnewsy* expnewsy* recy* expy* recg* expg* t t2 t3 t4
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
                              rel_weight15 rel_weight16 rel_weight17 rel_weight18 rel_weight19 rel_weight20)


tsline rel_weight_avg, ///
    ytitle("Average Relative Weight") xtitle("Quarter") 
	

graph export "$Folder/2_Code/3_Output/Graphs/rec_weight_plot.pdf", as(pdf)  replace	

twoway (tsline rel_weight_avg, lcolor(navy%70)) ///
		(tsline newsy, lcolor(red%30)), ///
		 legend(label(1 "Average Relative Weight") ///
         label(2 "Military News Shock")) ///
		 ytitle("Average Relative Weight") xtitle("Quarter") ///
		 name(avg_weight_plot, replace)
		 
corr rel_weight_avg newsy		

graph export "$Folder/2_Code/3_Output/Graphs/rec_weight_plot_overlap.pdf", as(pdf)  replace	

*** HHIs *** 
gen rel_weight_avg_abs=abs(rel_weight_avg)

egen total_weight = total(rel_weight_avg_abs)
gen share = rel_weight_avg_abs / total_weight
replace share =share*100
gen share_sq = share^2
egen HHI = total(share_sq) // 

** Compute the ratio of weights: 
drop total_weight 
replace rel_weight_avg=. if rel_weight_avg<0
egen total_weight = total(rel_weight_avg)
order rel_weight_avg 
gsort -rel_weight_avg
gen rank = _n
summ rel_weight_avg if rank <= 10
scalar top10_sum = r(sum)
scalar ratio = top10_sum / total_weight
display ratio // 0.46618026

* Step 1: absolute value of weights
gen abs_w = abs(rel_weight_avg)
* Step 2: normalize so weights sum to 1
egen TOTAL_weights=sum(abs_w)
gen abs_w_norm = abs_w / TOTAL_weights
replace abs_w_norm=abs_w_norm*100
* Step 3: square normalized weights
gen abs_w_norm_sq = abs_w_norm^2
* Step 4: compute Herfindahl index
egen HHI_norm = total(abs_w_norm_sq)
* Display result
su HHI_norm // 580.037  

keep if rel_weight_avg>0
egen SUM_pre52=sum(rel_weight_avg) if quarter<1952
egen SUM_total=sum(rel_weight_avg)
gen ratio_weights=SUM_pre52/SUM_total // 84.6707  
su ratio_weights

restore
