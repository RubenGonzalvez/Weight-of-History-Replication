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

********************************************************************************
* Original Ramey code: 
********************************************************************************

drop _all
clear all

set more 1
set matsize 800

capture log close
*log using jordagkirfs_results.log, replace;

set scheme Modern

**# Multiplier before 1952

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

import delimited "$Folder/1_Data/rzdatnew.csv"

*insheet using rzdatnew.csv;

drop if quarter<1889

keep if quarter>1952

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


* Renaming variables: 

drop sey*

rename multlin multlin2
rename multexp multexp2
rename multrec multrec2


foreach var in multlin1 multexp1 multrec1 Fkplin Fkpexp Fkprec seylin seyexp seyrec ptestdiff Fdifflin Fdiffexp Fdiffrec{
  
  quietly gen `var' = .
  
}


** Ramey Multipliers pre 1952


forvalues i = 0/20 {

  ivreg2 f`i'cumuly (f`i'cumulg = $`shock'linshock) $`shock'linxlist t t2 t3 t4 if L`p'.`omit'==0 & F`i'.`omit'==0 & `omit'==0, robust bw(auto)
  gen Fkplinh`i'= e(widstat) /* Kleibergen-Paap rk Wald F statistic*/
  gen Fdifflinh`i'= Fkplinh`i'- 23.1085 
  gen multlinh`i' = _b[f`i'cumulg]
  gen seylinh`i' = _se[f`i'cumulg] /* HAC robust standard error*/
  
  ivreg2 f`i'cumuly (expf`i'cumulg = exp0`shock') $`shock'nlxlist t t2 t3 t4 if L`p'.`omit'==0 & F`i'.`omit'==0 & `omit'==0, robust bw(auto)
  gen Fkpexph`i'= e(widstat)
  gen Fdiffexph`i'= Fkpexph`i'- 23.1085 
  gen multexph`i' = _b[expf`i'cumulg]
  gen seyexph`i' = _se[expf`i'cumulg]
  
  ivreg2 f`i'cumuly (recf`i'cumulg = rec0`shock') $`shock'nlxlist t t2 t3 t4 if L`p'.`omit'==0 & F`i'.`omit'==0 & `omit'==0, robust bw(auto)
  gen Fkprech`i'= e(widstat)  
  gen Fdiffrech`i'= Fkprech`i'- 23.1085
  gen multrech`i' = _b[recf`i'cumulg]
  gen seyrech`i' = _se[recf`i'cumulg]
 
  ivreg2 f`i'cumuly (expf`i'cumulg recf`i'cumulg = exp0`shock' rec0`shock') $`shock'nlxlist t t2 t3 t4 if L`p'.`omit'==0 & F`i'.`omit'==0 & `omit'==0, robust bw(auto)
  test expf`i'cumulg=recf`i'cumulg
  gen ptestdiffh`i' = r(p)
 
 
  	
 foreach var in multlin multexp multrec {
  
    quietly replace `var'1 = `var'h`i' if h==`i'
	
  }
  
  foreach var in seylin seyexp seyrec ptestdiff Fkplin Fkpexp Fkprec  {
  
    quietly replace `var' = `var'h`i' if h==`i'
	
  }
  
 foreach var in  Fdifflin Fdiffexp Fdiffrec {
  
    quietly replace `var' = `var'h`i' if h==`i'
	quietly replace `var' = 30 if `var'>30
	
  }
}

**# Multipliers plot: 

/*

gen ub_linear_pre = multlin1 + 1.96 * seylin
gen lb_linear_pre = multlin1 - 1.96 * seylin
gen ub_exp_pre = multexp1 + 1.96 * seyexp
gen lb_exp_pre = multexp1 - 1.96 * seyexp
gen ub_rec_pre = multrec1 + 1.96 * seyrec
gen lb_rec_pre = multrec1 - 1.96 * seyrec

keep if h<=20

keep ub_linear_pre lb_linear_pre ub_exp_pre lb_exp_pre ub_rec_pre lb_rec_pre h
tempfile tmp1 
save `tmp1'


*/

keep if h<=20

gen ub_linear_pre = multlin1 + 1.96 * seylin
gen lb_linear_pre = multlin1 - 1.96 * seylin

twoway ///
    (rarea ub_linear lb_linear_pre h, color(blue%20)) ///  
    (line multlin1 h, lcolor(blue) lwidth(medium)), ///
    xtitle("Horizon") ///
    ytitle("Coefficient and 95% CI") ///
    graphregion(color(white)) legend(off)
	
gr rename avg_mult_linear_post, replace

graph export "$Folder/3_Output/Graphs/avg_mult_linear_post.pdf", as(pdf)  replace	

	
gen ub_exp = multexp1 + 1.96 * seyexp
gen lb_exp = multexp1 - 1.96 * seyexp

twoway ///
    (rarea ub_exp lb_exp h, color(blue%20)) /// 
    (line multexp1 h, lcolor(blue) lwidth(medium)), ///
    xtitle("Horizon") ///
    ytitle("Coefficient and 95% CI") ///
	ylabel(-20(10)20) ///
    graphregion(color(white)) legend(off)
	
gr rename avg_mult_exp_post, replace
	
graph export "$Folder/3_Output/Graphs/avg_mult_exp_post.pdf", as(pdf)  replace	


	
gen ub_rec = multrec1 + 1.96 * seyrec
gen lb_ec = multrec1 - 1.96 * seyrec

twoway ///
    (rarea ub_rec lb_ec h, color(blue%20)) ///  
    (line multrec1 h, lcolor(blue) lwidth(medium)), ///
    xtitle("Horizon") ///
    ytitle("Coefficient and 95% CI") ///
    graphregion(color(white)) legend(off)
	
gr rename avg_mult_rec_post, replace

graph export "$Folder/3_Output/Graphs/avg_mult_rec_post.pdf", as(pdf)  replace	






