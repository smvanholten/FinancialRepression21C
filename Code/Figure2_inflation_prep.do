************************************************************************************
************************************************************************************
*************************************************************************************

************************************************************
* 0) Setup
************************************************************
clear all
set more off
cd "YOUR ROOT HERE"

* tracked variables (includes predicted interest)
local vars GGXINT_NGDP NGDP_D NGDP_RPCH GGXWDG_NGDP GGXONLB_NGDP F_pi F_g i_eff_tm1 b_pred_B GGXINT_NGDP_pred err_B

************************************************************
* 1) Block A (2019, horizons):
*    - restricted to 2019 (October)
*    - compute factors on the horizon axis, recursive b_pred_B,
*    - compute GGXINT_NGDP_pred (from h>=2 with predicted debt at h-1),
*    - record h=0..5 → 2019..2024,
*    - includes the primary balance actually used in recursion
************************************************************
local Y = 2019
use "WEO", clear
keep if month(Date)==10
keep if year(Date)==`Y'
*-----------------------------------------------
* Gountry selection
*-----------------------------------------------
gen byte oecd = 0
replace oecd = 1 if Country=="Japan"
replace oecd = 1 if Country=="India"
replace oecd = 1 if Country=="China"
replace oecd = 1 if Country=="Australia"
replace oecd = 1 if Country=="Austria"
replace oecd = 1 if Country=="Belgium"
replace oecd = 1 if Country=="Canada"
replace oecd = 1 if Country=="Chile"
replace oecd = 1 if Country=="Colombia"
replace oecd = 1 if Country=="Costa Rica"
replace oecd = 1 if Country=="Czechia"
replace oecd = 1 if Country=="Denmark"
replace oecd = 1 if Country=="Estonia"
replace oecd = 1 if Country=="Finland"
replace oecd = 1 if Country=="France"
replace oecd = 1 if Country=="Germany"
replace oecd = 1 if Country=="Greece"
replace oecd = 1 if Country=="Hungary"
replace oecd = 1 if Country=="Iceland"
replace oecd = 1 if Country=="Ireland"
replace oecd = 1 if Country=="Israel"
replace oecd = 1 if Country=="Italy"
replace oecd = 1 if Country=="Japan"
replace oecd = 1 if Country=="Korea"
replace oecd = 1 if Country=="Latvia"
replace oecd = 1 if Country=="Lithuania"
replace oecd = 1 if Country=="Luxembourg"
replace oecd = 1 if Country=="Mexico"
replace oecd = 1 if Country=="Netherlands"
replace oecd = 1 if Country=="New Zealand"
replace oecd = 1 if Country=="Norway"
replace oecd = 1 if Country=="Poland"
replace oecd = 1 if Country=="Portugal"
replace oecd = 1 if Country=="Slovak Republic"
replace oecd = 1 if Country=="Slovenia"
replace oecd = 1 if Country=="Spain"
replace oecd = 1 if Country=="Sweden"
replace oecd = 1 if Country=="Switzerland"
replace oecd = 1 if Country=="Türkiye"
replace oecd = 1 if Country=="United Kingdom"
replace oecd = 1 if Country=="United States"

keep if oecd==1
drop oecd


capture confirm variable NGDP_RPCH
local HAS_RPCH = (_rc==0)

capture confirm variable NGDP
local HAS_NGDP = (_rc==0)



* Panel (country × horizons)
capture confirm variable Date
destring Date, replace force
encode Country, gen(ctry)
xtset ctry horizon

* Construct GGXINT_NGDP if missing
capture confirm variable GGXINT_NGDP
if _rc {
    gen double GGXINT_NGDP = GGXONLB_NGDP - GGXCNL_NGDP
    label var GGXINT_NGDP "Interest payments (% of GDP, WEO)"
}

* (1+pi_h) vs h-1
capture drop F_pi
by ctry (horizon): gen double F_pi = NGDP_D / L.NGDP_D if horizon>0
label var F_pi "(1+pi_h) from NGDP_D (h vs h-1)"

* (1+g_h)
capture drop F_g
capture confirm variable NGDP_RPCH
if !_rc {
    gen double F_g = 1 + NGDP_RPCH/100
    label var F_g "(1+g_h) from NGDP_RPCH (%)"
}
capture drop F_g2
if `HAS_RPCH' {
    assert !missing(F_pi) if horizon>0
    gen double F_g2 = F_g * F_pi
    label var F_g2 "(1+g_h)*(1+pi_h) (constructed nominal growth)"
}
capture drop F_g3
if `HAS_RPCH' {
    assert !missing(F_pi) if horizon>0
    gen double F_g3 = NGDP / NGDP_R if horizon>0
    label var F_g3 "(1+g_3"
}
capture drop F_g4
if `HAS_RPCH' {
    assert !missing(F_pi) if horizon>0
    gen double F_g4 = F_g3 / L.F_g3 if horizon>0
    label var F_g4 "(1+g_3"
}
*-----

*------------------------------------------------------------

*------------------------------------------------------------
capture drop F_gT
if `HAS_NGDP' {
    by ctry (horizon): gen double F_gT = NGDP / L.NGDP if horizon>0
    label var F_gT "Nominal growth from NGDP: NGDP_t / NGDP_{t-1}"
}






* ======= Effective interest rate and predicted debt (recursive, base = predicted debt from h>=2) =======
capture drop i_eff_tm1 b_pred_B GGXINT_NGDP_pred
gen double i_eff_tm1         = .
gen double b_pred_B          = .
gen double GGXINT_NGDP_pred  = .

* --- h = 1: the rate uses observed debt at h-1, predicted debt starts from L.GGXWDG_NGDP
by ctry (horizon): replace i_eff_tm1 = (GGXINT_NGDP * F_pi * F_g) / L.GGXWDG_NGDP if horizon==1
by ctry (horizon): replace GGXINT_NGDP_pred = i_eff_tm1 * L.GGXWDG_NGDP if horizon==1
by ctry (horizon): replace b_pred_B = ((1 + i_eff_tm1)/(F_pi*F_g)) * L.GGXWDG_NGDP ///
                                      - (L.GGXONLB_NGDP/(F_pi*F_g)) if horizon==1

* --- h = 2..5: i_eff_tm1 uses predicted debt at h-1 (L.b_pred_B)
forvalues h = 2/5 {
     by ctry (horizon): replace i_eff_tm1 = (GGXINT_NGDP * F_pi * F_g) / L.b_pred_B if horizon==`h'
    by ctry (horizon): replace GGXINT_NGDP_pred = i_eff_tm1 * L.b_pred_B if horizon==`h'
    by ctry (horizon): replace b_pred_B = ((1 + i_eff_tm1)/(F_pi*F_g)) * L.b_pred_B ///
                                          - (L.GGXONLB_NGDP/(F_pi*F_g)) if horizon==`h'
}

label var i_eff_tm1        "Effective interest rate i_{h-1} (uses predicted debt from h>=2)"
label var b_pred_B         "Predicted debt (Variant B, recursive on horizon)"
label var GGXINT_NGDP_pred "Predicted interest (uses predicted debt from h>=2)"

* Residual (defined for h>=1)
capture drop err_B
gen double err_B = GGXWDG_NGDP - b_pred_B if horizon>=1
label var err_B "Actual - Predicted (Variant B, horizon)"

* Primary balance actually used (lag across horizons)
capture drop primary_used_B
by ctry (horizon): gen double primary_used_B = L.GGXONLB_NGDP if horizon>=1
label var primary_used_B "Primary USED in recursion (lag across horizons)"

*******************************************************
* Save each step h=0..5 as target years
* (h=0→2019, h=1→2020, ..., h=5→2024)
*******************************************************
keep if inrange(horizon,0,5)
gen int target_year = `Y' + horizon
label var target_year "Target year implied by horizon (Y+h)"
format target_year %ty

* ------ LONG panel (horizons → years)
keep Country ctry horizon target_year ///
     b_pred_B i_eff_tm1 GGXINT_NGDP_pred F_pi F_g ///
     GGXWDG_NGDP GGXINT_NGDP GGXONLB_NGDP primary_used_B err_B
order Country target_year horizon b_pred_B GGXWDG_NGDP i_eff_tm1 GGXINT_NGDP_pred F_pi F_g GGXINT_NGDP GGXONLB_NGDP primary_used_B err_B

tempfile panel_long_h
save `panel_long_h', replace
save "WEO_G7_expected_series_2019vintage_h0toh5_LONG_OECD.dta", replace




************************************************************************************
* 2) Block B (time axis, horizon==0), G7, 2019→2024
************************************************************************************
clear
use "weo_country_year_subset2024OECD", clear
* --- Country selection ---
gen byte oecd = 0
replace oecd = 1 if Country=="Japan"
replace oecd = 1 if Country=="India"
replace oecd = 1 if Country=="China"
replace oecd = 1 if Country=="Australia"
replace oecd = 1 if Country=="Austria"
replace oecd = 1 if Country=="Belgium"
replace oecd = 1 if Country=="Canada"
replace oecd = 1 if Country=="Chile"
replace oecd = 1 if Country=="Colombia"
replace oecd = 1 if Country=="Costa Rica"
replace oecd = 1 if Country=="Czechia"
replace oecd = 1 if Country=="Denmark"
replace oecd = 1 if Country=="Estonia"
replace oecd = 1 if Country=="Finland"
replace oecd = 1 if Country=="France"
replace oecd = 1 if Country=="Germany"
replace oecd = 1 if Country=="Greece"
replace oecd = 1 if Country=="Hungary"
replace oecd = 1 if Country=="Iceland"
replace oecd = 1 if Country=="Ireland"
replace oecd = 1 if Country=="Israel"
replace oecd = 1 if Country=="Italy"
replace oecd = 1 if Country=="Japan"
replace oecd = 1 if Country=="Korea"
replace oecd = 1 if Country=="Latvia"
replace oecd = 1 if Country=="Lithuania"
replace oecd = 1 if Country=="Luxembourg"
replace oecd = 1 if Country=="Mexico"
replace oecd = 1 if Country=="Netherlands"
replace oecd = 1 if Country=="New Zealand"
replace oecd = 1 if Country=="Norway"
replace oecd = 1 if Country=="Poland"
replace oecd = 1 if Country=="Portugal"
replace oecd = 1 if Country=="Slovak Republic"
replace oecd = 1 if Country=="Slovenia"
replace oecd = 1 if Country=="Spain"
replace oecd = 1 if Country=="Sweden"
replace oecd = 1 if Country=="Switzerland"
replace oecd = 1 if Country=="Türkiye"
replace oecd = 1 if Country=="United Kingdom"
replace oecd = 1 if Country=="United States"

keep if oecd==1
drop oecd

capture confirm variable NGDP_RPCH
local HAS_RPCH = (_rc==0)
capture confirm variable NGDP
local HAS_NGDP = (_rc==0)
capture confirm variable NGDP_D
local HAS_NGDP_D = (_rc==0)

* Annual index
rename Date year

encode Country, gen(ctry)
xtset ctry year

* GGXINT_NGDP if missing
capture confirm variable GGXINT_NGDP
if _rc {
    gen double GGXINT_NGDP = GGXONLB_NGDP - GGXCNL_NGDP
    label var GGXINT_NGDP "Interest payments (% of GDP, WEO)"
}

* Annual factors
capture drop F_pi_year F_g_year
by ctry (year): gen double F_pi_year = NGDP_D / L.NGDP_D if year>=2019
gen double F_g_year = 1 + NGDP_RPCH/100
label var F_pi_year "(1+pi_t) from NGDP_D (t vs t-1)"
label var F_g_year  "(1+g_t)  from NGDP_RPCH (%)"
* (1+g_t)*(1+pi_t) 
if `HAS_RPCH' & `HAS_NGDP_D' {
    gen double F_g2year = F_g_year * F_pi_year
    label var F_g2year "(1+g_t)*(1+pi_t) constructed nominal growth"
}

if `HAS_NGDP' {
    by ctry (year): gen double F_gT_year = NGDP / L.NGDP if year>=2019
    label var F_gT_year "Nominal growth from NGDP: NGDP_t / NGDP_{t-1}"
}



if `HAS_NGDP' {
    by ctry (year): gen double F_DEF = NGDP / NGDP_R if year>=2019
    label var F_DEF "Nominal growth from NGDP: NGDP_t / NGDP_{t-1}"
}

if `HAS_NGDP' {
    by ctry (year): gen double F_PIDEF = F_DEF / L.F_DEF if year>=2019
    label var F_PIDEF "Nominal growth from NGDP: NGDP_t / NGDP_{t-1}"
}



* Annual recursion (2019 obs → 2020..2024 predicted)
capture drop b_base i_eff_tm1_year b_pred_year
gen double b_base          = .
gen double i_eff_tm1_year  = .
gen double b_pred_year     = .

* 2020: base = observed debt 2019
by ctry (year): replace b_base         = L.GGXWDG_NGDP                    if year==2020
by ctry (year): replace i_eff_tm1_year = (GGXINT_NGDP * F_pi_year * F_g_year) / b_base if year==2020

by ctry (year): replace b_pred_year    = ((1 + i_eff_tm1_year)/(F_pi_year*F_g_year)) * b_base ///
                                         - (L.GGXONLB_NGDP/(F_pi_year*F_g_year))            if year==2020

* 2021..2024: base = predicted debt from previous year
forvalues y = 2021/2024 {
    by ctry (year): replace b_base         = L.b_pred_year                      if year==`y'
    by ctry (year): replace i_eff_tm1_year = (GGXINT_NGDP * F_pi_year * F_g_year) / b_base if year==`y'
    by ctry (year): replace b_pred_year    = ((1 + i_eff_tm1_year)/(F_pi_year*F_g_year)) * b_base ///
                                             - (L.GGXONLB_NGDP/(F_pi_year*F_g_year))       if year==`y'
}

label var b_pred_year    "Predicted debt-to-GDP (Variant B, year recursion)"
label var i_eff_tm1_year "Effective interest i_{t-1} using base debt (obs 2019, then predicted)"

* Primary balance used (lag t-1 on the time axis)
capture drop primary_used_year
by ctry (year): gen double primary_used_year = L.GGXONLB_NGDP if year>=2020
label var primary_used_year "Primary USED in year recursion (t-1)"
rename GGXINT_NGDP  GGXINT_NGDP_year

* ------ LONG outputs (2019 obs + 2020..2024)
preserve
keep Country year GGXWDG_NGDP GGXINT_NGDP_year GGXONLB_NGDP ///
     F_pi_year F_g_year i_eff_tm1_year b_pred_year primary_used_year
order Country year GGXWDG_NGDP b_pred_year i_eff_tm1_year F_pi_year F_g_year GGXINT_NGDP GGXONLB_NGDP primary_used_year
keep if inrange(year, 2019, 2024)
rename GGXWDG_NGDP GGXWDG_NGDP_year
save "WEO_yearpath_2019to2024_LONG_OECD.dta", replace
restore



************************************************************************************
* 3) Final merge: horizons (2019 vintage) vs years (obs→pred)
************************************************************************************
clear
use "WEO_G7_expected_series_2019vintage_h0toh5_LONG_OECD.dta", clear
rename target_year year   // harmonize key
tempfile horizon_based
save `horizon_based'

use "WEO_yearpath_2019to2024_LONG_OECD.dta", clear
tempfile year_based
save `year_based'

use `horizon_based', clear
merge 1:1 Country year using `year_based'

* Diagnostic
tab _merge

* (Option) put the "primary used" series at the end
local tailvars
foreach y of numlist 2019/2024 {
    capture confirm variable primary_used_B_y`y'
    if !_rc local tailvars `tailvars' primary_used_B_y`y'
}
* the primary_used_yYYYY only exist in WIDE; here in LONG we have "primary_used_year"
capture confirm variable primary_used_year
if !_rc local tailvars `tailvars' primary_used_year
order `tailvars', last

save "WEO_merged_horizon_vs_year_2019to2024_OECD.dta", replace


/******************************************************************
* Counterfactuals: replace ONE series by its observed (year-axis)
* while keeping the rest "expected 2019" (horizon-axis)
******************************************************************/
clear
cd "C:\Users\tokay\Dropbox\ReisMacroLab\Mundell-Fleming\Replication_Package\Data\Institution_data"
use "WEO_merged_horizon_vs_year_2019to2024_OECD.dta", clear

* Select useful window
keep if inrange(year, 2019, 2024)
*-----------------------------------------------
* Country selection
*-----------------------------------------------
gen byte oecd = 0
replace oecd = 1 if Country=="Japan"
replace oecd = 1 if Country=="India"
replace oecd = 1 if Country=="China"
replace oecd = 1 if Country=="Australia"
replace oecd = 1 if Country=="Austria"
replace oecd = 1 if Country=="Belgium"
replace oecd = 1 if Country=="Canada"
replace oecd = 1 if Country=="Chile"
replace oecd = 1 if Country=="Colombia"
replace oecd = 1 if Country=="Costa Rica"
replace oecd = 1 if Country=="Czechia"
replace oecd = 1 if Country=="Denmark"
replace oecd = 1 if Country=="Estonia"
replace oecd = 1 if Country=="Finland"
replace oecd = 1 if Country=="France"
replace oecd = 1 if Country=="Germany"
replace oecd = 1 if Country=="Greece"
replace oecd = 1 if Country=="Hungary"
replace oecd = 1 if Country=="Iceland"
replace oecd = 1 if Country=="Ireland"
replace oecd = 1 if Country=="Israel"
replace oecd = 1 if Country=="Italy"
replace oecd = 1 if Country=="Japan"
replace oecd = 1 if Country=="Korea"
replace oecd = 1 if Country=="Latvia"
replace oecd = 1 if Country=="Lithuania"
replace oecd = 1 if Country=="Luxembourg"
replace oecd = 1 if Country=="Mexico"
replace oecd = 1 if Country=="Netherlands"
replace oecd = 1 if Country=="New Zealand"
replace oecd = 1 if Country=="Norway"
replace oecd = 1 if Country=="Poland"
replace oecd = 1 if Country=="Portugal"
replace oecd = 1 if Country=="Slovak Republic"
replace oecd = 1 if Country=="Slovenia"
replace oecd = 1 if Country=="Spain"
replace oecd = 1 if Country=="Sweden"
replace oecd = 1 if Country=="Switzerland"
replace oecd = 1 if Country=="Türkiye"
replace oecd = 1 if Country=="United Kingdom"
replace oecd = 1 if Country=="United States"

keep if oecd==1
drop oecd


* Panel index if needed
capture confirm variable ctry
if _rc encode Country, gen(ctry)
xtset ctry year

* Separate "expected" (horizon) and "observed" (year) series
capture drop F_pi_exp F_g_exp primary_exp i_eff_exp_tm1
gen double F_pi_exp       = F_pi
gen double F_g_exp        = F_g
gen double primary_exp    = primary_used_B         // primary used in "expected" recursion
gen double i_eff_exp_tm1  = i_eff_tm1              // expected i_{t-1} (already computed on horizons)

* Observed (year-axis) already present: F_pi_year, F_g_year, GGXONLB_NGDP, GGXINT_NGDP

* Counterfactual variables to create
capture drop b_cf_pi b_cf_g b_cf_def b_cf_i b_base_cf i_eff_cf_tm1
gen double b_cf_pi  = .
gen double b_cf_g   = .
gen double b_cf_def = .
gen double b_cf_i   = .
gen double b_base_cf = .
gen double i_eff_cf_tm1 = .

*******************************************************
* 1) CF inflation: replace F_pi with F_pi_year
*    Base 2020 = observed debt 2019; then base = L.b_cf_pi
*******************************************************
* 2020
by ctry (year): replace b_base_cf = L.GGXWDG_NGDP if year==2020
by ctry (year): replace b_cf_pi = ((1 + i_eff_exp_tm1)/(F_pi_year * F_g_exp)) * b_base_cf ///
                                  - (primary_exp / (F_pi_year * F_g_exp))          if year==2020
* 2021..2024
forvalues y = 2021/2024 {
    by ctry (year): replace b_base_cf = L.b_cf_pi                                         if year==`y'
    by ctry (year): replace b_cf_pi   = ((1 + i_eff_exp_tm1)/(F_pi_year * F_g_exp)) * b_base_cf ///
                                        - (primary_exp / (F_pi_year * F_g_exp))           if year==`y'
}

*******************************************************
* 2) CF growth: replace F_g with F_g_year
*******************************************************
* 2020
by ctry (year): replace b_base_cf = L.GGXWDG_NGDP if year==2020
by ctry (year): replace b_cf_g  = ((1 + i_eff_exp_tm1)/(F_pi_exp * F_g_year)) * b_base_cf ///
                                  - (primary_exp / (F_pi_exp * F_g_year))          if year==2020
* 2021..2024
forvalues y = 2021/2024 {
    by ctry (year): replace b_base_cf = L.b_cf_g                                           if year==`y'
    by ctry (year): replace b_cf_g   = ((1 + i_eff_exp_tm1)/(F_pi_exp * F_g_year)) * b_base_cf ///
                                       - (primary_exp / (F_pi_exp * F_g_year))             if year==`y'
}

*******************************************************
* 3) CF primary deficit: replace primary with L.GGXONLB_NGDP (observed t-1)
*******************************************************
* 2020
by ctry (year): replace b_base_cf = L.GGXWDG_NGDP if year==2020
by ctry (year): replace b_cf_def = ((1 + i_eff_exp_tm1)/(F_pi_exp * F_g_exp)) * b_base_cf ///
                                   - (primary_used_year / (F_pi_exp * F_g_exp))       if year==2020
* 2021..2024
forvalues y = 2021/2024 {
    by ctry (year): replace b_base_cf = L.b_cf_def                                        if year==`y'
    by ctry (year): replace b_cf_def  = ((1 + i_eff_exp_tm1)/(F_pi_exp * F_g_exp)) * b_base_cf ///
                                        - (primary_used_year / (F_pi_exp * F_g_exp))        if year==`y'
}

*******************************************************
* 4) CF effective interest: replace i_{t-1} by observed (GGXINT_NGDP_{t-1}/base_cf)
*    NB: i depends on the debt base used (counterfactual)
*******************************************************
* 2020
by ctry (year): replace b_base_cf     = L.GGXWDG_NGDP                    if year==2020
by ctry (year): replace i_eff_cf_tm1 = (GGXINT_NGDP_year * F_pi_year * F_g_year) / b_base_cf if year==2020
by ctry (year): replace b_cf_i        = ((1 + i_eff_cf_tm1)/(F_pi_exp * F_g_exp)) * b_base_cf ///
                                        - (primary_exp / (F_pi_exp * F_g_exp))     if year==2020
* 2021..2024
forvalues y = 2021/2024 {
    by ctry (year): replace b_base_cf    = L.b_cf_i                             if year==`y'
    by ctry (year): replace i_eff_cf_tm1 = (GGXINT_NGDP_year * F_pi_year * F_g_year) / b_base_cf if year==`y'
    by ctry (year): replace b_cf_i       = ((1 + i_eff_cf_tm1)/(F_pi_exp * F_g_exp)) * b_base_cf ///
                                           - (primary_exp / (F_pi_exp * F_g_exp)) if year==`y'
}

*******************************************************
* 5) Deltas vs. "all expected 2019" path (b_pred_B)
*******************************************************
capture drop d_cf_pi d_cf_g d_cf_def d_cf_i
gen double d_cf_pi  = b_cf_pi  - b_pred_B if inrange(year,2020,2024)
gen double d_cf_g   = b_cf_g   - b_pred_B if inrange(year,2020,2024)
gen double d_cf_def = b_cf_def - b_pred_B if inrange(year,2020,2024)
gen double d_cf_i   = b_cf_i   - b_pred_B if inrange(year,2020,2024)

label var b_cf_pi   "CF: replace inflation with observed (year)"
label var b_cf_g    "CF: replace growth with observed (year)"
label var b_cf_def  "CF: replace primary with observed (t-1)"
label var b_cf_i    "CF: replace effective interest with observed (t-1)"
label var d_cf_pi   "ΔCF(inflation) vs expected-2019"
label var d_cf_g    "ΔCF(growth) vs expected-2019"
label var d_cf_def  "ΔCF(primary) vs expected-2019"
label var d_cf_i    "ΔCF(interest) vs expected-2019"

order Country year GGXWDG_NGDP b_pred_B b_cf_pi b_cf_g b_cf_def b_cf_i d_cf_pi d_cf_g d_cf_def d_cf_i, first
* Sum of all deltas
gen double d_cf_total = d_cf_pi + d_cf_g + d_cf_def + d_cf_i
label var d_cf_total "Total ΔCF (sum of inflation, growth, primary, interest)"

gen double d_actuC_exp = b_pred_year - b_pred_B
label var d_cf_total "Total unexpected"

gen double d_actu_exp = GGXWDG_NGDP_year - b_pred_B
label var d_cf_total "Total unexpected ACTUAL"

save "WEO_cf_from_year_series_2019vintage_LONG_OECD.dta", replace

* (Option) Wide version for quick inspection
preserve
keep Country year b_pred_B b_pred_year b_cf_pi b_cf_g b_cf_def b_cf_i
keep if inrange(year,2020,2024)
reshape wide b_pred_B b_pred_year b_cf_pi b_cf_g b_cf_def b_cf_i, i(Country) j(year)
order Country b_pred_B2020 b_cf_pi2020 b_cf_g2020 b_cf_def2020 b_cf_i2020
save "WEO_cf_from_year_series_2019vintage_WIDE_OECD.dta", replace
restore













* --- Create GGXWDG ---
capture confirm variable GGXWDG
if _rc {
    capture confirm variable GGXWDG_NGDP_year
    if !_rc {
        gen double GGXWDG = GGXWDG_NGDP_year
    }
    else {
        capture confirm variable GGXWDG_NGDP
        if !_rc gen double GGXWDG = GGXWDG_NGDP
        else    gen double GGXWDG = .
    }
}
label var GGXWDG "Debt-to-GDP (actual)"

* ---keep necessary variabkles ---
keep Country year GGXWDG_NGDP GGXWDG_NGDP_year b_pred_B b_pred_year b_cf_pi b_cf_g b_cf_def b_cf_i d_cf_pi d_cf_g d_cf_def d_cf_i
order Country year GGXWDG_NGDP  GGXWDG_NGDP_year b_pred_B b_pred_year b_cf_pi b_cf_g b_cf_def b_cf_i d_cf_pi d_cf_g d_cf_def d_cf_i
sort Country year
format GGXWDG_NGDP GGXWDG_NGDP_year b_pred_B b_pred_year b_cf_pi b_cf_g b_cf_def b_cf_i d_cf_pi d_cf_g d_cf_def d_cf_i %9.3f
rename GGXWDG_NGDP b_expected
rename GGXWDG_NGDP_year b_actual
rename b_pred_B b_expected_constr
rename b_pred_year b_actual_constr
gen double b_unexp_expconstruct  =  b_actual - b_expected_constr 
gen double b_unexp_bothconstruct  =  b_actual_constr - b_expected_constr 

gen double SUM_gap_noresid = d_cf_pi + d_cf_g + d_cf_def + d_cf_i
gen residual_actual_construction = b_actual - b_actual_constr
gen d_cf_mpi = -d_cf_pi
gen d_cf_mpig = -(d_cf_pi + d_cf_g)

gen double SUM_TOTAL = SUM_gap_noresid + residual_actual_construction
gen b_unexp = b_actual - b_expected_constr
gen errorsum = b_actual - SUM_TOTAL - b_expected_constr
order Country year ///
      b_expected b_actual ///
      b_expected_constr b_actual_constr ///
      b_cf_pi b_cf_g b_cf_def b_cf_i ///
      d_cf_pi d_cf_mpi d_cf_mpig d_cf_g d_cf_def d_cf_i ///
      b_unexp_expconstruct b_unexp_bothconstruct ///
      SUM_gap_noresid residual_actual_construction SUM_TOTAL ///
      b_unexp errorsum

format b_expected b_actual b_expected_constr b_actual_constr ///
       b_cf_pi b_cf_g b_cf_def b_cf_i ///
       d_cf_pi d_cf_mpi d_cf_mpig d_cf_g d_cf_def d_cf_i ///
       b_unexp_expconstruct b_unexp_bothconstruct ///
       SUM_gap_noresid residual_actual_construction SUM_TOTAL ///
       b_unexp errorsum %9.3f
* === Labels for all final variables ===

label var b_expected              "Debt/GDP – expected baseline (2019 vintage, GGXWDG_NGDP)"
label var b_actual                "Debt/GDP – actual observed (GGXWDG_NGDP_year)"
label var b_expected_constr       "Predicted debt from horizon recursion (b_pred_B, Oct 2019 vintage)"
label var b_actual_constr         "Predicted debt from year recursion (b_pred_year, base 2019 observed)"

label var b_cf_pi                 "Counterfactual debt: replace inflation with observed (F_pi_year)"
label var b_cf_g                  "Counterfactual debt: replace growth with observed (F_g_year)"
label var b_cf_def                "Counterfactual debt: replace primary with observed lagged (primary_used_year)"
label var b_cf_i                  "Counterfactual debt: replace effective interest with observed (GGXINT_NGDP_year/base)"

label var d_cf_pi                 "Δ CF inflation = b_cf_pi - b_expected_constr"
label var d_cf_g                  "Δ CF growth = b_cf_g - b_expected_constr"
label var d_cf_def                "Δ CF primary = b_cf_def - b_expected_constr"
label var d_cf_i                  "Δ CF interest = b_cf_i - b_expected_constr"

label var b_unexp_expconstruct    "Unexpected debt = b_actual - b_expected_constr"
label var b_unexp_bothconstruct   "Unexpected debt (both constr.) = b_actual_constr - b_expected_constr"

label var SUM_gap_noresid         "Sum of Δ CFs (pi + g + def + i)"
label var residual_actual_construction "Residual gap = b_actual - b_actual_constr"
label var SUM_TOTAL               "SUM_gap_noresid + residual_actual_construction"

label var b_unexp                 "Total unexpected = b_actual - b_expected"
label var errorsum                "Check residual = b_actual - SUM_TOTAL - b_expected"

* Export Excel (une seule feuille, ligne d'en-tête = noms de variables)
export excel using "WEO_cf_2019vintage_final.xlsx", ///
    sheet("final", replace) firstrow(variables)

* (Optionnel) Créer un mini dictionnaire (variable + label) et l'exporter sur une autre feuille
preserve
ds, has(varlabel)
local varlist `r(varlist)'
tempfile dict
postfile P str32 variable str200 label using `dict', replace
foreach v of local varlist {
    local L : variable label `v'
    post P ("`v'") ("`L'")
}
postclose P
use `dict', clear
export excel using "WEO_cf_2019vintage_final_OECD.xlsx", ///
    sheet("dictionary", replace) firstrow(variables)
restore
keep if year == 2024


order Country year ///
      b_expected b_actual ///
      b_expected_constr b_actual_constr ///
      b_cf_pi b_cf_g b_cf_def b_cf_i ///
      d_cf_pi d_cf_mpi d_cf_mpig d_cf_g d_cf_def d_cf_i ///
      b_unexp_expconstruct b_unexp_bothconstruct ///
      SUM_gap_noresid residual_actual_construction SUM_TOTAL ///
      b_unexp errorsum
compress
capture mkdir "Output"
save "C:\Users\tokay\Dropbox\ReisMacroLab\Mundell-Fleming\Replication_Package\Data\Institution_data\WEO_cf_2019vintage_long_OECD_forplots.dta", replace


