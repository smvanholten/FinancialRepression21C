*------------------------------------------------------------
* Ball_replication.xlsx — sheet Decomposition
* Plot of (a2) against End of Fiscal Year — years > 1969
*------------------------------------------------------------
version 18
clear all
set more off
* Put your root below
cd "" 
import excel using "Data/Ball_replication.xlsx", sheet("Decomposition") firstrow case(lower) clear

capture confirm variable endoffiscalyear
if _rc {
    di as err "Variable 'endoffiscalyear' absent"
    describe
    exit 111
}
capture confirm variable a2
if _rc {
    di as err "Variable 'a2' absent"
    describe
    exit 111
}

gen int year = real(endoffiscalyear)
label var year "End of Fiscal Year"
drop if missing(year)
assert inrange(year,1800,2100)

keep if !missing(a2)
keep if year > 1970

quietly summarize year
local x_min = r(min)
local x_max = r(max)
local x_rng = `x_max' - `x_min'

quietly summarize a2
local y_min = r(min)
local y_max = r(max)
local mu    = r(mean)

local x_left = `x_min' + 0.09*`x_rng'
local y_pad  = 0.9*(`y_max' - `y_min')
local y_step = 0.07*(`y_max' - `y_min')
local y1 = `y_max' - `y_pad'
local y2 = `y1' - `y_step'
local y3 = `y2' - `y_step'

local maxlab : display %9.3f `y_max'
local minlab : display %9.3f `y_min'
local mulab  : display %9.3f `mu'

twoway ///
    (line a2 year, sort lcolor(blue) lwidth(thick)), ///
    xlabel(`x_min'(5)`x_max', format(%4.0f)) ///
    ytitle("Percentage") xtitle("") ///
    legend(off) graphregion(color(white)) ///
    text(`y1' `x_left' "Max: `maxlab'",  box bcolor(white) lcolor(none) margin(tiny)) ///
    text(`y2' `x_left' "Min: `minlab'",  box bcolor(white) lcolor(none) margin(tiny)) ///
    text(`y3' `x_left' "Average: `mulab'", box bcolor(white) lcolor(none) margin(tiny)) ///
    name(FY_a2_1969, replace)

capture noisily graph export "Figures/Figure2b_US_devaluation.png", width(2400) replace
