clear all
set more off

cd "YOUR ROOT HERE"

*------------------------------------------------------------*
* SETUP: OECD FULL SAMPLE
*------------------------------------------------------------*
use "WEO_cf_2019vintage_long_OECD_forplots.dta", clear

*------------------------------------------------------------*
* Bar chart: d_cf_mpi (average 2020–2024) with inverted sign
*------------------------------------------------------------*
preserve

    * Subset of selected countries and period 2020–2024
    keep if inlist(Country, ///
        "China","France","United States","United Kingdom", ///
        "Japan","India","Germany","Canada","Italy")
    keep if inrange(year, 2020, 2024)

    * Average by country
    collapse (mean) d_cf_mpi, by(Country)

    * Invert sign only for display
    gen d_cf_mpi_flip = -d_cf_mpi

    * Sorting: preserve the original graph order (by d_cf_mpi descending)
    gsort -d_cf_mpi Country
    * (Alternative: sort by the inverted series:)
    * gsort -d_cf_mpi_flip Country

    * Y index + country labels in display order
    gen y = _n
    capture label drop ylab
    forvalues i = 1/`=_N' {
        local nm = Country[`i']
        local nm = subinstr("`nm'","""","'",.)
        label define ylab `i' "`nm'", add
    }
    label values y ylab

    * Symmetric range (on displayed series)
    quietly summarize d_cf_mpi_flip
    local absmax = max(abs(r(min)), abs(r(max)))
    local lo = -ceil(`absmax'*1.05)
    local hi =  ceil(`absmax'*1.05)
    local step = round((`hi' - `lo')/6, 1)
    if (`step'<=0) local step = 1

    * Manually set symmetric range
    local lo = -12
    local hi = 12

    twoway ///
        (bar d_cf_mpi_flip y, horizontal barwidth(0.8) fcolor(navy) lcolor(navy)) ///
        , xscale(range(`lo' `hi')) ///
          xlabel(`lo'(`step')`hi' 0, format(%9.0g)) ///
          yscale(reverse) ///
          ylabel(1(1)`=_N', valuelabel noticks nogrid) ///
          xtitle("Unexpected inflation contribution, 2020–2024 (pp)") ///
          ytitle("") ///
          xline(0, lpattern(shortdash) lcolor(gs8)) ///
          title("") ///
          legend(off) name(bar_d_cf_mpi_7sym_inv, replace)

    graph export ///
	 * Put your root below

        "....\Figures\Figure2a_inflation_bar.png", replace

restore
