# Figure generation guide (self-contained)

**Folder assumption**: run everything from this folder. Update any hard-coded `ROOT`/`cd` paths inside the scripts to point here.

## Figure 1 – Twin debt problem
- Script: `Code/Figure1_twin_debt.R`
- Inputs: `Data/EWN-database-January-2025.xlsx`, `Data/IMF_public_debt_GDP.xlsx`
- Output: `Figures/Figure1_twin_debt.png`

## Figure 2 – Unexpected inflation and US devaluation
1) **Prep (run first in Stata)**  
   - Script: `Code/Figure2_inflation_prep.do`  
   - Uses: `Data/Institution_data/WEO.dta` and related WEO files  
   - Produces: `Data/Institution_data/WEO_cf_2019vintage_long_OECD_forplots.dta`
2) **Panel (a) bar chart**  
   - Script: `Code/Figure2a_inflation_bar.do`  
   - Inputs: `Data/Institution_data/WEO_cf_2019vintage_long_OECD_forplots.dta` (from step 1)  
   - Output: `Figures/Figure2a_inflation_bar.png`
3) **Panel (b) US history line**  
   - Script: `Code/Figure2b_US_devaluation.do`  
   - Inputs: `Data/Ball_Replication.xlsx`  
   - Output: `Figures/Figure2b_US_devaluation.png`

## Figure 3 – Components of public liabilities
- Script: `Code/Figure3_public_liabilities.R`
- Inputs: country files under `Data/Canada`, `Data/France`, `Data/Germany`, `Data/Italy`, `Data/Japan` (incl. `Japan/Japan CB Holdings Data`), `Data/UK`
- External pulls: FRED/Eurostat/BoJ/BoE/StatCan APIs (requires internet access and a FRED key set in the script)
- Output: `Figures/Figure3_public_liabilities.png`
