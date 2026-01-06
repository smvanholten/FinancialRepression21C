# Figure inputs

- `Code/Figure1_twin_debt.R` → Figure 1 (The twin debt problem) using `Data/EWN-database-January-2025.xlsx` and `Data/IMF_public_debt_GDP.xlsx`. Update `ROOT` at the top of the script to point to this `YOLO` folder before running.
- `Code/Figure2a_inflation_bar.do` and `Code/Figure2b_US_devaluation.do` → Figure 2 panels (unexpected inflation bar + historical US devaluation line). They read `Data/Institution_data/WEO_cf_2019vintage_long_OECD_forplots.dta` and `Data/Ball_Replication.xlsx`; change the `cd`/export paths to this folder when running in Stata.
- `Code/Figure3_public_liabilities.R` → Figure 3 (Components of public liabilities) using the country files under `Data/Canada`, `Data/France`, `Data/Germany`, `Data/Italy`, `Data/Japan`, and `Data/UK`, plus the same `Data/` root. It also pulls FRED/Eurostat/BoJ/BoE/StatCan series via APIs, so set `ROOT_DIR` to this `YOLO/Data` path and ensure internet/API access.

# Ready-made figure files

- `Figures/Figure1_twin_debt.png` – Figure 1
- `Figures/Figure2a_inflation_bar.png` and `Figures/Figure2b_US_devaluation.png` – Figure 2 panels (a) and (b)
- `Figures/Figure3_public_liabilities.png` – Figure 3
