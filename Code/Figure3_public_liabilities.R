# ============================================================
# Public Debt Composition (% of GDP), avg 2006–2024
#
# Countries:
#   - United States, Japan, United Kingdom, Germany, France, Italy, Canada
#
# Sources:
#   - US/UK/JP GDP: FRED (quarterly; JP/US SAAR handled properly)
#   - DE/FR/IT GDP: Eurostat (quarterly levels)
#   - CA GDP: StatCan (annual, current prices)
#   - Debt composition: country-specific sources (Eurostat, FRED, BoJ/BoE,
#                       Bank of England, StatCan, and local CSV/XLSX files)
#
# Notes:
#   - For DE/FR/IT, central bank holdings appear as a single block.
#   - For US/UK/JP/CA, central bank is split into Currency/Reserves/Residual.
#   - Private locally-held debt is split short/long using national maturity
#     shares.
#   - The final chart reports the average 2006–2024 of each component
#     as a share of GDP. This range can be adjusted in "Global Settings"
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(zoo)
  library(ggplot2)
  library(scales)
  library(readr)
  library(readxl)
  library(janitor)
  library(purrr)
  library(stringr)
  library(fredr)
  library(eurostat)
  library(rsdmx)
  library(cansim)
})

# ---------------------------
# Root directories 
# ---------------------------
ROOT <- "YOUR ROOT HERE"
DATA_DIR <- file.path(ROOT, "Data")
FIG_DIR  <- file.path(ROOT, "Figures")
fp <- function(filename) file.path(DATA_DIR, filename)

# ---------------------------
# Global settings 
# ---------------------------
WINDOW_START <- 2006L
WINDOW_END   <- 2024L
fred_key = fredr_set_key("ENTER FRED KEY HERE")

# ---------------------------
# Plotting: colors & labels
# ---------------------------
COLS_GDP <- c(
  `Public external debt / GDP`          = "#0072B2",
  `Central bank / GDP`                  = "#6A3D9A",    # DE/FR/IT central bank = single block
  `Central bank — Currency / GDP`       = "#56B4E9",
  `Central bank — Reserves / GDP`       = "#009E73",
  `Central bank — Residual / GDP`       = "#E69F00",
  `Private locally-held — Short / GDP`  = "#D55E00",
  `Private locally-held — Long / GDP`   = "#CC79A7"
)

# Legend order (stacking order) and display labels
COMP_ORDER <- c(
  "Central bank / GDP",
  "Central bank — Reserves / GDP",
  "Central bank — Currency / GDP",
  "Central bank — Residual / GDP",
  "Public external debt / GDP",
  "Private locally-held — Short / GDP",
  "Private locally-held — Long / GDP"
)

# Labeling for the plots
NEW_LABELS <- c(
  "Central bank / GDP"                 = "Central bank",
  "Public external debt / GDP"         = "Foreign held",
  "Central bank — Currency / GDP"      = "Currency",
  "Central bank — Reserves / GDP"      = "Reserves",
  "Central bank — Residual / GDP"      = "CB-held bonds",
  "Private locally-held — Short / GDP" = "Domestic — Short term",
  "Private locally-held — Long / GDP"  = "Domestic — Long term"
)

# Country order (y-axis)
CTRY_ORDER <- c("Japan", "Italy", "United States", "Canada", "France", "United Kingdom", "Germany")

# ============================================================
# Helper functions
# ============================================================

# For stock series at (typically) monthly/quarterly frequency:
#   - pick the quarter-end observation (Q4 for each calendar year)
#   - convert from millions to billions
to_q_end_bn <- function(df) {
  df %>%
    mutate(Yq = as.yearqtr(date)) %>%
    group_by(Yq) %>%
    slice_max(date, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      date     = as.Date(Yq, frac = 1),
      value_bn = value / 1e3
    )
}

# Average selected columns over the window [WINDOW_START, WINDOW_END]; Used only at the very end.
avg_window <- function(df, year_col, cols) {
  tmp <- df %>%
    filter({{ year_col }} >= WINDOW_START, {{ year_col }} <= WINDOW_END)
  used_range <- range(tmp %>% pull({{ year_col }}), na.rm = TRUE)
  out <- tmp %>%
    summarise(across(all_of(cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  list(
    avg      = out,
    used_min = used_range[1],
    used_max = used_range[2]
  )
}

# ------------------------------------------------------------
# GDP helpers
# ------------------------------------------------------------

# FRED quarterly GDP → annual sum
# - If SAAR (is_saar = TRUE): convert to quarterly level by /4, then sum 4 quarters.
# - If not SAAR: values already quarterly levels, sum directly.
# value_scale rescales from original units into desired output units.
fred_gdp_annual <- function(series_id,
                            is_saar,
                            value_scale = 1,
                            start = as.Date("1980-01-01")) {
  fredr(series_id = series_id, observation_start = start) %>%
    transmute(
      date,
      q_level = if (is_saar) value / 4 else value
    ) %>%
    mutate(Year = year(date)) %>%
    group_by(Year) %>%
    summarise(GDP = sum(q_level), .groups = "drop") %>%
    mutate(GDP = GDP / value_scale)
}

# Eurostat GDP: quarterly levels in CP_MEUR → annual sums in €bn (Gives Nominal values)
eu_annual_gdp <- function(geo_code, out_name = "GDP_bn") {
  eurostat::get_eurostat("namq_10_gdp", time_format = "date") %>%
    filter(
      geo    == geo_code,
      s_adj  == "SCA", 
      na_item == "B1GQ",
      unit   == "CP_MEUR"
    ) %>%
    transmute(
      Year       = year(TIME_PERIOD),
      q_val_meur = values
    ) %>%
    group_by(Year) %>%
    summarise(!!out_name := sum(q_val_meur, na.rm = TRUE) / 1e3, .groups = "drop")
}

# StatCan REF_DATE parser for mixed formats:
#   yyyy, yyyy-mm, yyyyQq, yyyy-Qq
parse_statcan_ref_date <- function(x) {
  x <- as.character(x)
  d <- suppressWarnings(ymd(x, quiet = TRUE))
  
  bad <- is.na(d) & grepl("^\\d{4}-\\d{2}$", x)
  d[bad] <- as.Date(as.yearmon(x[bad], "%Y-%m"), frac = 1)
  
  bad <- is.na(d) & grepl("^\\d{4}Q[1-4]$", x)
  d[bad] <- as.Date(as.yearqtr(x[bad], "%YQ%q"), frac = 1)
  
  bad <- is.na(d) & grepl("^\\d{4}-Q[1-4]$", x)
  d[bad] <- as.Date(as.yearqtr(x[bad], "%Y-Q%q"), frac = 1)
  
  bad <- is.na(d) & grepl("^\\d{4}$", x)
  d[bad] <- as.Date(sprintf("%s-12-31", x[bad]))
  
  d
}

# ============================================================
# Country panels: annual debt components (country units)
# ============================================================

# ------------------------------------------------------------
# UNITED STATES (USD bn)
# ------------------------------------------------------------
build_us_ann <- function() {
  start_date <- as.Date("1980-01-01")
  
  # Federal + state & local government gross debt
  fed_q <- fredr(series_id = "GFDEBTN", observation_start = start_date) %>%
    transmute(date, value) %>%
    to_q_end_bn() %>%
    rename(fed_bn = value_bn)
  
  # State and Local Governments; Debt Securities and Loans; Liability, Level 
  slg_q <- fredr(series_id = "SLGSDODNS", observation_start = start_date) %>%
    transmute(date, slg_bn = value) %>%
    mutate(date = as.Date(as.yearqtr(date), frac = 1))
  
  # Combines the above two
  pd_q <- inner_join(fed_q, slg_q, by = "date") %>%
    transmute(date, PD_bn = fed_bn + slg_bn)
  
  # Public external debt: Treasury + municipal holdings by rest of the world
  treas_q <- fredr(series_id = "ROWTSEQ027S", observation_start = start_date) %>%
    transmute(date, value) %>%
    to_q_end_bn() %>%
    rename(treas_bn = value_bn)
  munis_q <- fredr(series_id = "ROWMLAQ027S", observation_start = start_date) %>%
    transmute(date, value) %>%
    to_q_end_bn() %>%
    rename(munis_bn = value_bn)
  
  # Combines the above two
  ped_q <- full_join(treas_q, munis_q, by = "date") %>%
    mutate(across(c(treas_bn, munis_bn), ~ coalesce(.x, 0))) %>%
    mutate(PED_bn = treas_bn + munis_bn) %>%
    select(date, PED_bn)
  
  # Central bank holdings (Fed)
  cb_q <- fredr(series_id = "BOGZ1FL713061103Q", observation_start = start_date) %>%
    transmute(date, value) %>%
    to_q_end_bn() %>%
    rename(CB_bn = value_bn)
  
  # Currency and reserves
  cur_q <- fredr(series_id = "CURRCIR", observation_start = start_date) %>%
    transmute(date, value = value) %>%
    to_q_end_bn() %>%
    rename(currency_bn = value_bn)
  res_q <- fredr(series_id = "WRESBAL", observation_start = start_date) %>%
    transmute(date, value = value) %>%
    to_q_end_bn() %>%
    rename(reserves_bn = value_bn)
  
  # Combine quarterly series
  q_df <- pd_q %>%
    left_join(ped_q, by = "date") %>%
    left_join(cb_q,  by = "date") %>%
    left_join(cur_q, by = "date") %>%
    left_join(res_q, by = "date") %>%
    mutate(
      across(c(PED_bn, CB_bn, currency_bn, reserves_bn), ~ coalesce(.x, 0)),
      PLH_bn         = PD_bn - PED_bn - CB_bn,
      CB_residual_bn = CB_bn - currency_bn - reserves_bn
    )
  
  # Maturity shares of Fed holdings (for PLH split)
  fed_st <- fredr(series_id = "BOGZ1FL313161110Q", observation_start = as.Date("1984-01-01")) %>%
    transmute(date, fed_ST_bn = value / 1e3)
  fed_lt <- fredr(series_id = "BOGZ1FL313161275Q", observation_start = as.Date("1984-01-01")) %>%
    transmute(date, fed_LT_excl_nonmkt_bn = value / 1e3)
  fed_nonmkt <- fredr(series_id = "BOGZ1FL313161305Q", observation_start = as.Date("1984-01-01")) %>%
    transmute(date, fed_nonmkt_bn = value / 1e3)
  
  # Get the maturity ratio
  fed_mty <- reduce(list(fed_st, fed_lt, fed_nonmkt), full_join, by = "date") %>%
    mutate(across(-date, ~ coalesce(.x, 0))) %>%
    mutate(
      fed_LT_bn    = fed_LT_excl_nonmkt_bn + fed_nonmkt_bn,
      fed_total_bn = fed_ST_bn + fed_LT_bn
    ) %>%
    transmute(
      Year = year(date),
      Q    = quarter(date),
      st_share = fed_ST_bn / fed_total_bn,
      lt_share = fed_LT_bn / fed_total_bn
    ) %>%
    group_by(Year) %>%
    slice_max(Q, with_ties = FALSE) %>%
    ungroup() %>%
    select(Year, st_share, lt_share)
  
  # Collapse to annual (Q4) and apply maturity split to PLH
  q_df %>%
    mutate(Year = year(date), Q = quarter(date)) %>%
    group_by(Year) %>%
    slice_max(Q, with_ties = FALSE) %>%
    ungroup() %>%
    filter(Q == 4, Year >= 1984) %>%
    left_join(fed_mty, by = "Year") %>%
    mutate(
      PLH_ST_bn = PLH_bn * st_share,
      PLH_LT_bn = PLH_bn * lt_share
    ) %>%
    select(
      Year, PD_bn, PED_bn, CB_bn,
      currency_bn, reserves_bn, CB_residual_bn,
      PLH_ST_bn, PLH_LT_bn
    ) %>%
    arrange(Year)
}

# ------------------------------------------------------------
# JAPAN (¥ trillion)
# ------------------------------------------------------------
build_jp_ann <- function() {
  # QPSD database (public sector debt statistics)
  qpsd_raw <- read_csv(fp("Japan/japan_public_debt.csv"), show_col_types = FALSE) %>%
    clean_names()
  
  time_cols <- names(qpsd_raw)[grepl("^x\\d{4}q[1-4]", names(qpsd_raw))]
  
  # Long format, quarterly
  qpsd_long <- qpsd_raw %>%
    rename(
      country_name = coalesce(names(.)[match("country_name", names(.))], "country_name"),
      country_code = coalesce(names(.)[match("country_code", names(.))], "country_code"),
      series_name  = coalesce(names(.)[match("series_name",  names(.))], "series_name"),
      series_code  = coalesce(names(.)[match("series_code",  names(.))], "series_code")
    ) %>%
    pivot_longer(
      cols      = all_of(time_cols),
      names_to  = "col",
      values_to = "value"
    ) %>%
    mutate(
      yq_str = stringr::str_extract(col, "\\d{4}q[1-4]"),
      yq     = as.yearqtr(
        toupper(stringr::str_replace(yq_str, "q", " Q")),
        format = "%Y Q%q"
      ),
      date = as.Date(yq, frac = 1),
      Year = year(date),
      Q    = quarter(date),
      value = suppressWarnings(as.numeric(value))
    ) %>%
    filter(!is.na(Year), !is.na(value))
  
  # Take Q4 for each series
  qpsd_q4 <- qpsd_long %>%
    group_by(series_name, Year) %>%
    slice_max(Q, with_ties = FALSE) %>%
    ungroup()
  
  # Map QPSD series into PD, PED, and ST
  psd_map <- qpsd_q4 %>%
    mutate(bucket = case_when(
      stringr::str_detect(
        series_name,
        regex("All maturities.*All instruments.*External creditors.*Nominal Value.*National Currency", TRUE)
      ) ~ "PED",
      stringr::str_detect(
        series_name,
        regex("^Gross PSD.*All maturities.*All instruments.*Nominal Value.*National Currency$", TRUE)
      ) ~ "PD",
      stringr::str_detect(
        series_name,
        regex("^Gross PSD.*Short-term.*All instruments", TRUE)
      ) ~ "ST",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(bucket)) %>%
    select(Year, bucket, value) %>%
    distinct() %>%
    pivot_wider(names_from = bucket, values_from = value)
  
  # Convert to ¥ trillion and derive maturity shares
  psd_trn <- psd_map %>%
    transmute(
      Year,
      PD_trn  = PD  / 1e12,
      PED_trn = PED / 1e12,
      ST_trn  = ST  / 1e12,
      LT_trn  = (PD - ST) / 1e12,
      st_share = ST / PD,
      lt_share = 1 - st_share
    )
  
  # BoJ holdings (total CB); Do note that the values in 'Summarized.xlsx' take the sum value for each of the country's individual sheet
  cb_trn <- read_excel(fp("Japan/Japan CB Holdings Data/Summarized.xlsx")) %>%
    select(1, 3) %>%
    rename(Year = 1, CB_yen = 2) %>%
    mutate(
      Year   = as.integer(Year),
      CB_trn = as.numeric(CB_yen) / 1e12
    ) %>%
    group_by(Year) %>%
    summarise(CB_trn = dplyr::last(CB_trn), .groups = "drop")
  
  # Currency and reserves (BoJ balance sheet)
  base_q4 <- read_csv(fp("Japan/japan_currency_reserves.csv"), show_col_types = FALSE) %>%
    select(
      Date = "Name of time-series",
      currency = "Liabilities/-Currency/Central bank/Stock",
      reserves = "Liabilities/-Deposits with the Bank of Japan/Central bank/Stock"
    ) %>%
    mutate(
      date = as.Date(paste0(Date, "/01"), format = "%Y/%m/%d"),
      Year = year(date),
      currency_trn = as.numeric(currency) / 1e4,
      reserves_trn = as.numeric(reserves) / 1e4
    ) %>%
    group_by(Year) %>%
    slice_max(date, with_ties = FALSE) %>%
    ungroup() %>%
    select(Year, currency_trn, reserves_trn)
  
  # Assemble annual panel
  jp_ann <- psd_trn %>%
    left_join(cb_trn,  by = "Year") %>%
    left_join(base_q4, by = "Year") %>%
    mutate(
      CB_trn  = coalesce(CB_trn, 0),
      PED_trn = coalesce(PED_trn, 0),
      PLH_trn = PD_trn - PED_trn - CB_trn,
      lt_share = LT_trn / (LT_trn + ST_trn),
      st_share = ST_trn / (LT_trn + ST_trn),
      PLH_LT_trn = PLH_trn * lt_share,
      PLH_ST_trn = PLH_trn * st_share,
      CB_residual_trn = CB_trn - currency_trn - reserves_trn
    ) %>%
    arrange(Year) %>%
    filter(Year <= 2024)
  
  jp_ann
}

# ------------------------------------------------------------
# UNITED KINGDOM (GBP bn)
# ------------------------------------------------------------
build_uk_ann <- function() {
  
  # 1. Helpers (UK-specific parsing)
  to_friday <- function(x) {
    d <- lubridate::parse_date_time(
      x,
      orders = c("d b y", "d b Y", "d B y", "d B Y", "Y-m-d")
    )
    floor_date(d, unit = "week", week_start = 1) + days(4)
  }
  
  parse_quarter_col <- function(x) {
    y <- stringr::str_trim(as.character(x))
    y <- ifelse(stringr::str_detect(y, "^\\d{4}$"), paste0(y, " Q4"), y)
    y <- stringr::str_replace_all(
      y,
      c("Q" = " Q", "-" = " ", "—" = " ", "–" = " ")
    )
    as.Date(as.yearqtr(y, format = "%Y Q%q"), frac = 1)
  }
  
  parse_year_only <- function(df, value_name) {
    names(df)[1:2] <- c("Period", "Value")
    df %>%
      mutate(Period = stringr::str_trim(Period)) %>%
      filter(stringr::str_detect(Period, "^\\d{4}$")) %>%
      transmute(
        Year = as.integer(Period),
        !!value_name := as.numeric(gsub(",", "", Value))
      ) %>%
      arrange(Year)
  }
  
  # 2. Load UK sources
  UK_LT_CG    <- read_csv(fp("UK/uk_lt_cg.csv"))        %>% slice(-c(1:7))
  UK_ST_CG    <- read_csv(fp("UK/uk_st_cg.csv"))        %>% slice(-c(1:7))
  UK_LT_LG    <- read_csv(fp("UK/uk_lt_lg.csv"))        %>% slice(-c(1:7))
  UK_ST_LG    <- read_csv(fp("UK/uk_st_lg.csv"))        %>% slice(-c(1:7))
  UK_ST_Loans <- read_csv(fp("UK/uk_st_loans.csv"))     %>% slice(-c(1:7))
  UK_LT_Loans <- read_csv(fp("UK/uk_lt_loans.csv"))     %>% slice(-c(1:7))
  
  UK_PublicDebt       <- read_csv(fp("UK/uk_publicDebt.csv")) %>% slice(-c(1:7))
  UK_reserves_monthly <- read_csv(fp("UK/uk_reserves_monthly.csv"))
  UK_notes            <- read_csv(fp("UK/uk_notes.csv"))
  UK_liabilities         <- read_csv(fp("UK/uk_liabilities.csv")) %>%
    slice(-c(1:7))
  colnames(UK_liabilities) <- c("date", "value")
  
  uk_ext <- read_excel(fp("UK/uk_external.xlsx"), sheet = "necessary", skip = 1) %>%
    filter(!is.na(CDID)) %>%
    mutate(
      Year = as.integer(CDID),
      across(
        c(HLYU, HHHD, HHGF, HHGZ, HHHA),
        ~ readr::parse_number(as.character(.))
      ),
      PED_bn = HLYU + HHHD + HHGF + HHGZ + HHHA
    ) %>%
    select(Year, PED_bn)
  
  
  # 3. Public debt (PD)
  uk_pd <- UK_PublicDebt %>%
    rename(Year = 1, pd_m = 2) %>%
    mutate(
      Year  = as.integer(Year),
      PD_bn = as.numeric(pd_m) / 1e3
    ) %>%
    filter(!is.na(Year), !is.na(PD_bn)) %>%
    select(Year, PD_bn)
  
  
  # 4. Currency in circulation & reserves
  notes_clean <- UK_notes %>%
    rename(date_chr = 1, notes_m = 2) %>%
    mutate(date = to_friday(date_chr)) %>%
    group_by(date) %>%
    summarise(notes_m = dplyr::last(notes_m[!is.na(notes_m)]), .groups = "drop")
  
  reserves_clean <- UK_reserves_monthly %>%
    clean_names() %>%
    rename(date_chr = 1, reserves_m = dplyr::last_col()) %>%
    mutate(date = to_friday(date_chr)) %>%
    group_by(date) %>%
    summarise(reserves_m = dplyr::last(reserves_m[!is.na(reserves_m)]), .groups = "drop")
  
  base_q <- inner_join(notes_clean, reserves_clean, by = "date") %>%
    mutate(date = as.Date(as.yearqtr(date), frac = 1)) %>%
    group_by(date) %>%
    summarise(
      notes_m    = dplyr::last(notes_m),
      reserves_m = dplyr::last(reserves_m),
      .groups    = "drop"
    ) %>%
    transmute(
      Year        = year(date),
      Q           = quarter(date),
      currency_bn = notes_m    / 1e3,
      reserves_bn = reserves_m / 1e3
    ) %>%
    group_by(Year) %>%
    slice_max(Q, with_ties = FALSE) %>%
    ungroup() %>%
    select(Year, currency_bn, reserves_bn)
  
  # 5. Central bank gilt holdings (APF)
  apf_clean <- UK_liabilities %>%
    rename(apf_col1 = 1, apf_gilts_m = 2) %>%
    mutate(
      date        = suppressWarnings(parse_quarter_col(apf_col1)),
      apf_gilts_m = as.numeric(apf_gilts_m)
    ) %>%
    filter(!is.na(date)) %>%
    arrange(date) %>%
    transmute(
      Year  = year(date),
      Q     = quarter(date),
      CB_bn = apf_gilts_m / 1e3
    ) %>%
    group_by(Year) %>%
    slice_max(Q, with_ties = FALSE) %>%
    ungroup() %>%
    select(Year, CB_bn)
  
  
  # 6. Annual UK debt table
  uk_ann <- uk_pd %>%
    left_join(uk_ext,    by = "Year") %>%
    left_join(apf_clean, by = "Year") %>%
    left_join(base_q,    by = "Year") %>%
    mutate(
      PED_bn = coalesce(PED_bn, 0),
      CB_bn  = coalesce(CB_bn, 0),
      PLH_bn = PD_bn - PED_bn - CB_bn,
      CB_residual_bn = CB_bn - coalesce(currency_bn, 0) - coalesce(reserves_bn, 0)
    ) %>%
    arrange(Year)
  
  
  # 7. Maturity shares for PLH split
  cg_lt <- parse_year_only(UK_LT_CG,     "CG_LT_mn")
  cg_st <- parse_year_only(UK_ST_CG,     "CG_ST_mn")
  lg_lt <- parse_year_only(UK_LT_LG,     "LG_LT_mn")
  lg_st <- parse_year_only(UK_ST_LG,     "LG_ST_mn")
  lo_lt <- parse_year_only(UK_LT_Loans,  "LOANS_LT_mn")
  lo_st <- parse_year_only(UK_ST_Loans,  "LOANS_ST_mn")
  
  mty_shares <- cg_lt %>%
    full_join(cg_st, by = "Year") %>%
    full_join(lg_lt, by = "Year") %>%
    full_join(lg_st, by = "Year") %>%
    full_join(lo_lt, by = "Year") %>%
    full_join(lo_st, by = "Year") %>%
    mutate(
      across(-Year, ~ coalesce(.x, 0)),
      sec_LT_mn  = CG_LT_mn   + LG_LT_mn,
      sec_ST_mn  = CG_ST_mn   + LG_ST_mn,
      loan_LT_mn = LOANS_LT_mn,
      loan_ST_mn = LOANS_ST_mn,
      GG_LT_mn   = sec_LT_mn + loan_LT_mn,
      GG_ST_mn   = sec_ST_mn + loan_ST_mn,
      GG_total_mn = GG_LT_mn + GG_ST_mn,
      lt_share    = if_else(GG_total_mn > 0, GG_LT_mn / GG_total_mn, NA_real_),
      st_share    = if_else(GG_total_mn > 0, GG_ST_mn / GG_total_mn, NA_real_)
    ) %>%
    select(Year, lt_share, st_share) %>%
    arrange(Year)
  
  
  # 8. Apply maturity split to PLH
  uk_ann %>%
    left_join(mty_shares, by = "Year") %>%
    mutate(
      PLH_LT_bn = PLH_bn * lt_share,
      PLH_ST_bn = PLH_bn * st_share
    ) %>%
    select(
      Year, PD_bn, PED_bn, CB_bn,
      currency_bn, reserves_bn, CB_residual_bn,
      PLH_ST_bn, PLH_LT_bn
    )
}

# ------------------------------------------------------------
# GERMANY (CB unsplit; € bn)
# ------------------------------------------------------------
build_de_ann <- function() {
  # Helper: last available quarter of each year, values in € bn
  q4_last_bn <- function(df, date_col, value_col) {
    df %>%
      mutate(Y = year({{ date_col }}), Q = quarter({{ date_col }})) %>%
      group_by(Y) %>%
      slice_max(Q, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(
        Year     = Y,
        value_bn = {{ value_col }} / 1e3
      )
  }
  
  # General government gross debt
  DE_public_debt <- eurostat::get_eurostat("gov_10q_ggdebt", time_format = "date") %>%
    filter(
      geo    == "DE",
      sector == "S13",
      na_item == "GD",
      unit   == "MIO_EUR"
    ) %>%
    transmute(
      date  = TIME_PERIOD,
      value = values
    ) %>%
    q4_last_bn(date, value) %>%
    rename(PD_eur_bn = value_bn)
  
  # Central bank holdings of government debt
  DE_CB_debt <- read_csv(fp("Germany/germany_CB_debt.csv"), show_col_types = FALSE) %>%
    select(yq_chr = "TIME PERIOD", CB_raw = 3) %>%
    mutate(
      yq   = as.yearqtr(yq_chr, "%Y-Q%q"),
      date = as.Date(yq, frac = 1),
      Year = year(date),
      Q    = quarter(date)
    ) %>%
    group_by(Year) %>%
    slice_max(Q, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(Year, CB_eur_bn = CB_raw / 1e3)
  
  # Public external debt
  DE_PED <- read_csv(fp("Germany/germany_ext_debt.csv"), na = c("", ".", "NA"), show_col_types = FALSE) %>%
    mutate(
      gross_ext_debt_num = readr::parse_number(as.character(gross_ext_debt)),
      yq   = as.yearqtr(gsub("[–—−]", "-", time), "%Y-Q%q"),
      date = as.Date(yq, frac = 1)
    ) %>%
    q4_last_bn(date, gross_ext_debt_num) %>%
    rename(PED_eur_bn = value_bn)
  
  # Assemble annual table
  DE_ann <- DE_public_debt %>%
    left_join(DE_PED,    by = "Year") %>%
    left_join(DE_CB_debt, by = "Year") %>%
    mutate(
      PED_eur_bn = coalesce(PED_eur_bn, 0),
      CB_eur_bn  = coalesce(CB_eur_bn, 0),
      PLH_eur_bn = PD_eur_bn - PED_eur_bn - CB_eur_bn
    ) %>%
    arrange(Year)
  
  # Short/long maturity shares from Eurostat; 
  # Little summary: 
  # - GDF31: Government consolidated gross debt at face value - short-term debt securities
  # - GDF32: Government consolidated gross debt at face value - long-term debt securities
  # - GDF41: Government consolidated gross debt at face value - short-term loans
  # - GDF42: Government consolidated gross debt at face value - long-term loans
  DE_stlt <- eurostat::get_eurostat("gov_10dd_edpt1", time_format = "date") %>%
    filter(
      geo    == "DE",
      sector == "S13",
      unit   == "MIO_EUR",
      na_item %in% c("GD_F31", "GD_F32", "GD_F41", "GD_F42")
    ) %>%
    mutate(
      Year   = year(TIME_PERIOD),
      bucket = case_when(
        na_item %in% c("GD_F31", "GD_F41") ~ "ST",
        na_item %in% c("GD_F32", "GD_F42") ~ "LT"
      )
    ) %>%
    group_by(Year, bucket) %>%
    summarise(value_eur_m = sum(values, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = bucket, values_from = value_eur_m) %>%
    mutate(
      total    = ST + LT,
      st_share = if_else(total > 0, ST / total, NA_real_)
    ) %>%
    transmute(
      Year,
      st_share = pmin(pmax(st_share, 0), 1)
    )
  
  # Apply maturity split to PLH
  DE_ann %>%
    left_join(DE_stlt, by = "Year") %>%
    mutate(
      lt_share        = if_else(!is.na(st_share), 1 - st_share, NA_real_),
      PLH_ST_eur_bn   = PLH_eur_bn * st_share,
      PLH_LT_eur_bn   = PLH_eur_bn * lt_share
    ) %>%
    transmute(
      Year,
      PD_bn       = PD_eur_bn,
      PED_bn      = PED_eur_bn,
      CB_bn       = CB_eur_bn,
      PLH_ST_bn   = PLH_ST_eur_bn,
      PLH_LT_bn   = PLH_LT_eur_bn
    )
}

# ------------------------------------------------------------
# FRANCE (CB unsplit; € bn)
# ------------------------------------------------------------
build_fr_ann <- function() {
  q4_last_bn <- function(df, date_col, value_col) {
    df %>%
      mutate(Y = year({{ date_col }}), Q = quarter({{ date_col }})) %>%
      group_by(Y) %>%
      slice_max(Q, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(
        Year     = Y,
        value_bn = {{ value_col }} / 1e3
      )
  }
  
  # General government gross debt
  FR_public_debt <- eurostat::get_eurostat("gov_10q_ggdebt", time_format = "date") %>%
    filter(
      geo    == "FR",
      sector == "S13",
      na_item == "GD",
      unit   == "MIO_EUR"
    ) %>%
    transmute(
      date  = TIME_PERIOD,
      value = values
    ) %>%
    q4_last_bn(date, value) %>%
    rename(PD_eur_bn = value_bn)
  
  # Central bank holdings
  FR_CB_debt <- read_csv(fp("France/France_CB.csv"), show_col_types = FALSE) %>%
    select(yq_chr = "TIME PERIOD", CB_raw = 3) %>%
    mutate(
      yq   = as.yearqtr(yq_chr, "%Y-Q%q"),
      date = as.Date(yq, frac = 1),
      Year = year(date),
      Q    = quarter(date)
    ) %>%
    group_by(Year) %>%
    slice_max(Q, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(Year, CB_eur_bn = as.numeric(CB_raw) / 1e3)
  
  # Foreign-ownership share of negotiable debt
  FR_nr_annual <- read_excel(fp("France/France_external.xlsx")) %>%
    transmute(
      Year = as.integer(substr(time_period, 1, 4)),
      f_nr = readr::parse_number(obs_value) / 100
    ) %>%
    group_by(Year) %>%
    summarise(f_nr = dplyr::last(f_nr), .groups = "drop")
  
  # Negotiable general government debt (Eurostat)
  FR_neg_annual <- eurostat::get_eurostat("gov_10dd_edpt1", time_format = "date") %>%
    filter(
      geo    == "FR",
      sector == "S13",
      na_item == "GD_F3",
      unit   == "MIO_EUR"
    ) %>%
    transmute(
      Year              = year(TIME_PERIOD),
      negotiable_eur_bn = values / 1e3
    ) %>%
    arrange(Year)
  
  # Public external debt = negotiable * foreign share; (Note we exclude non-negotiable debt, which is a small share)
  FR_PED_annual <- FR_neg_annual %>%
    inner_join(FR_nr_annual, by = "Year") %>%
    mutate(PED_eur_bn = negotiable_eur_bn * f_nr) %>%
    select(Year, PED_eur_bn)
  
  # Short/long maturity shares
  FR_stlt <- eurostat::get_eurostat("gov_10dd_edpt1", time_format = "date") %>%
    filter(
      geo    == "FR",
      sector == "S13",
      unit   == "MIO_EUR",
      na_item %in% c("GD_F31", "GD_F32", "GD_F41", "GD_F42")
    ) %>%
    transmute(
      Year   = year(TIME_PERIOD),
      na_item,
      val_m  = values
    ) %>%
    mutate(bucket = case_when(
      na_item %in% c("GD_F31", "GD_F41") ~ "ST",
      na_item %in% c("GD_F32", "GD_F42") ~ "LT"
    )) %>%
    group_by(Year, bucket) %>%
    summarise(val_m = sum(val_m, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = bucket, values_from = val_m) %>%
    mutate(
      st_share = if_else(ST + LT > 0, ST / (ST + LT), NA_real_),
      lt_share = 1 - st_share
    ) %>%
    select(Year, st_share, lt_share)
  
  # Assemble annual table and apply maturity split to PLH
  FR_public_debt %>%
    left_join(FR_CB_debt,  by = "Year") %>%
    left_join(FR_PED_annual, by = "Year") %>%
    left_join(FR_stlt,       by = "Year") %>%
    mutate(
      PED_eur_bn    = coalesce(PED_eur_bn, 0),
      CB_eur_bn     = coalesce(CB_eur_bn, 0),
      PLH_eur_bn    = PD_eur_bn - PED_eur_bn - CB_eur_bn,
      PLH_ST_eur_bn = PLH_eur_bn * st_share,
      PLH_LT_eur_bn = PLH_eur_bn * lt_share
    ) %>%
    arrange(Year) %>%
    transmute(
      Year,
      PD_bn     = PD_eur_bn,
      PED_bn    = PED_eur_bn,
      CB_bn     = CB_eur_bn,
      PLH_ST_bn = PLH_ST_eur_bn,
      PLH_LT_bn = PLH_LT_eur_bn
    )
}


# ------------------------------------------------------------
# ITALY (CB unsplit; € bn)
# ------------------------------------------------------------
build_it_ann <- function() {
  q4_last_bn <- function(df, date_col, value_col) {
    df %>%
      mutate(Y = year({{ date_col }}), Q = quarter({{ date_col }})) %>%
      group_by(Y) %>%
      slice_max(Q, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(
        Year     = Y,
        value_bn = {{ value_col }} / 1e3
      )
  }
  
  # General government gross debt
  IT_public_debt <- eurostat::get_eurostat("gov_10q_ggdebt", time_format = "date") %>%
    filter(
      geo    == "IT",
      sector == "S13",
      na_item == "GD",
      unit   == "MIO_EUR"
    ) %>%
    transmute(
      date  = TIME_PERIOD,
      value = values
    ) %>%
    q4_last_bn(date, value) %>%
    rename(PD_eur_bn = value_bn)
  
  # Central bank holdings
  IT_CB_debt <- read_csv(fp("Italy/Italy_CB.csv"), show_col_types = FALSE) %>%
    select(yq_chr = "TIME PERIOD", CB_raw = 3) %>%
    mutate(
      yq   = as.yearqtr(yq_chr, "%Y-Q%q"),
      date = as.Date(yq, frac = 1),
      Year = year(date),
      Q    = quarter(date)
    ) %>%
    group_by(Year) %>%
    slice_max(Q, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(Year, CB_eur_bn = as.numeric(CB_raw) / 1e3)
  
  # Public external debt from monthly series
  it_raw <- read_excel(fp("Italy/Italy_PED.xlsx"), skip = 3)
  
  date_col <- it_raw %>%
    select(where(~ is.character(.x) || is.factor(.x))) %>%
    names() %>%
    purrr::keep(~ any(str_detect(it_raw[[.x]], "^\\d{4}-[A-Za-z]{3}$"), na.rm = TRUE)) %>%
    dplyr::first()
  val_col <- it_raw %>%
    select(where(~ is.numeric(.x) || all(grepl("^\\s*[-\\d\\.,]+\\s*$", .x %||% ""), na.rm = TRUE))) %>%
    names() %>%
    dplyr::last()
  
  # Clean up the data
  it_monthly <- it_raw %>%
    transmute(
      date_str = .data[[date_col]],
      value_m  = readr::parse_number(as.character(.data[[val_col]]))
    ) %>%
    filter(!is.na(date_str), !is.na(value_m)) %>%
    mutate(
      date = as.Date(zoo::as.yearmon(date_str, "%Y-%b")),
      ym   = floor_date(date, "month")
    ) %>%
    group_by(ym) %>%
    summarise(value_m = dplyr::last(value_m), .groups = "drop")
  
  IT_PED <- it_monthly %>%
    mutate(
      Y = year(ym),
      Q = quarter(ym)
    ) %>%
    group_by(Y, Q) %>%
    slice_max(ym, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      date       = ym,
      Year       = Y,
      Q,
      PED_eur_bn = value_m / 1e3
    ) %>%
    group_by(Year) %>%
    slice_max(Q, with_ties = FALSE) %>%
    ungroup() %>%
    filter(Q == 4) %>%
    select(Year, PED_eur_bn) %>%
    arrange(Year)
  
  # Assemble annual table
  IT_ann <- IT_public_debt %>%
    left_join(IT_PED,    by = "Year") %>%
    left_join(IT_CB_debt, by = "Year") %>%
    mutate(
      PED_eur_bn = coalesce(PED_eur_bn, 0),
      CB_eur_bn  = coalesce(CB_eur_bn, 0),
      PLH_eur_bn = PD_eur_bn - PED_eur_bn - CB_eur_bn
    ) %>%
    arrange(Year)
  
  # Short/long maturity shares
  IT_stlt <- eurostat::get_eurostat("gov_10dd_edpt1", time_format = "date") %>%
    filter(
      geo    == "IT",
      sector == "S13",
      unit   == "MIO_EUR",
      na_item %in% c("GD_F31", "GD_F32", "GD_F41", "GD_F42")
    ) %>%
    mutate(
      Year   = year(TIME_PERIOD),
      bucket = case_when(
        na_item %in% c("GD_F31", "GD_F41") ~ "ST",
        na_item %in% c("GD_F32", "GD_F42") ~ "LT"
      )
    ) %>%
    group_by(Year, bucket) %>%
    summarise(val_m = sum(values, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = bucket, values_from = val_m) %>%
    mutate(
      total    = ST + LT,
      st_share = if_else(total > 0, ST / total, NA_real_)
    ) %>%
    transmute(
      Year,
      st_share = pmin(pmax(st_share, 0), 1)
    )
  
  # Apply maturity split to PLH
  IT_ann %>%
    left_join(IT_stlt, by = "Year") %>%
    mutate(
      lt_share        = if_else(!is.na(st_share), 1 - st_share, NA_real_),
      PLH_ST_eur_bn   = PLH_eur_bn * st_share,
      PLH_LT_eur_bn   = PLH_eur_bn * lt_share
    ) %>%
    transmute(
      Year,
      PD_bn     = PD_eur_bn,
      PED_bn    = PED_eur_bn,
      CB_bn     = CB_eur_bn,
      PLH_ST_bn = PLH_ST_eur_bn,
      PLH_LT_bn = PLH_LT_eur_bn
    )
}


# ------------------------------------------------------------
# CANADA (CB split; C$ bn)
# ------------------------------------------------------------
build_ca_ann <- function() {
  
  ## HELPERS Specific for Canada
  # Helper: last Q4 observation, keep Year and value in original units
  q4_last <- function(df, date_col) {
    df %>%
      mutate(
        Year = year({{ date_col }}),
        Q    = quarter({{ date_col }})
      ) %>%
      group_by(Year) %>%
      slice_max(Q, with_ties = FALSE) %>%
      ungroup() %>%
      filter(Q == 4) %>%
      select(-Q)
  }
  
  parse_b2_date <- function(x) {
    d <- suppressWarnings(lubridate::dmy(x))
    d[is.na(d)] <- suppressWarnings(lubridate::ymd(x[is.na(d)]))
    d
  }
  
  get_or_na <- function(df, nm) {
    if (nm %in% names(df)) {
      suppressWarnings(as.numeric(df[[nm]]))
    } else {
      NA_real_
    }
  }
  
  
  # 1. Public debt from PD/GDP × GDP
  PD_gdp_pct <- get_cansim("38-10-0237-01", factors = FALSE) %>%
    filter(
      GEO       == "Canada",
      Categories == "General government gross debt to gross domestic product (GDP)"
    ) %>%
    transmute(
      date = coalesce(
        if ("Date" %in% names(.)) Date else as.Date(NA),
        parse_statcan_ref_date(REF_DATE)
      ),
      Year           = year(date),
      PD_to_GDP_pct  = as.numeric(VALUE)
    ) %>%
    group_by(Year) %>%
    summarise(PD_to_GDP_pct = dplyr::last(PD_to_GDP_pct), .groups = "drop")
  
  GDP_annual <- get_cansim("36-10-0699-01", factors = FALSE) %>%
    filter(
      GEO       == "Canada",
      Estimates == "Gross domestic product at market prices",
      Prices    == "Current Prices"
    ) %>%
    transmute(
      date = coalesce(
        if ("Date" %in% names(.)) Date else as.Date(NA),
        parse_statcan_ref_date(REF_DATE)
      ),
      Year        = year(date),
      GDP_cad_bn  = as.numeric(VALUE) / 1e3
    )
  
  CA_PD <- PD_gdp_pct %>%
    inner_join(GDP_annual, by = "Year") %>%
    mutate(PD_bn = (PD_to_GDP_pct / 100) * GDP_cad_bn) %>%
    select(Year, PD_bn)
  
  
  # 2. Public external debt
  CA_PED <- get_cansim("36-10-0469-01", factors = FALSE) %>%
    filter(
      GEO == "Canada",
      Sector == "General government",
      Valuation == "Book value",
      `Type of instrument and direct investment lending` == "Total, all types of instruments"
    ) %>%
    transmute(
      date = coalesce(
        if ("Date" %in% names(.)) Date else as.Date(NA),
        parse_statcan_ref_date(REF_DATE)
      ),
      value_bn = as.numeric(VALUE) / 1e3
    ) %>%
    q4_last(date) %>%
    transmute(Year, PED_bn = value_bn)
  
  
  # 3. Central bank holdings, currency, reserves (BoC)
  Canada_CB <- read_csv(fp("Canada/Canada_CB.csv"), show_col_types = FALSE)
  
  cb_q4 <- Canada_CB %>%
    mutate(date = parse_b2_date(date)) %>%
    transmute(
      date,
      TBills_m     = get_or_na(., "V36612"),
      GoC_Bonds_m  = get_or_na(., "V36613"),
      RRBonds_m    = get_or_na(., "V1160788296"),
      CMBs_m       = get_or_na(., "V1038114416"),
      Prov_MM_m    = get_or_na(., "v1146067261"),
      Prov_Bonds_m = get_or_na(., "v1154426989"),
      Notes_m      = get_or_na(., "V36625"),
      Members_m    = get_or_na(., "V36636")
    ) %>%
    mutate(
      CB_total_GoC_m =
        coalesce(TBills_m, 0) +
        coalesce(GoC_Bonds_m, 0) +
        coalesce(RRBonds_m, 0),
      CB_total_GG_m  = CB_total_GoC_m + coalesce(Prov_MM_m, 0) + coalesce(Prov_Bonds_m, 0),
      Currency_m     = Notes_m,
      Reserves_m     = Members_m,
      Y = year(date),
      Q = quarter(date)
    ) %>%
    group_by(Y, Q) %>%
    slice_max(date, with_ties = FALSE) %>%
    ungroup() %>%
    group_by(Y) %>%
    slice_max(Q, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      Year        = Y,
      CB_bn       = CB_total_GG_m / 1e3,
      currency_bn = Currency_m    / 1e3,
      reserves_bn = Reserves_m    / 1e3
    ) %>%
    mutate(
      CB_residual_bn = CB_bn - coalesce(currency_bn, 0) - coalesce(reserves_bn, 0)
    ) %>%
    arrange(Year)
  
  
  # 4. Combine and compute PLH
  ann <- CA_PD %>%
    left_join(CA_PED, by = "Year") %>%
    left_join(cb_q4,  by = "Year") %>%
    mutate(
      across(
        c(PED_bn, CB_bn, currency_bn, reserves_bn, CB_residual_bn),
        ~ coalesce(.x, 0)
      ),
      PLH_bn = PD_bn - PED_bn - CB_bn
    )
  
  
  # 5. PLH short/long split from remaining maturity distribution
  can <- read_csv(fp("Canada/Canada_split.csv"), show_col_types = FALSE) %>%
    mutate(
      date = dmy(date),
      across(-date, as.numeric)
    ) %>%
    rename(
      t_bills         = V37331,
      canada_bills_usd = V37323_G6,
      bonds_le_3y     = V37332,
      bonds_3_5y      = V37333,
      bonds_5_10y     = V37334,
      bonds_ge_10y    = V37335
    )
  
  
  # Using the above data (ST/LT securities), we split the PLH
  can_rm <- can %>%
    transmute(
      date,
      ST = coalesce(t_bills, 0) +
        coalesce(canada_bills_usd, 0) +
        coalesce(bonds_le_3y, 0),
      LT = coalesce(bonds_3_5y, 0) +
        coalesce(bonds_5_10y, 0) +
        coalesce(bonds_ge_10y, 0),
      total = ST + LT
    ) %>%
    mutate(
      ST_share = if_else(total > 0, ST / total, NA_real_),
      LT_share = if_else(total > 0, LT / total, NA_real_)
    ) %>%
    mutate(
      Y = year(date),
      Q = quarter(date)
    ) %>%
    filter(Q == 4) %>%
    transmute(
      Year    = Y,
      ST_share,
      LT_share
    ) %>%
    group_by(Year) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  ann %>%
    left_join(can_rm, by = "Year") %>%
    mutate(
      PLH_ST_bn = PLH_bn * ST_share,
      PLH_LT_bn = PLH_bn * LT_share
    ) %>%
    select(
      Year, PD_bn, PED_bn, CB_bn,
      currency_bn, reserves_bn, CB_residual_bn,
      PLH_ST_bn, PLH_LT_bn
    )
}


# ============================================================
# Build country panels
# ============================================================
US_ann <- build_us_ann()
JP_ann <- build_jp_ann()
UK_ann <- build_uk_ann()
DE_ann <- build_de_ann()
FR_ann <- build_fr_ann()
IT_ann <- build_it_ann()
CA_ann <- build_ca_ann()

# ============================================================
# GDP denominators (calendar-year sums; aligned units)
# ============================================================

# US: FRED "GDP" (billions SAAR) → quarterly level by /4; sum → $ bn
US_gdp <- fred_gdp_annual(
  series_id  = "GDP",
  is_saar    = TRUE,
  value_scale = 1
) %>%
  rename(GDP_bn = GDP)

# UK: FRED "UKNGDP" (quarterly level, £ m) → sum → £ m → £ bn
UK_gdp <- fred_gdp_annual(
  series_id  = "UKNGDP",
  is_saar    = FALSE,
  value_scale = 1e3
) %>%
  rename(GDP_bn = GDP)

# JP: FRED "JPNNGDP" (¥ bn SAAR) → quarterly level by /4; sum → ¥ bn → ¥ trn
JP_gdp <- fred_gdp_annual(
  series_id  = "JPNNGDP",
  is_saar    = TRUE,
  value_scale = 1
) %>%
  mutate(GDP_trn = GDP / 1e3) %>%
  select(Year, GDP_trn)

# DE/FR/IT: Eurostat, quarterly levels (CP_MEUR), sum → € bn
DE_gdp <- eu_annual_gdp("DE", out_name = "GDP_bn")
FR_gdp <- eu_annual_gdp("FR", out_name = "GDP_bn")
IT_gdp <- eu_annual_gdp("IT", out_name = "GDP_bn")

# CA: StatCan annual, current prices (to match debt units); millions → billions
CA_gdp <- get_cansim("36-10-0699-01", factors = FALSE) %>%
  filter(
    GEO       == "Canada",
    Estimates == "Gross domestic product at market prices",
    Prices    == "Current Prices"
  ) %>%
  transmute(
    date = coalesce(
      if ("Date" %in% names(.)) Date else as.Date(NA),
      parse_statcan_ref_date(REF_DATE)
    ),
    Year   = year(date),
    GDP_bn = as.numeric(VALUE) / 1e3
  ) %>%
  group_by(Year) %>%
  summarise(GDP_bn = dplyr::last(GDP_bn), .groups = "drop")

# ============================================================
# Convert components → % of GDP and window-average
# ============================================================

# Here, since some countries' values are left in billion and some in trillion, we standardise it over the GDP
to_gdp_ratios <- function(df, gdp_df, gdp_var = "GDP_bn") {
  df %>%
    left_join(gdp_df, by = "Year") %>%
    transmute(
      Year,
      GDP = .data[[gdp_var]],
      
      `Public external debt / GDP` =
        (if ("PED_bn" %in% names(.)) PED_bn else if ("PED_trn" %in% names(.)) PED_trn else NA_real_) / GDP,
      
      `Central bank — Currency / GDP` =
        (if ("currency_bn" %in% names(.)) currency_bn else if ("currency_trn" %in% names(.)) currency_trn else NA_real_) / GDP,
      
      `Central bank — Reserves / GDP` =
        (if ("reserves_bn" %in% names(.)) reserves_bn else if ("reserves_trn" %in% names(.)) reserves_trn else NA_real_) / GDP,
      
      `Central bank — Residual / GDP` =
        (if ("CB_residual_bn" %in% names(.)) CB_residual_bn else if ("CB_residual_trn" %in% names(.)) CB_residual_trn else NA_real_) / GDP,
      
      `Central bank / GDP` =
        (if ("CB_bn" %in% names(.)) CB_bn else if ("CB_trn" %in% names(.)) CB_trn else NA_real_) / GDP,
      
      `Private locally-held — Short / GDP` =
        (if ("PLH_ST_bn" %in% names(.)) PLH_ST_bn else if ("PLH_ST_trn" %in% names(.)) PLH_ST_trn else NA_real_) / GDP,
      
      `Private locally-held — Long / GDP` =
        (if ("PLH_LT_bn" %in% names(.)) PLH_LT_bn else if ("PLH_LT_trn" %in% names(.)) PLH_LT_trn else NA_real_) / GDP
    )
}

# Standardizing all components by GDP
US_gdp_norm <- to_gdp_ratios(US_ann, US_gdp, "GDP_bn")
UK_gdp_norm <- to_gdp_ratios(UK_ann, UK_gdp, "GDP_bn")
JP_gdp_norm <- to_gdp_ratios(JP_ann, JP_gdp, "GDP_trn")
DE_gdp_norm <- to_gdp_ratios(DE_ann, DE_gdp, "GDP_bn")
FR_gdp_norm <- to_gdp_ratios(FR_ann, FR_gdp, "GDP_bn")
IT_gdp_norm <- to_gdp_ratios(IT_ann, IT_gdp, "GDP_bn")
CA_gdp_norm <- to_gdp_ratios(CA_ann, CA_gdp, "GDP_bn")


avg_cols <- function(x) setdiff(names(x), c("Year", "GDP"))

# Taking the average over the pre-specified window (see very top for date adjustments)
us_gdp_avg <- avg_window(US_gdp_norm, Year, avg_cols(US_gdp_norm))
uk_gdp_avg <- avg_window(UK_gdp_norm, Year, avg_cols(UK_gdp_norm))
jp_gdp_avg <- avg_window(JP_gdp_norm, Year, avg_cols(JP_gdp_norm))
de_gdp_avg <- avg_window(DE_gdp_norm, Year, avg_cols(DE_gdp_norm))
fr_gdp_avg <- avg_window(FR_gdp_norm, Year, avg_cols(FR_gdp_norm))
it_gdp_avg <- avg_window(IT_gdp_norm, Year, avg_cols(IT_gdp_norm))
ca_gdp_avg <- avg_window(CA_gdp_norm, Year, avg_cols(CA_gdp_norm))

# ============================================================
# Assemble panel of average composition (% of GDP, 2006–2024)
# ============================================================

bars_gdp <- bind_rows(
  us_gdp_avg$avg %>%
    transmute(
      Country = "United States",
      `Public external debt / GDP`,
      `Central bank — Currency / GDP`,
      `Central bank — Reserves / GDP`,
      `Central bank — Residual / GDP`,
      `Private locally-held — Short / GDP`,
      `Private locally-held — Long / GDP`
    ),
  jp_gdp_avg$avg %>%
    transmute(
      Country = "Japan",
      `Public external debt / GDP`,
      `Central bank — Currency / GDP`,
      `Central bank — Reserves / GDP`,
      `Central bank — Residual / GDP`,
      `Private locally-held — Short / GDP`,
      `Private locally-held — Long / GDP`
    ),
  uk_gdp_avg$avg %>%
    transmute(
      Country = "United Kingdom",
      `Public external debt / GDP`,
      `Central bank — Currency / GDP`,
      `Central bank — Reserves / GDP`,
      `Central bank — Residual / GDP`,
      `Private locally-held — Short / GDP`,
      `Private locally-held — Long / GDP`
    ),
  de_gdp_avg$avg %>%
    transmute(
      Country = "Germany",
      `Public external debt / GDP`,
      `Central bank / GDP`,
      `Private locally-held — Short / GDP`,
      `Private locally-held — Long / GDP`
    ),
  fr_gdp_avg$avg %>%
    transmute(
      Country = "France",
      `Public external debt / GDP`,
      `Central bank / GDP`,
      `Private locally-held — Short / GDP`,
      `Private locally-held — Long / GDP`
    ),
  it_gdp_avg$avg %>%
    transmute(
      Country = "Italy",
      `Public external debt / GDP`,
      `Central bank / GDP`,
      `Private locally-held — Short / GDP`,
      `Private locally-held — Long / GDP`
    ),
  ca_gdp_avg$avg %>%
    transmute(
      Country = "Canada",
      `Public external debt / GDP`,
      `Central bank — Currency / GDP`,
      `Central bank — Reserves / GDP`,
      `Central bank — Residual / GDP`,
      `Private locally-held — Short / GDP`,
      `Private locally-held — Long / GDP`
    )
) %>%
  pivot_longer(
    -Country,
    names_to  = "component",
    values_to = "ratio"
  ) %>%
  mutate(
    ratio   = coalesce(ratio, 0),
    Country = factor(Country, levels = CTRY_ORDER)
  )

# ============================================================
# Plot: Average composition as % of GDP (2006–2024)
# ============================================================

p_gdp <- ggplot(bars_gdp, aes(x = ratio, y = Country, fill = component)) +
  geom_col(width = 0.6) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = COLS_GDP,
    breaks = COMP_ORDER,
    labels = NEW_LABELS[COMP_ORDER],
    drop   = FALSE
  ) +
  guides(
    fill = guide_legend(
      title   = NULL,
      nrow    = 1,
      byrow   = TRUE,
      reverse = TRUE
    )
  ) +
  labs(
    title    = "General-government debt by holder, average 2006–2024 (% of GDP)",
    subtitle = "Central bank is a single block for DE/FR/IT; split into Currency/Reserves/CB-held bonds where data permit.",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor  = element_blank(),
    legend.position   = "bottom",
    axis.text.x       = element_text(size = 12, colour = "black"),
    axis.text.y       = element_text(size = 12, colour = "black"),
    legend.text       = element_text(size = 10),
    legend.key.size   = unit(10, "pt"),
    legend.box.spacing = unit(6, "pt")
  )

print(p_gdp)

# Saving the plot
ggsave(
  filename = file.path(FIG_DIR, "Figure3_public_liabilities.png"),
  plot     = p_gdp,
  width    = 2800,
  height   = 1600,
  units    = "px",
  dpi = 200
)
