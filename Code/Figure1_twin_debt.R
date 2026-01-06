# ============================================================
# Gross Public Debt (% GDP) and External Debt Liabilities (% GDP)
# Countries: US, UK, DE, FR, IT, JP, CA, CN, IN
#
# Plot 1: Annual data, arrows as specified, with G9 GDP-weighted aggregate
# Plot 2: 5-year non-overlapping averages, arrows only, no circles
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(grid) 
})

## ---- Paths ----
ROOT <- "YOUR ROOT HERE"
FP_EWN      <- file.path(ROOT, "Data", "EWN-database-January-2025.xlsx")
FP_IMF_CTRY <- file.path(ROOT, "Data", "IMF_public_debt_GDP.xlsx")

## ---- Inputs ----
ewn <- read_excel(FP_EWN, sheet = "Dataset")
imf_public_debt_wide <- read_excel(FP_IMF_CTRY) %>% rename(country = 1)

## ---- Country name harmonization ----
label_map <- tibble::tibble(
  ewn_country = c("United States","China,P.R.: Mainland","India",
                  "United Kingdom","France","Japan","Germany","Italy","Canada"),
  label = c("United States","China","India","United Kingdom","France",
            "Japan","Germany","Italy","Canada")
)

imf_map <- tibble::tibble(
  country_imf = c("United States","China, People's Republic of","India",
                  "United Kingdom","France","Japan","Germany","Italy","Canada"),
  label = c("United States","China","India","United Kingdom","France",
            "Japan","Germany","Italy","Canada")
)

COUNTRIES_IMF <- imf_map$country_imf

## ---- EWN: external debt liabilities (% GDP) WITH GDP ----
ewn_debtliab <- ewn %>%
  semi_join(label_map, by = c("Country" = "ewn_country")) %>%
  transmute(
    Country_raw     = Country,
    Year            = as.integer(Year),
    GDP             = `GDP (US$)`,
    debt_liab_usd   = `Debt liabilities (portfolio debt + other investment)`,
    DebtLiab_pctGDP = 100 * debt_liab_usd / GDP
  ) %>%
  left_join(label_map, by = c("Country_raw" = "ewn_country")) %>%
  transmute(
    Country = label,
    Year,
    GDP,
    DebtLiab_pctGDP
  ) %>%
  filter(!is.na(Year), !is.na(DebtLiab_pctGDP), !is.na(GDP))

## ---- IMF: gross public debt (% GDP), wide -> long ----
imf_debt <- imf_public_debt_wide %>%
  filter(country %in% COUNTRIES_IMF) %>%
  left_join(imf_map, by = c("country" = "country_imf")) %>%
  mutate(across(-c(country, label), as.character)) %>%
  pivot_longer(-c(country, label), names_to = "Year", values_to = "Debt_GDP_raw") %>%
  mutate(
    Year     = as.integer(Year),
    Debt_GDP = as.numeric(Debt_GDP_raw)
  ) %>%
  transmute(Country = label, Year, Debt_GDP) %>%
  filter(!is.na(Year), !is.na(Debt_GDP))

## ---- Merge: base annual dataframe ----
plot_df <- ewn_debtliab %>%
  inner_join(imf_debt, by = c("Country","Year")) %>%
  arrange(Country, Year)

# ============================================================
# GDP-weighted aggregate (annual) – as a "Country" for legend
# ============================================================
gdp_weighted <- plot_df %>%
  group_by(Year) %>%
  summarise(
    GDP_total       = sum(GDP, na.rm = TRUE),
    Debt_GDP        = sum(Debt_GDP * GDP, na.rm = TRUE) / GDP_total,
    DebtLiab_pctGDP = sum(DebtLiab_pctGDP * GDP, na.rm = TRUE) / GDP_total,
    .groups         = "drop"
  ) %>%
  mutate(Country = "G9 GDP-weighted") %>%
  arrange(Year)

# ============================================================
# Colors: nicer qualitative palette for countries, G9 in black
# ============================================================
country_levels <- sort(unique(plot_df$Country))

# Use a qualitative HCL palette ("Set 3") for 9 countries
country_cols <- grDevices::hcl.colors(length(country_levels), "Set 2")
names(country_cols) <- country_levels

color_values <- c(country_cols, "G9 GDP-weighted" = "black")
legend_breaks <- c(country_levels, "G9 GDP-weighted")

# ============================================================
# 1) ARROWS for ANNUAL data – endpoints at points
# ============================================================

# Country-level arrows: between Year_t and Year_{t+1}, keep idx %% 4 == 1
country_arrows <- plot_df %>%
  group_by(Country) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(
    idx    = row_number(),
    x_start = Debt_GDP,
    y_start = DebtLiab_pctGDP,
    x_end   = dplyr::lead(Debt_GDP),
    y_end   = dplyr::lead(DebtLiab_pctGDP)
  ) %>%
  ungroup() %>%
  filter(!is.na(x_end), !is.na(y_end)) %>%
  filter(idx %% 4 == 1)

# GDP-weighted arrows: idx %% 3 == 1
gdp_arrows <- gdp_weighted %>%
  arrange(Year) %>%
  mutate(
    idx    = row_number(),
    x_start = Debt_GDP,
    y_start = DebtLiab_pctGDP,
    x_end   = dplyr::lead(Debt_GDP),
    y_end   = dplyr::lead(DebtLiab_pctGDP)
  ) %>%
  filter(!is.na(x_end), !is.na(y_end)) %>%
  filter(idx %% 3 == 1)

# ============================================================
# PLOT 1: Annual data
# ============================================================

p_annual <- ggplot() +
  # (a) Country full lines
  geom_path(
    data = plot_df,
    aes(
      x     = Debt_GDP,
      y     = DebtLiab_pctGDP,
      color = Country,
      group = Country
    ),
    linewidth = 0.7
  ) +
  # (b) Country arrows
  geom_segment(
    data = country_arrows,
    aes(
      x     = x_start,
      y     = y_start,
      xend  = x_end,
      yend  = y_end,
      color = Country
    ),
    linewidth = 0.7,
    arrow     = arrow(type = "closed", length = unit(0.08, "inches"))
  ) +
  # (c) Country points
  geom_point(
    data = plot_df,
    aes(
      x     = Debt_GDP,
      y     = DebtLiab_pctGDP,
      color = Country
    ),
    size  = 1.8,
    alpha = 0.9
  ) +
  # (d) G9 GDP-weighted full path
  geom_path(
    data = gdp_weighted,
    aes(
      x     = Debt_GDP,
      y     = DebtLiab_pctGDP,
      color = Country,
      group = Country
    ),
    linewidth = 2,
    alpha     = 0.8
  ) +
  # (e) G9 GDP-weighted arrows
  geom_segment(
    data = gdp_arrows,
    aes(
      x     = x_start,
      y     = y_start,
      xend  = x_end,
      yend  = y_end,
      color = Country
    ),
    linewidth = 1.5,
    arrow     = arrow(type = "closed", length = unit(0.12, "inches"))
  ) +
  # (f) G9 GDP-weighted points
  geom_point(
    data = gdp_weighted,
    aes(
      x     = Debt_GDP,
      y     = DebtLiab_pctGDP,
      color = Country
    ),
    size = 2
  ) +
  scale_color_manual(values = color_values, breaks = legend_breaks) +
  theme_classic(base_size = 12) +
  theme(
    axis.text  = element_text(size = 13),
    axis.title = element_text(size = 15)
  ) +
  labs(
    title = "Gross public and external debt liabilities (annual)",
    x     = "Gross public debt (% of GDP)",
    y     = "Gross external debt liabilities (% of GDP)",
    color = NULL
  )

print(p_annual)

# ============================================================
# 2) 5-year NON-OVERLAPPING AVERAGES, arrows only, NO circles
# ============================================================

# Define 5-year blocks: e.g. if first year is 1970, blocks are 1970–74, 75–79, ...
min_year <- min(plot_df$Year, na.rm = TRUE)

plot_df_5y <- plot_df %>%
  mutate(
    PeriodStart = min_year + 5 * ((Year - min_year) %/% 5)
  ) %>%
  group_by(Country, PeriodStart) %>%
  summarise(
    Debt_GDP        = mean(Debt_GDP, na.rm = TRUE),
    DebtLiab_pctGDP = mean(DebtLiab_pctGDP, na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  arrange(Country, PeriodStart)

gdp_weighted_5y <- gdp_weighted %>%
  mutate(
    PeriodStart = min_year + 5 * ((Year - min_year) %/% 5)
  ) %>%
  group_by(Country, PeriodStart) %>%  # Country = "G9 GDP-weighted"
  summarise(
    Debt_GDP        = mean(Debt_GDP, na.rm = TRUE),
    DebtLiab_pctGDP = mean(DebtLiab_pctGDP, na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  arrange(PeriodStart)

# Country arrows for 5-year averages
country_arrows_5y <- plot_df_5y %>%
  group_by(Country) %>%
  arrange(PeriodStart, .by_group = TRUE) %>%
  mutate(
    x_start = Debt_GDP,
    y_start = DebtLiab_pctGDP,
    x_end   = dplyr::lead(Debt_GDP),
    y_end   = dplyr::lead(DebtLiab_pctGDP)
  ) %>%
  ungroup() %>%
  filter(!is.na(x_end), !is.na(y_end))

# G9 arrows for 5-year averages
gdp_arrows_5y <- gdp_weighted_5y %>%
  arrange(PeriodStart) %>%
  mutate(
    x_start = Debt_GDP,
    y_start = DebtLiab_pctGDP,
    x_end   = dplyr::lead(Debt_GDP),
    y_end   = dplyr::lead(DebtLiab_pctGDP)
  ) %>%
  filter(!is.na(x_end), !is.na(y_end))

# ============================================================
# PLOT 2: 5-year averages, arrows only
# ============================================================

p_5y <- ggplot() +
  # (a) Country 5-year lines
  geom_path(
    data = plot_df_5y,
    aes(
      x     = Debt_GDP,
      y     = DebtLiab_pctGDP,
      color = Country,
      group = Country
    ),
    linewidth = 1
  ) +
  # (b) Country 5-year arrows
  geom_segment(
    data = country_arrows_5y,
    aes(
      x     = x_start,
      y     = y_start,
      xend  = x_end,
      yend  = y_end,
      color = Country
    ),
    linewidth = 1,
    arrow     = arrow(type = "closed", length = unit(0.08, "inches"))
  ) +
  # (c) G9 5-year line
  geom_path(
    data = gdp_weighted_5y,
    aes(
      x     = Debt_GDP,
      y     = DebtLiab_pctGDP,
      color = Country,
      group = Country
    ),
    linewidth = 2,
    alpha     = 0.8
  ) +
  # (d) G9 5-year arrows
  geom_segment(
    data = gdp_arrows_5y,
    aes(
      x     = x_start,
      y     = y_start,
      xend  = x_end,
      yend  = y_end,
      color = Country
    ),
    linewidth = 1.5,
    arrow     = arrow(type = "closed", length = unit(0.12, "inches"))
  ) +
  scale_color_manual(values = color_values, breaks = legend_breaks) +
  theme_classic(base_size = 12) +
  theme(
    axis.text      = element_text(size = 20),
    axis.title     = element_text(size = 21),
    legend.text    = element_text(size = 18),
    legend.position = "bottom"          # <- legend under the plot
  ) +
  labs(
    x     = "Gross public debt (% of GDP, 5-year average)",
    y     = "Gross external debt liabilities (% of GDP, 5-year average)",
    color = NULL
  ) +guides(color = guide_legend(nrow = 1, byrow = TRUE))


print(p_5y)


# ---- Save the figure ----
fig_dir <- file.path(ROOT, "Figures")  

if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

# ---- Save the plot ----
ggsave(
  filename = file.path(fig_dir, "Figure1_twin_debt.png"),
  plot     = p_5y,
  width    = 2800,
  height   = 1400,
  units    = "px",
  dpi      = 150
)


