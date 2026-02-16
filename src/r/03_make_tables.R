# == 03_make_tables.R ==
# CSV tables only. Adds ISO3 glossary reference and a CHE-focused property-mix table.

suppressPackageStartupMessages({
  library(tidyverse)
})

log_path <- file.path("outputs", "logs", "03_make_tables.log")
dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
if (file.exists(log_path)) file.remove(log_path)

log_line <- function(...) {
  txt <- paste0(...)
  cat(txt, "\n")
  cat(txt, "\n", file = log_path, append = TRUE)
}

log_line("== 03_make_tables.R ==")
log_line("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
log_line("")

INP <- "data/processed/panel_taxmix.csv"
stopifnot(file.exists(INP))
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

w <- readr::read_csv(INP, show_col_types = FALSE, progress = FALSE)

comparators <- c("AUT","DEU","FRA","ITA","GBR","SWE","USA","NLD")
keep_key <- c("CHE","OECD_REP", comparators)
main_n <- 15

full_start <- 1990
full_end   <- 2022
recent_start <- 2018
recent_end   <- 2022

safe_div <- function(num, den) ifelse(!is.na(den) & den > 0, num / den, NA_real_)

# ISO3 glossary (again, for convenience; built in 01 too)
gloss <- w %>% distinct(country_code, country_name) %>% arrange(country_code)
readr::write_csv(gloss, "outputs/tables/country_iso3_glossary.csv")
log_line("Wrote: outputs/tables/country_iso3_glossary.csv")

# ---------------------------
# Table 0: Coverage 2018–2022
# ---------------------------
tab0 <- w %>%
  filter(year >= recent_start, year <= recent_end) %>%
  group_by(country_code, country_name) %>%
  summarise(
    years_obs = n_distinct(year),
    any_total = any(!is.na(total_tax_pct_gdp)),
    any_4000  = any(!is.na(property_tax_pct_gdp)),
    any_4300  = any(!is.na(inheritance_gifts_pct_gdp)),
    any_4310  = any(!is.na(inheritance_pct_gdp)),
    any_4320  = any(!is.na(gifts_pct_gdp)),
    .groups = "drop"
  ) %>%
  arrange(desc(years_obs), country_code)

readr::write_csv(tab0, "outputs/tables/table0_coverage_2018_2022.csv")
log_line("Wrote: outputs/tables/table0_coverage_2018_2022.csv")

# ---------------------------
# Table 1: 2018–2022 averages (mean levels -> shares), incl. OECD_REP
# ---------------------------
tab1 <- w %>%
  filter(year >= recent_start, year <= recent_end) %>%
  group_by(country_code, country_name) %>%
  summarise(
    mT    = mean(total_tax_pct_gdp, na.rm = TRUE),
    m4000 = mean(property_tax_pct_gdp, na.rm = TRUE),
    m4300 = mean(inheritance_gifts_pct_gdp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    T4300_pct_gdp = m4300,
    T4300_share_total_tax_pct = 100 * safe_div(m4300, mT),
    T4300_share_property_tax_pct = 100 * safe_div(m4300, m4000),
    property_tax_share_total_tax_pct = 100 * safe_div(m4000, mT)
  ) %>%
  arrange(desc(T4300_share_total_tax_pct))

readr::write_csv(tab1, "outputs/tables/table1_2018_2022_levels_shares_full.csv")
log_line("Wrote: outputs/tables/table1_2018_2022_levels_shares_full.csv")

top1 <- tab1 %>% filter(is.finite(T4300_share_total_tax_pct)) %>% slice_head(n = main_n) %>% pull(country_code)
tab1_main <- tab1 %>% filter(country_code %in% unique(c(keep_key, top1))) %>%
  arrange(desc(T4300_share_total_tax_pct), country_code)

readr::write_csv(tab1_main, "outputs/tables/table1_2018_2022_levels_shares_main.csv")
log_line("Wrote: outputs/tables/table1_2018_2022_levels_shares_main.csv")

# ---------------------------
# Table 2: Gifts vs inheritance within 4300 (2018–2022), excl. OECD_REP
# ---------------------------
tab2 <- w %>%
  filter(year >= recent_start, year <= recent_end) %>%
  group_by(country_code, country_name) %>%
  summarise(
    m4300 = mean(inheritance_gifts_pct_gdp, na.rm = TRUE),
    m4310 = mean(inheritance_pct_gdp, na.rm = TRUE),
    m4320 = mean(gifts_pct_gdp, na.rm = TRUE),
    has_4320 = any(!is.na(gifts_pct_gdp)),
    .groups = "drop"
  ) %>%
  filter(country_code != "OECD_REP") %>%
  filter(is.finite(m4300), m4300 > 0) %>%
  mutate(
    inheritance_share = if_else(is.finite(m4310), m4310 / m4300, NA_real_),
    gifts_share = if_else(is.finite(m4320), m4320 / m4300, NA_real_),
    gifts_share = if_else(is.na(gifts_share) & !is.na(inheritance_share),
                          pmax(0, 1 - inheritance_share), gifts_share),
    s = inheritance_share + gifts_share,
    inheritance_share = if_else(is.finite(s) & s > 0.95 & s < 1.05, inheritance_share / s, inheritance_share),
    gifts_share       = if_else(is.finite(s) & s > 0.95 & s < 1.05, gifts_share / s, gifts_share),
    note = case_when(
      country_code == "CHE" ~ "CHE: gifts not separately reported (residual used)",
      !has_4320 ~ "No separate gifts series",
      TRUE ~ ""
    )
  ) %>%
  filter(is.finite(inheritance_share), is.finite(gifts_share)) %>%
  arrange(desc(gifts_share))

readr::write_csv(tab2, "outputs/tables/table2_gifts_vs_inheritance_2018_2022_full.csv")
log_line("Wrote: outputs/tables/table2_gifts_vs_inheritance_2018_2022_full.csv")

top2 <- tab2 %>% slice_head(n = main_n) %>% pull(country_code)
tab2_main <- tab2 %>% filter(country_code %in% unique(c("CHE", comparators, top2))) %>%
  arrange(desc(gifts_share), country_code)

readr::write_csv(tab2_main, "outputs/tables/table2_gifts_vs_inheritance_2018_2022_main.csv")
log_line("Wrote: outputs/tables/table2_gifts_vs_inheritance_2018_2022_main.csv")

# ---------------------------
# Extra: CHE focus (property mix: 4200 vs 4300 within 4000)
# This supports your point (20): CHE net-wealth share is unusually high.
# ---------------------------
mix_focus <- w %>%
  filter(year >= recent_start, year <= recent_end) %>%
  filter(country_code %in% keep_key) %>%
  group_by(country_code, country_name) %>%
  summarise(
    m4000 = mean(property_tax_pct_gdp, na.rm = TRUE),
    m4200 = mean(T_4200, na.rm = TRUE),
    m4300 = mean(inheritance_gifts_pct_gdp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sh_4200_in_4000 = 100 * safe_div(m4200, m4000),
    sh_4300_in_4000 = 100 * safe_div(m4300, m4000),
    sh_other_in_4000 = 100 - sh_4200_in_4000 - sh_4300_in_4000
  ) %>%
  arrange(desc(sh_4200_in_4000))

readr::write_csv(mix_focus, "outputs/tables/table_focus_property_mix_CHE_2018_2022.csv")
log_line("Wrote: outputs/tables/table_focus_property_mix_CHE_2018_2022.csv")

log_line("")
log_line("Done. CSV tables saved to outputs/tables.")
