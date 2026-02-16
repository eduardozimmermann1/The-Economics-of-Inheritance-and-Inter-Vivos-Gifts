# == 01_build_panel.R ==
# Build wide panel + derived measures + QA summary/by-country + codebook + ISO3 glossary.
# Robust to lower/upper tax codes; enforces 1 row per country-year.

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(haven)
})

log_path <- file.path("outputs", "logs", "01_build_panel.log")
dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
if (file.exists(log_path)) file.remove(log_path)

log_line <- function(...) {
  txt <- paste0(...)
  cat(txt, "\n")
  cat(txt, "\n", file = log_path, append = TRUE)
}

log_line("== 01_build_panel.R ==")
log_line("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
log_line("Working dir: ", normalizePath(getwd(), winslash = "/", mustWork = FALSE))
log_line("")

RAW <- "data/raw/oecd_taxmix.csv"
OUT_CSV <- "data/processed/panel_taxmix.csv"
OUT_DTA <- "data/processed/panel_taxmix.dta"

QA_SUMMARY <- "outputs/tables/qa_summary.csv"
QA_BY_COUNTRY <- "outputs/tables/qa_by_country.csv"
CODEBOOK <- "outputs/tables/codebook.csv"
ISO3_GLOSSARY <- "outputs/tables/country_iso3_glossary.csv"

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(RAW))
log_line("Reading: ", RAW)

raw <- readr::read_csv(RAW, show_col_types = FALSE, progress = FALSE) %>%
  janitor::clean_names()

needed <- c("ref_area", "reference_area", "time_period", "standard_revenue", "obs_value")
missing_cols <- setdiff(needed, names(raw))
if (length(missing_cols) > 0) {
  stop("Missing columns in raw file: ", paste(missing_cols, collapse = ", "))
}

df <- raw %>%
  transmute(
    country_code  = toupper(trimws(as.character(ref_area))),
    country_name  = as.character(reference_area),
    year          = suppressWarnings(as.integer(time_period)),
    tax_code      = toupper(as.character(standard_revenue)),
    value_pct_gdp = suppressWarnings(as.numeric(obs_value))
  ) %>%
  filter(!is.na(country_code), !is.na(year)) %>%
  filter(year >= 1990, year <= 2022) %>%
  # Guard against pseudo-codes; keep OECD_REP explicitly
  filter(country_code == "OECD_REP" | stringr::str_detect(country_code, "^[A-Z]{3}$"))

tax_codes <- sort(unique(df$tax_code))
core <- c("_T","T_4000","T_4200","T_4300","T_4310","T_4320")
log_line("Distinct tax codes in raw: ", length(tax_codes), " (core present: ", sum(core %in% tax_codes), ")")

# Stable country_name per country_code (most frequent non-empty)
name_map <- df %>%
  filter(!is.na(country_name), country_name != "") %>%
  count(country_code, country_name, sort = TRUE) %>%
  group_by(country_code) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(country_code, country_name)

# 1 value per country-year-tax_code
df2 <- df %>%
  group_by(country_code, year, tax_code) %>%
  summarise(value_pct_gdp = mean(value_pct_gdp, na.rm = TRUE), .groups = "drop")

# Wide panel: 1 row per country-year
w <- df2 %>%
  tidyr::pivot_wider(
    names_from = tax_code,
    values_from = value_pct_gdp
  ) %>%
  left_join(name_map, by = "country_code") %>%
  relocate(country_code, country_name, year) %>%
  arrange(country_code, year)

# Ensure required columns exist
req_cols <- c("_T","T_4000","T_4100","T_4200","T_4300","T_4310","T_4320","T_4500","T_4600")
for (cc in req_cols) if (!cc %in% names(w)) w[[cc]] <- NA_real_

safe_div <- function(num, den) ifelse(!is.na(den) & den > 0, num / den, NA_real_)

w <- w %>%
  mutate(
    total_tax_pct_gdp          = `_T`,
    property_tax_pct_gdp       = T_4000,
    inheritance_gifts_pct_gdp  = T_4300,
    inheritance_pct_gdp        = T_4310,
    gifts_pct_gdp              = T_4320,

    # Shares of total tax (%)
    share_4300_total_tax_pct = 100 * safe_div(T_4300, `_T`),
    share_4000_total_tax_pct = 100 * safe_div(T_4000, `_T`),

    # Shares within property taxes (%)
    share_4300_in_property_pct = 100 * safe_div(T_4300, T_4000),
    share_4200_in_property_pct = 100 * safe_div(T_4200, T_4000),

    # Within-4300 (fractions)
    gift_share_within_4300    = safe_div(T_4320, T_4300),
    inherit_share_within_4300 = safe_div(T_4310, T_4300)
  )

qa <- w %>%
  summarise(
    n_rows = n(),
    years_min = min(year, na.rm = TRUE),
    years_max = max(year, na.rm = TRUE),
    n_countries = n_distinct(country_code),
    missing_T_4300 = sum(is.na(T_4300)),
    missing_T_4310 = sum(is.na(T_4310)),
    missing_T_4320 = sum(is.na(T_4320)),
    missing_T_4000 = sum(is.na(T_4000)),
    missing_total  = sum(is.na(`_T`))
  )

qa_by_country <- w %>%
  group_by(country_code, country_name) %>%
  summarise(
    years_obs = n_distinct(year),
    years_min = min(year, na.rm = TRUE),
    years_max = max(year, na.rm = TRUE),
    miss_total = sum(is.na(`_T`)),
    miss_4000  = sum(is.na(T_4000)),
    miss_4200  = sum(is.na(T_4200)),
    miss_4300  = sum(is.na(T_4300)),
    miss_4310  = sum(is.na(T_4310)),
    miss_4320  = sum(is.na(T_4320)),
    .groups = "drop"
  ) %>%
  arrange(country_code)

readr::write_csv(qa, QA_SUMMARY)
readr::write_csv(qa_by_country, QA_BY_COUNTRY)

# ISO3 glossary (for the whole paper + figures)
iso3_glossary <- w %>%
  distinct(country_code, country_name) %>%
  arrange(country_code)
readr::write_csv(iso3_glossary, ISO3_GLOSSARY)

codebook <- tibble::tribble(
  ~variable, ~definition,
  "country_code", "OECD reference area code (ISO3); includes OECD_REP aggregate",
  "country_name", "OECD reference area name (modal label per code)",
  "year", "Calendar year",
  "`_T`", "Total tax revenue (% of GDP)",
  "T_4000", "Taxes on property (% of GDP)",
  "T_4200", "Recurrent taxes on net wealth (% of GDP)",
  "T_4300", "Estate, inheritance and gift taxes (% of GDP)",
  "T_4310", "Estate and inheritance taxes (% of GDP)",
  "T_4320", "Gift taxes (% of GDP)",
  "share_4300_total_tax_pct", "T_4300 as share of total tax revenue (%)",
  "share_4300_in_property_pct", "T_4300 as share of property taxes T_4000 (%)",
  "share_4200_in_property_pct", "T_4200 as share of property taxes T_4000 (%)",
  "gift_share_within_4300", "T_4320 / T_4300 (share within 4300, if available)",
  "inherit_share_within_4300", "T_4310 / T_4300 (share within 4300, if available)"
)
readr::write_csv(codebook, CODEBOOK)

readr::write_csv(w, OUT_CSV, na = "")
haven::write_dta(w, OUT_DTA, version = 12)

log_line("Wrote: ", OUT_CSV)
log_line("Wrote: ", OUT_DTA)
log_line("Wrote: ", QA_SUMMARY)
log_line("Wrote: ", QA_BY_COUNTRY)
log_line("Wrote: ", CODEBOOK)
log_line("Wrote: ", ISO3_GLOSSARY)
log_line("")
log_line("Done.")
