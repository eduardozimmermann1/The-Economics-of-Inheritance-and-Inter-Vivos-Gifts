# == 02_make_figures.R ==
# PNG outputs via ragg::agg_png.
# Adds Fig 5: Europe-centered choropleth (heatmap map) + Tableau fallback.
# Adds full-period (1990–2022) appendix variants for fig3/fig4 summaries.
# Restores Fig A1: cross-country scatter (recent period) for CHE positioning.
# Fixes Fig A4/A4b: include full OECD coverage by showing "no breakdown" (grey) when 4310/4320 split is unavailable,
#                   plus symmetric residual handling + rescaling to sum to 100%.

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(patchwork)
  library(ggrepel)
  library(ragg)
})

log_path <- file.path("outputs", "logs", "02_make_figures.log")
dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
if (file.exists(log_path)) file.remove(log_path)

log_line <- function(...) {
  txt <- paste0(...)
  cat(txt, "\n")
  cat(txt, "\n", file = log_path, append = TRUE)
}

log_line("== 02_make_figures.R ==")
log_line("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
log_line("")

INP <- "data/processed/panel_taxmix.csv"
stopifnot(file.exists(INP))
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)

save_png <- function(p, file, width = 12, height = 7.5, res = 320) {
  ragg::agg_png(file, width = width, height = height, units = "in", res = res)
  print(p)
  dev.off()
}

BLUE   <- "#0072B2"
ORANGE <- "#D55E00"

theme_kof <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey90"),
      plot.title = element_text(face = "bold", size = 18, margin = margin(b = 6)),
      plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
      # NOTE: caption left-aligned + line breaks (fix clipping/cut-off on long captions)
      plot.caption = element_text(size = 10, color = "grey35", margin = margin(t = 10),
                                  hjust = 0, lineheight = 1.05),
      axis.title = element_text(size = 12),
      axis.text  = element_text(size = 11),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      plot.margin = margin(10, 18, 14, 10)
    )
}

select_main_codes <- function(stats_df, metric, top_n = 15, keep = character()) {
  metric <- rlang::ensym(metric)
  top_codes <- stats_df %>%
    filter(is.finite(!!metric)) %>%
    arrange(desc(!!metric)) %>%
    slice_head(n = top_n) %>%
    pull(country_code)
  unique(c(keep, top_codes))
}

order_levels_desc <- function(stats_df, metric, codes) {
  metric <- rlang::ensym(metric)
  stats_df %>%
    filter(country_code %in% codes) %>%
    arrange(desc(!!metric), country_code) %>%
    pull(country_code) %>%
    unique()
}

dyn_height <- function(n, base = 6, per = 0.30, min_h = 10, max_h = 20) {
  h <- base + per * n
  h <- max(min_h, h)
  h <- min(max_h, h)
  h
}

w <- readr::read_csv(INP, show_col_types = FALSE, progress = FALSE)

need_vars <- c("country_code","year",
               "inheritance_gifts_pct_gdp","share_4300_total_tax_pct",
               "property_tax_pct_gdp","T_4200","inheritance_pct_gdp","gifts_pct_gdp")
missing_vars <- setdiff(need_vars, names(w))
if (length(missing_vars) > 0) {
  stop("Missing required columns in panel_taxmix.csv: ", paste(missing_vars, collapse = ", "),
       ". Run 01_build_panel.R again.")
}

comparators <- c("AUT","DEU","FRA","ITA","GBR","SWE","USA","NLD")
keep_key <- c("CHE","OECD_REP", comparators)
end_year <- max(w$year, na.rm = TRUE)
main_n <- 15

full_start <- 1990
full_end   <- 2022
recent_start <- 2018
recent_end   <- 2022

# ---------------------------
# FIG 1: Switzerland focus — 4300 level (% GDP) + share of total tax revenue
# ---------------------------
d1 <- w %>%
  filter(country_code %in% keep_key) %>%
  select(country_code, year, inheritance_gifts_pct_gdp, share_4300_total_tax_pct)

p1a <- ggplot(d1, aes(x = year, y = inheritance_gifts_pct_gdp, group = country_code)) +
  geom_line(
    data = ~ filter(.x, !(country_code %in% c("CHE","OECD_REP"))),
    color = "grey70", linewidth = 0.7, alpha = 0.8
  ) +
  geom_line(data = ~ filter(.x, country_code == "OECD_REP"),
            color = ORANGE, linewidth = 1.1, linetype = "dashed") +
  geom_line(data = ~ filter(.x, country_code == "CHE"),
            color = BLUE, linewidth = 1.3) +
  geom_text(data = d1 %>% filter(year == end_year, country_code == "CHE"),
            aes(label = "CHE"), hjust = 0, nudge_x = 0.6, size = 3.8, color = BLUE) +
  geom_text(data = d1 %>% filter(year == end_year, country_code == "OECD_REP"),
            aes(label = "OECD_REP"), hjust = 0, nudge_x = 0.6, size = 3.8, color = ORANGE) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.12))) +
  labs(
    title = "Inheritance & inter-vivos gift tax revenues (OECD 4300), % of GDP",
    subtitle = "Switzerland vs OECD average; selected comparators shown as context (grey)",
    x = NULL, y = "% of GDP"
  ) +
  theme_kof()

p1b <- ggplot(d1, aes(x = year, y = share_4300_total_tax_pct, group = country_code)) +
  geom_line(
    data = ~ filter(.x, !(country_code %in% c("CHE","OECD_REP"))),
    color = "grey70", linewidth = 0.7, alpha = 0.8
  ) +
  geom_line(data = ~ filter(.x, country_code == "OECD_REP"),
            color = ORANGE, linewidth = 1.1, linetype = "dashed") +
  geom_line(data = ~ filter(.x, country_code == "CHE"),
            color = BLUE, linewidth = 1.3) +
  geom_text(data = d1 %>% filter(year == end_year, country_code == "CHE"),
            aes(label = "CHE"), hjust = 0, nudge_x = 0.6, size = 3.8, color = BLUE) +
  geom_text(data = d1 %>% filter(year == end_year, country_code == "OECD_REP"),
            aes(label = "OECD_REP"), hjust = 0, nudge_x = 0.6, size = 3.8, color = ORANGE) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.12))) +
  labs(
    title = "Inheritance & gift taxes as share of total tax revenue",
    subtitle = "Same comparison set; comparator labels omitted for readability",
    x = "Year", y = "% of total tax revenue"
  ) +
  theme_kof()

fig1 <- p1a / p1b + plot_annotation(
  caption = paste0(
    "Comparators: ", paste(comparators, collapse = ", "),
    ". Source: OECD Global Revenue Statistics (Revenue Statistics classification)."
  )
)

save_png(fig1, "outputs/figures/fig1_timeseries_CHE_focus.png", width = 12, height = 10, res = 320)
log_line("Wrote: outputs/figures/fig1_timeseries_CHE_focus.png")

# ---------------------------
# FIG A1: Cross-country scatter — CHE positioning (recent averages)
# ---------------------------
dA1 <- w %>%
  filter(year >= recent_start, year <= recent_end) %>%
  group_by(country_code) %>%
  summarise(
    avg_level = mean(inheritance_gifts_pct_gdp, na.rm = TRUE),      # (% GDP)
    avg_share = mean(share_4300_total_tax_pct, na.rm = TRUE),      # (% of total tax revenue)
    .groups = "drop"
  ) %>%
  filter(is.finite(avg_level), is.finite(avg_share))

pA1 <- ggplot(dA1 %>% filter(!(country_code %in% c("CHE","OECD_REP"))),
              aes(x = avg_level, y = avg_share)) +
  geom_point(color = "grey65", size = 2.6, alpha = 0.85) +
  geom_point(data = dA1 %>% filter(country_code == "OECD_REP"),
             color = ORANGE, size = 3.2) +
  geom_point(data = dA1 %>% filter(country_code == "CHE"),
             color = BLUE, size = 3.2) +
  ggrepel::geom_text_repel(
    data = dA1 %>% filter(country_code %in% c("CHE","OECD_REP")),
    aes(label = country_code),
    size = 4.2,
    box.padding = 0.35,
    point.padding = 0.25,
    min.segment.length = 0
  ) +
  labs(
    title = "Where does Switzerland sit cross-country (recent period)?",
    subtitle = paste0(recent_start, "–", recent_end, " averages: level (% GDP) vs share of total tax revenue (%)"),
    x = "Avg 4300 (% of GDP)",
    y = "Avg 4300 as share of total tax revenue (%)",
    caption = "Descriptive positioning (no causal interpretation). Source: OECD GRS."
  ) +
  theme_kof()

save_png(pA1, "outputs/figures/figA1_scatter_CHE_position.png", width = 12, height = 8, res = 320)
log_line("Wrote: outputs/figures/figA1_scatter_CHE_position.png")

# ---------------------------
# FIG 2: Boxplots — main view + appendix (full OECD), ordered by median (1990–2022)
# ---------------------------
stats2 <- w %>%
  filter(!is.na(share_4300_total_tax_pct)) %>%
  group_by(country_code) %>%
  summarise(med = median(share_4300_total_tax_pct, na.rm = TRUE), .groups = "drop")

codes2_main <- select_main_codes(stats2, med, top_n = main_n, keep = keep_key)
levels2_main <- order_levels_desc(stats2, med, codes2_main)

d2_main <- w %>%
  filter(country_code %in% codes2_main) %>%
  filter(!is.na(share_4300_total_tax_pct)) %>%
  mutate(country_code = factor(country_code, levels = levels2_main))

med_pts_main <- stats2 %>%
  filter(country_code %in% c("CHE","OECD_REP")) %>%
  mutate(color = if_else(country_code == "CHE", BLUE, ORANGE),
         country_code = factor(country_code, levels = levels2_main))

p2_main <- ggplot(d2_main, aes(x = share_4300_total_tax_pct, y = country_code)) +
  geom_boxplot(fill = "white", color = "grey25", linewidth = 0.7, outlier.alpha = 0.30) +
  geom_point(data = med_pts_main, aes(x = med, y = country_code, color = color),
             inherit.aes = FALSE, size = 2.6) +
  geom_label(data = med_pts_main,
             aes(x = med, y = country_code, label = paste0(as.character(country_code), " (median)"), color = color),
             inherit.aes = FALSE, fill = "white", label.size = NA, size = 3.5, nudge_x = 0.12) +
  scale_color_identity() +
  coord_cartesian(xlim = c(0, max(d2_main$share_4300_total_tax_pct, na.rm = TRUE) * 1.05)) +
  labs(
    title = "Distribution of inheritance & gift taxes (OECD 4300) as share of total tax revenue",
    subtitle = paste0(full_start, "–", full_end,
                      "; main view (top ", main_n, " by median + CHE/OECD + comparators)"),
    x = "% of total tax revenue", y = NULL,
    caption = "Full OECD version exported as appendix. Source: OECD Global Revenue Statistics."
  ) +
  theme_kof()

save_png(p2_main, "outputs/figures/fig2_boxplot_4300_share_total_main.png", width = 12, height = 8, res = 320)
log_line("Wrote: outputs/figures/fig2_boxplot_4300_share_total_main.png")

levels2_full <- stats2 %>%
  arrange(desc(med), country_code) %>%
  pull(country_code) %>% unique()

d2_full <- w %>%
  filter(!is.na(share_4300_total_tax_pct)) %>%
  mutate(country_code = factor(country_code, levels = levels2_full))

med_pts_full <- stats2 %>%
  filter(country_code %in% c("CHE","OECD_REP")) %>%
  mutate(color = if_else(country_code == "CHE", BLUE, ORANGE),
         country_code = factor(country_code, levels = levels2_full))

p2_full <- ggplot(d2_full, aes(x = share_4300_total_tax_pct, y = country_code)) +
  geom_boxplot(fill = "white", color = "grey25", linewidth = 0.6, outlier.alpha = 0.25) +
  geom_point(data = med_pts_full, aes(x = med, y = country_code, color = color),
             inherit.aes = FALSE, size = 2.2) +
  geom_label(data = med_pts_full,
             aes(x = med, y = country_code, label = paste0(as.character(country_code), " (median)"), color = color),
             inherit.aes = FALSE, fill = "white", label.size = NA, size = 3.2, nudge_x = 0.10) +
  scale_color_identity() +
  coord_cartesian(xlim = c(0, quantile(d2_full$share_4300_total_tax_pct, 0.995, na.rm = TRUE))) +
  labs(
    title = "Distribution of inheritance & gift taxes (OECD 4300) as share of total tax revenue",
    subtitle = paste0(full_start, "–", full_end, "; full OECD (appendix)"),
    x = "% of total tax revenue", y = NULL
  ) +
  theme_kof()

save_png(p2_full, "outputs/figures/figA2_boxplot_4300_share_total_full_oecd.png", width = 12, height = 12, res = 320)
log_line("Wrote: outputs/figures/figA2_boxplot_4300_share_total_full_oecd.png")

# ---------------------------
# FIG 3: Property-tax mix (4000) — shares from mean LEVELS
# Main view (recent) + appendix recent full + appendix full-period full
# ---------------------------
make_stats3 <- function(df, y0, y1) {
  df %>%
    filter(year >= y0, year <= y1) %>%
    group_by(country_code) %>%
    summarise(
      m4000 = mean(property_tax_pct_gdp, na.rm = TRUE),
      m4200 = mean(T_4200, na.rm = TRUE),
      m4300 = mean(inheritance_gifts_pct_gdp, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      net_wealth_sh    = if_else(is.finite(m4000) & m4000 > 0, m4200 / m4000, NA_real_),
      inherit_gifts_sh = if_else(is.finite(m4000) & m4000 > 0, m4300 / m4000, NA_real_),
      other_sh         = if_else(is.finite(net_wealth_sh) & is.finite(inherit_gifts_sh),
                                 pmax(0, 1 - net_wealth_sh - inherit_gifts_sh),
                                 NA_real_)
    )
}

pal3 <- c(
  "Other property taxes (residual)" = "grey75",
  "4200: Net wealth" = BLUE,
  "4300: Inheritance & gifts" = ORANGE
)

plot_fig3 <- function(stats3, subtitle, out_file, main_view = TRUE) {
  if (main_view) {
    codes3 <- select_main_codes(stats3, inherit_gifts_sh, top_n = main_n, keep = keep_key)
    levels3 <- order_levels_desc(stats3, inherit_gifts_sh, codes3)
    dd <- stats3 %>%
      filter(country_code %in% codes3) %>%
      filter(is.finite(other_sh), is.finite(net_wealth_sh), is.finite(inherit_gifts_sh)) %>%
      mutate(country_code = factor(country_code, levels = levels3))
  } else {
    levels3 <- stats3 %>%
      filter(is.finite(inherit_gifts_sh)) %>%
      arrange(desc(inherit_gifts_sh), country_code) %>%
      pull(country_code) %>% unique()
    dd <- stats3 %>%
      filter(is.finite(other_sh), is.finite(net_wealth_sh), is.finite(inherit_gifts_sh)) %>%
      mutate(country_code = factor(country_code, levels = levels3))
  }

  dd <- dd %>%
    select(country_code, other_sh, net_wealth_sh, inherit_gifts_sh) %>%
    pivot_longer(cols = c(other_sh, net_wealth_sh, inherit_gifts_sh),
                 names_to = "component", values_to = "share") %>%
    mutate(
      component_label = recode(component,
                               other_sh = "Other property taxes (residual)",
                               net_wealth_sh = "4200: Net wealth",
                               inherit_gifts_sh = "4300: Inheritance & gifts"),
      component_label = factor(component_label,
                               levels = c("Other property taxes (residual)", "4200: Net wealth", "4300: Inheritance & gifts"))
    )

  cap <- if (main_view) {
    paste0(
      "Shares computed from mean levels (ratio of means).\n",
      "Full OECD version exported as appendix. Source: OECD Global Revenue Statistics."
    )
  } else {
    "Shares computed from mean levels (ratio of means). Source: OECD Global Revenue Statistics."
  }

  p <- ggplot(dd, aes(x = share, y = country_code, fill = component_label)) +
    geom_col(width = 0.78) +
    scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_fill_manual(values = pal3) +
    guides(fill = guide_legend(nrow = 1)) +
    labs(
      title = "Within property taxes (OECD 4000): where do inheritance & gifts (4300) fit?",
      subtitle = subtitle,
      x = "Share within property taxes (4000)", y = NULL,
      caption = cap
    ) +
    theme_kof()

  save_png(p, out_file, width = 12, height = ifelse(main_view, 8.5, 14), res = 320)
  log_line("Wrote: ", out_file)
}

stats3_recent <- make_stats3(w, recent_start, recent_end)
plot_fig3(stats3_recent,
          subtitle = paste0(recent_start, "–", recent_end,
                            " average shares within 4000; main view (top ", main_n,
                            " by 4300 share + CHE/OECD + comparators)"),
          out_file = "outputs/figures/fig3_property_tax_mix_2018_2022_main.png",
          main_view = TRUE)

plot_fig3(stats3_recent,
          subtitle = paste0(recent_start, "–", recent_end, " average shares within 4000; full OECD (appendix)"),
          out_file = "outputs/figures/figA3_property_tax_mix_2018_2022_full_oecd.png",
          main_view = FALSE)

stats3_full <- make_stats3(w, full_start, full_end)
plot_fig3(stats3_full,
          subtitle = paste0(full_start, "–", full_end, " average shares within 4000; full OECD (appendix)"),
          out_file = "outputs/figures/figA3b_property_tax_mix_1990_2022_full_oecd.png",
          main_view = FALSE)

# ---------------------------
# FIG 4: Decomposition within 4300 — inheritance vs gifts (mean-level shares)
# Main view (recent) + appendix recent full + appendix full-period full
# Fix: keep full OECD coverage by explicitly representing missing 4310/4320 breakdowns.
# ---------------------------
make_stats4 <- function(df, y0, y1) {
  base <- df %>%
    filter(year >= y0, year <= y1) %>%
    group_by(country_code) %>%
    summarise(
      m4300 = mean(inheritance_gifts_pct_gdp, na.rm = TRUE),
      m4310 = mean(inheritance_pct_gdp, na.rm = TRUE),
      m4320 = mean(gifts_pct_gdp, na.rm = TRUE),
      has_4310 = any(!is.na(inheritance_pct_gdp)),
      has_4320 = any(!is.na(gifts_pct_gdp)),
      .groups = "drop"
    ) %>%
    filter(country_code != "OECD_REP") %>%
    filter(is.finite(m4300), m4300 > 0)

  base %>%
    mutate(
      inheritance_raw = if_else(is.finite(m4310), m4310 / m4300, NA_real_),
      gifts_raw       = if_else(is.finite(m4320), m4320 / m4300, NA_real_),

      inheritance_sh = inheritance_raw,
      gifts_sh       = gifts_raw,

      # Symmetric residual handling when only one component is available
      inheritance_sh = if_else(is.na(inheritance_sh) & !is.na(gifts_sh), pmax(0, 1 - gifts_sh), inheritance_sh),
      gifts_sh       = if_else(is.na(gifts_sh)       & !is.na(inheritance_sh), pmax(0, 1 - inheritance_sh), gifts_sh),

      # If neither 4310 nor 4320 is available, mark as "no breakdown"
      no_breakdown = is.na(inheritance_raw) & is.na(gifts_raw),
      no_breakdown_sh = if_else(no_breakdown, 1, 0),

      # Rescale to sum to 100% when both parts exist but do not exactly match total 4300
      s = inheritance_sh + gifts_sh,
      rescaled_flag = if_else(!no_breakdown & is.finite(s) & s > 0, abs(s - 1) > 0.05, FALSE),
      inheritance_sh = if_else(!no_breakdown & is.finite(s) & s > 0, inheritance_sh / s, inheritance_sh),
      gifts_sh       = if_else(!no_breakdown & is.finite(s) & s > 0, gifts_sh / s, gifts_sh),

      note = case_when(
        country_code == "CHE" ~ "CHE: gifts not separately reported",
        no_breakdown ~ "No 4310/4320 breakdown in OECD data",
        is.na(gifts_raw) & !is.na(inheritance_raw) ~ "Gifts not separately reported (residual)",
        is.na(inheritance_raw) & !is.na(gifts_raw) ~ "Inheritance not separately reported (residual)",
        rescaled_flag ~ "Subcomponents rescaled to sum to 100%",
        TRUE ~ ""
      )
    ) %>%
    select(country_code, m4300, inheritance_sh, gifts_sh, no_breakdown_sh, note)
}

pal2 <- c(
  "Inheritance (4310 / 4300)" = ORANGE,
  "Gifts (4320 / 4300 or residual)" = BLUE,
  "No breakdown available" = "grey75"
)

plot_fig4 <- function(stats4, subtitle, out_file, main_view = TRUE) {

  if (main_view) {
    # Main view: keep only countries with some decomposition info (exclude pure "no breakdown")
    stats_view <- stats4 %>% filter(no_breakdown_sh == 0)

    codes4 <- select_main_codes(stats_view, gifts_sh, top_n = main_n, keep = c("CHE", comparators))
    levels4 <- order_levels_desc(stats_view, gifts_sh, codes4)

    dd <- stats_view %>%
      filter(country_code %in% codes4) %>%
      mutate(country_code = factor(country_code, levels = levels4))

    comps <- c("inheritance_sh", "gifts_sh")
    comp_levels <- c("Inheritance (4310 / 4300)", "Gifts (4320 / 4300 or residual)")
  } else {
    # Appendix: include full OECD coverage (with explicit "no breakdown" bars in grey)
    levels4 <- stats4 %>%
      mutate(
        grp = if_else(no_breakdown_sh > 0, 1L, 0L),
        sort_key = if_else(no_breakdown_sh > 0, -Inf, gifts_sh)
      ) %>%
      arrange(grp, desc(sort_key), country_code) %>%
      pull(country_code) %>% unique()

    dd <- stats4 %>%
      mutate(country_code = factor(country_code, levels = levels4))

    comps <- c("inheritance_sh", "gifts_sh", "no_breakdown_sh")
    comp_levels <- c("Inheritance (4310 / 4300)",
                     "Gifts (4320 / 4300 or residual)",
                     "No breakdown available")
  }

  dd_long <- dd %>%
    select(country_code, all_of(comps)) %>%
    pivot_longer(cols = all_of(comps), names_to = "component", values_to = "share") %>%
    mutate(
      component_label = recode(component,
                               inheritance_sh = "Inheritance (4310 / 4300)",
                               gifts_sh = "Gifts (4320 / 4300 or residual)",
                               no_breakdown_sh = "No breakdown available"),
      component_label = factor(component_label, levels = comp_levels),
      share = replace_na(share, 0)
    )

  n_cty <- nlevels(dd_long$country_code)
  h <- ifelse(main_view, 8, dyn_height(n_cty, base = 6, per = 0.28, min_h = 12, max_h = 22))

  cap <- if (main_view) {
    paste0(
      "If one component is not separately reported, the other segment includes residual ('not separately identified').\n",
      "Segments may be rescaled to sum to 100% when needed. Source: OECD Global Revenue Statistics."
    )
  } else {
    paste0(
      "If one component is not separately reported, the other segment includes residual ('not separately identified'); grey indicates no 4310/4320 breakdown.\n",
      "Segments may be rescaled to sum to 100% when needed. Source: OECD Global Revenue Statistics."
    )
  }

  p <- ggplot(dd_long, aes(x = share, y = country_code, fill = component_label)) +
    geom_col(width = 0.78) +
    scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_fill_manual(values = pal2, drop = FALSE) +
    guides(fill = guide_legend(nrow = 1)) +
    labs(
      title = "Inside 4300: inheritance vs inter-vivos gifts",
      subtitle = subtitle,
      x = "Share within 4300", y = NULL,
      caption = cap
    ) +
    theme_kof()

  save_png(p, out_file, width = 12, height = h, res = 320)
  log_line("Wrote: ", out_file)
}

stats4_recent <- make_stats4(w, recent_start, recent_end)
plot_fig4(stats4_recent,
          subtitle = paste0(recent_start, "–", recent_end,
                            "; main view (top ", main_n, " by gifts share + CHE/comparators)"),
          out_file = "outputs/figures/fig4_inheritance_vs_gifts_2018_2022_main.png",
          main_view = TRUE)

plot_fig4(stats4_recent,
          subtitle = paste0(recent_start, "–", recent_end, "; full OECD (appendix)"),
          out_file = "outputs/figures/figA4_inheritance_vs_gifts_2018_2022_full_oecd.png",
          main_view = FALSE)

stats4_full <- make_stats4(w, full_start, full_end)
plot_fig4(stats4_full,
          subtitle = paste0(full_start, "–", full_end, "; full OECD (appendix)"),
          out_file = "outputs/figures/figA4b_inheritance_vs_gifts_1990_2022_full_oecd.png",
          main_view = FALSE)

# ---------------------------
# FIG 5: Europe-centered choropleth (heatmap map) — 2018–2022 avg share of total tax
# If sf stack not available, export Tableau-ready table and skip map gracefully.
# ---------------------------
map_metric <- w %>%
  filter(year >= recent_start, year <= recent_end) %>%
  group_by(country_code) %>%
  summarise(
    avg_share = mean(share_4300_total_tax_pct, na.rm = TRUE),
    avg_level = mean(inheritance_gifts_pct_gdp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(country_code != "OECD_REP") %>%
  filter(is.finite(avg_share))

# Tableau-ready (always)
readr::write_csv(
  map_metric %>% arrange(desc(avg_share)),
  "outputs/tables/table_map_metric_2018_2022.csv"
)
log_line("Wrote: outputs/tables/table_map_metric_2018_2022.csv")

has_map <- requireNamespace("sf", quietly = TRUE) &&
  requireNamespace("rnaturalearth", quietly = TRUE) &&
  requireNamespace("ggplot2", quietly = TRUE)

if (has_map) {
  suppressPackageStartupMessages({
    library(sf)
    library(rnaturalearth)
  })

  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # Europe-ish bbox centered around Switzerland (tight enough to keep focus)
  bbox <- sf::st_bbox(c(xmin = -15, ymin = 35, xmax = 40, ymax = 72), crs = sf::st_crs(world))
  europe <- sf::st_crop(world, bbox)

  europe <- europe %>%
    left_join(map_metric, by = c("iso_a3" = "country_code"))

  p5 <- ggplot(europe) +
    geom_sf(aes(fill = avg_share), color = "grey80", linewidth = 0.2) +
    geom_sf(data = europe %>% filter(iso_a3 == "CHE"),
            fill = NA, color = "black", linewidth = 0.6) +
    scale_fill_gradient(low = "white", high = ORANGE, na.value = "grey92") +
    labs(
      title = "Inheritance & gift taxes (4300) across OECD members (Europe focus)",
      subtitle = paste0(recent_start, "–", recent_end, ": average 4300 share of total tax revenue (%)"),
      fill = "% of total tax"
    ) +
    theme_kof() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )

  save_png(p5, "outputs/figures/fig5_map_europe_4300_share_total_2018_2022.png",
           width = 12, height = 7.5, res = 320)
  log_line("Wrote: outputs/figures/fig5_map_europe_4300_share_total_2018_2022.png")
} else {
  log_line("NOTE: Map packages not available (sf/rnaturalearth). Skipping Fig 5 map image.")
  log_line("      Use outputs/tables/table_map_metric_2018_2022.csv in Tableau if needed.")
}

log_line("")
log_line("Done. Figures saved to outputs/figures (PNG).")
