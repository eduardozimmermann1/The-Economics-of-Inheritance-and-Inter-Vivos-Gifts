# == 00_install_packages.R ==
# Minimal, no sink(), no prompts. Adds optional spatial stack for Fig. 5 (heatmap map).

options(repos = c(CRAN = "https://cloud.r-project.org"))
options(stringsAsFactors = FALSE)

pkgs_core <- c(
  "tidyverse", "janitor", "scales", "patchwork", "ggrepel",
  "haven", "ragg"
)

# Optional map stack (Fig. 5). If it fails, pipeline still runs + exports Tableau-ready table.
pkgs_map <- c("sf", "rnaturalearth", "rnaturalearthdata", "countrycode")

install_quiet <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(to_install) > 0) {
    tryCatch(
      install.packages(to_install, dependencies = TRUE),
      error = function(e) message("NOTE: install failed for: ", paste(to_install, collapse = ", "),
                                  "\nReason: ", conditionMessage(e))
    )
  }
}

install_quiet(pkgs_core)
install_quiet(pkgs_map)

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(scales)
  library(patchwork)
  library(ggrepel)
  library(haven)
  library(ragg)
})

dir.create("outputs/logs", recursive = TRUE, showWarnings = FALSE)
writeLines(capture.output(sessionInfo()), "outputs/logs/sessionInfo.txt")

cat("OK. Core packages loaded.\n")
cat("Optional map packages available?\n")
cat("  sf:", requireNamespace("sf", quietly = TRUE), "\n")
cat("  rnaturalearth:", requireNamespace("rnaturalearth", quietly = TRUE), "\n")
cat("  countrycode:", requireNamespace("countrycode", quietly = TRUE), "\n")
cat("\nWrote: outputs/logs/sessionInfo.txt\n")
