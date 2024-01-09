# File Documentation ####
"
Author description:

Project description:

File description:

Table of Contents:
  1. File Documentation:  Script purpose and explanation
  2. Environment Set-up:  Options, directories, packages, and functions
  3. Data Importation:    Loading and formatting of data
  4. Data Preparation:    Change and transformation of data
  5. Data Analyses:
    1. Exploration:         Descriptives, missingness, and outliers
    2. Preparation:         Assumptions,
    3. Main:                Model creation and execution
    4. Interpretation:      Significance, fit, and relations
    5. Explication:         Graphs and tables
  6. Data Finalization:   Preparation and combination of export products
  7. Data Exportation:    Exportation of final data and related products
  8. Script Clean-up:     Object removal and process terminations
  9. File Report:         Notations, comments, and references

"

cap_norms <- function() {
  # Environment Set-up ####
  ##  General ####
  library(here)
  options(scipen = 999)

  ## Get helpers ####
  if (any(grepl("/", list.files(recursive = TRUE, pattern = "amn_helpers.R")))) {
    source(here(list.files(recursive = TRUE, pattern = "amn_helpers.R")))
  } else {
    source(list.files(recursive = TRUE, pattern = "amn_helpers.R"))
  }

  ## Load necessary packages: Fill into c() with comma-seperated quotation marks ####
  packages <- c("readr", "magrittr")
  pkg_loader(packages)
  rm(packages)

  ## Change directory: if not .Rproj, it will set working directory to script filepath ####
  wd_set_current()


  # Data Importation ####
  cap_data <- read.csv("./data/data_interrim/cap_final.csv")
  meta_data <- read.csv("./data/data_interrim/meta.csv")


  # Data Preparation ####
  ## Create dataframe - 5 ranks ####
  rank_5 <- data.frame(rank = 1:4)
  rank_10 <- data.frame(rank = 1:9)
  rank_percent <- data.frame(percent = 1:100)

  ## Create composite sum scores where needed ####
  numerieke_aanleg <- rowSums(cap_data[, c("cijfers_sum", "sommenmaken_sum")])
  verbale_aanleg <- rowSums(cap_data[, c("betekenissen_sum", "woorden_sum")])
  taalverzorging <- rowSums(cap_data[, c("lezen_deel1_sum", "lezen_deel2_sum", "opzoeken_sum")])
  leesteksten <- rowSums(cap_data[, c("spelling_sum", "leestekens_sum", "werkwoordspelling_sum")])


  # Data Analyses - Main ####
  ## Calculate rank 5 tresholds ####
  ### Capacity - underlying components ####
  rank_5$numerieke_aanleg <- quantile(numerieke_aanleg, probs = seq(0, 1, 0.10), na.rm = TRUE) %>%
    .[c("10%", "30%", "70%", "90%")]
  rank_5$verbale_aanleg <- quantile(verbale_aanleg, probs = seq(0, 1, 0.10), na.rm = TRUE) %>%
    .[c("10%", "30%", "70%", "90%")]
  rank_5$concreet_redeneren <- quantile(cap_data$woordenrelateren_sum, probs = seq(0, 1, 0.10), na.rm = TRUE) %>%
    .[c("10%", "30%", "70%", "90%")]
  rank_5$ruimtelijk_inzicht <- quantile(cap_data$componenten_sum, probs = seq(0, 1, 0.10), na.rm = TRUE) %>%
    .[c("10%", "30%", "70%", "90%")]
  rank_5$abstract_redeneren <- quantile(cap_data$figuren_sum, probs = seq(0, 1, 0.10), na.rm = TRUE) %>%
    .[c("10%", "30%", "70%", "90%")]
  rank_5$snelheid_en_nauwkeurigheid <- quantile(cap_data$verschillen_sum, probs = seq(0, 1, 0.10), na.rm = TRUE) %>%
    .[c("10%", "30%", "70%", "90%")]

  ### Rekenen - underlying components ####
  rank_5$getallen <- quantile(cap_data$getallen_sum, probs = seq(0, 1, 0.10), na.rm = TRUE) %>%
    .[c("10%", "30%", "70%", "90%")]
  rank_5$metenenmeetkunde <- quantile(cap_data$metenenmeetkunde_sum, probs = seq(0, 1, 0.10), na.rm = TRUE) %>%
    .[c("10%", "30%", "70%", "90%")]
  rank_5$verbanden <- quantile(cap_data$verbanden_sum, probs = seq(0, 1, 0.10), na.rm = TRUE) %>%
    .[c("10%", "30%", "70%", "90%")]
  rank_5$verhoudingen <- quantile(cap_data$verhoudingen_sum, probs = seq(0, 1, 0.10), na.rm = TRUE) %>%
    .[c("10%", "30%", "70%", "90%")]

  ### Taal - underlying components ####
  rank_5$leesteksten <- quantile(leesteksten, probs = seq(0, 1, 0.10), na.rm = TRUE) %>%
    .[c("10%", "30%", "70%", "90%")]
  rank_5$taalverzorging <- quantile(taalverzorging, probs = seq(0, 1, 0.10), na.rm = TRUE) %>%
    .[c("10%", "30%", "70%", "90%")]

  ## Calculate rank 10 tresholds ####
  ### Capacity ####
  cap_means <- rowSums(cap_data[, c("cijfers_sum", "sommenmaken_sum", "betekenissen_sum", "woorden_sum", "woordenrelateren_sum", "figuren_sum", "componenten_sum", "verschillen_sum")]) / nrow(meta_data[meta_data$PackageFriendlyName %in% c("cijfers", "sommenmaken", "betekenissen", "woorden", "woordenrelateren", "figuren", "componenten", "verschillen"), ])
  rank_10$capacity <- quantile(cap_means, probs = seq(0, 1, 0.10), na.rm = TRUE) %>%
    .[c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%")]

  ### Rekenen ####
  rekenen_means <- rowSums(cap_data[, c("getallen_sum", "metenenmeetkunde_sum", "verbanden_sum", "verhoudingen_sum")]) / nrow(meta_data[meta_data$PackageFriendlyName %in% c("getallen", "metenenmeetkunde", "verbanden", "verhoudingen"), ])
  rank_10$rekenen <- quantile(rekenen_means, probs = seq(0, 1, 0.10), na.rm = TRUE) %>%
    .[c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%")]

  ### Taal ####
  taal_means <- rowSums(cap_data[, c("lezen_deel1_sum", "lezen_deel2_sum", "opzoeken_sum", "spelling_sum", "leestekens_sum", "werkwoordspelling_sum")]) / nrow(meta_data[meta_data$PackageFriendlyName %in% c("lezen_deel1", "lezen_deel2", "opzoeken", "spelling", "leestekens", "werkwoordspelling"), ])
  rank_10$taal <- quantile(taal_means, probs = seq(0, 1, 0.10), na.rm = TRUE) %>%
    .[c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%")]

  ## Calculate percentage thresholds ####
  rank_percent$getallen <- quantile(seq_len(nrow(meta_data[meta_data$PackageFriendlyName == "getallen", ])), probs = seq(0.01, 1, 0.01))
  rank_percent$metenenmeetkunde <- quantile(seq_len(nrow(meta_data[meta_data$PackageFriendlyName == "metenenmeetkunde", ])), probs = seq(0.01, 1, 0.01))
  rank_percent$lezen <- quantile(seq_len(nrow(meta_data[meta_data$PackageFriendlyName %in% c("lezen_deel1", "lezen_deel2", "opzoeken"), ])), probs = seq(0.01, 1, 0.01))
  rank_percent$taalverzorging <- quantile(seq_len(nrow(meta_data[meta_data$PackageFriendlyName %in% c("spelling", "leestekens", "werkwoordspelling"), ])), probs = seq(0.01, 1, 0.01))

  # Data Exportation ####
  write.csv(rank_5, "./data/data_output/cap_ranks_5.csv", row.names = FALSE)
  write.csv(rank_10, "./data/data_output/cap_ranks_10.csv", row.names = FALSE)
  write.csv(rank_percent, "./data/data_output/cap_ranks_percent.csv", row.names = FALSE)

  # Script Clean-up ####
  rm(list = ls())
}

# File Report ####
