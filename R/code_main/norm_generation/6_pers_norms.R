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
packages <- c("readr")
pkg_loader(packages)
rm(packages)

## Change directory: if not .Rproj, it will set working directory to script filepath ####
wd_set_current()


# Data Importation ####
interesse <- read.csv("./data/data_interrim/interesse_final.csv")
gedrag_houding <- read.csv("./data/data_interrim/gedrag_houding_final.csv")
gedrag_houding_ouder <- read.csv("./data/data_interrim/gedrag_houding_ouder_final.csv")
gedrag_houding_leerkracht <- read.csv("./data/data_interrim/gedrag_houding_leerkracht_final.csv")
meta_data <- read.csv("./data/data_interrim/meta.csv")


# Data Preparation ####
## Create dataframe - 5 ranks ####
rank_3 <- data.frame(rank = 1:2)

gedrag_houding$mean
rank_5$numerieke_aanleg <- quantile(numerieke_aanleg, probs = seq(0, 1, 0.10), na.rm = TRUE) %>%
  .[c("10%", "30%", "70%", "90%")]





# Data Analyses - Exploration ####


# Data Analyses - Preparation ####


# Data Analyses - Main ####


# Data Analyses - Interpretation ####


# Data Analyses - Explication ####


# Data Finalization ####


# Data Exportation ####


# Script Clean-up ####


# File Report ####
