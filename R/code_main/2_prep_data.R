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
packages <- c("readr", "dplyr")
pkg_loader(packages)
rm(packages)

## Change directory: if not .Rproj, it will set working directory to script filepath ####
wd_set_current()


# Data Importation ####
## Main data ####
file_list <- list.files("./data/data_input", pattern = ".csv", full.names = TRUE) %>% .[!grepl("meta", .)]
for (df in file_list) {
  assign(
    gsub(
      ".csv", "", basename(df)
    ),
    read.csv(df)
  )
}
rm(df)

## Metadata ####
meta_df <- read.csv("./data/data_input/meta_file.csv")


# Data Preparation ####
## Main data ####
for (name in basename(file_list) %>% gsub(".csv", "", .)) {
  # get df localy
  df <- get(name)

  # filter df
  df <- df[, grepl("student_name|birth_date|student_number|package_origin|package_duration_raw|item.SCORE", colnames(df))]

  # Remove completely empty columns
  df <- df[, colSums(!is.na(df)) > 0]

  # Remove completely empty rows
  df <- df[complete.cases(df), ]

  assign(name, df, envir = .GlobalEnv)
}
rm(file_list, name, df)

## Metadata ####
meta_df <- meta_df[, c(
  "ID", "PackageFriendlyName", "Factor", "CorrectAnswer"
)]


# Data Exportation ####
save_all_dfs("./data/data_interrim")

# Script Clean-up ####


# File Report ####
