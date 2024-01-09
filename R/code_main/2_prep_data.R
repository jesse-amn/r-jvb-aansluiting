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

# Parameters ####
item_analyses <- TRUE


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
## Duraiton data ####
for (name in basename(file_list) %>% gsub(".csv", "", .)) {
  ## get df localy ####
  local_df <- get(name)

  ## filter local_df ####
  local_df <- local_df[, grepl("student_name|duration", colnames(local_df))]

  ## Change comma to dot ####
  local_df[, !grepl("student_name|package_duration", colnames(local_df))] <- lapply(local_df[, !grepl("student_name|package_duration", colnames(local_df))], function(x) gsub(",", ".", x))

  ## Change to numeric ####
  local_df[, !grepl("student_name|package_duration", colnames(local_df))] <- lapply(local_df[, !grepl("student_name|package_duration", colnames(local_df))], function(x) as.numeric(x))

  ## create subdirectory if it does not exist yet ####
  if (!dir.exists("./data/data_interrim/duration")) {
    dir.create("./data/data_interrim/duration")
  }
  ## Save df's ####
  write_csv(local_df, paste0("./data/data_interrim/duration/", name, "_duration.csv"))
}

## Main data ####
### Loop over dataframes and clean them up ####
for (name in basename(file_list) %>% gsub(".csv", "", .)) {
  #### get local_df localy ####
  local_df <- get(name)

  #### filter local_df ####
  local_df <- local_df[, grepl("student_name|birth_date|student_number|gender|package_origin|package_duration_raw|item.SCORE", colnames(local_df))]
  local_df <- local_df[, !grepl("intro_prac|intro_info|_wrong", colnames(local_df))]

  #### Remove completely empty columns ####
  local_df <- local_df[, colSums(!is.na(local_df)) > 0]

  #### Remove completely empty rows ####
  local_df <- local_df[rowSums(is.na(local_df)) != ncol(local_df), ]

  #### Remove ".SCORE" from selected column names ####
  colnames(local_df) <- gsub("_item.SCORE|_item.SCORES", "", colnames(local_df))

  #### Remove questions ending on MM (only applicable for metenenmeetkunde) ####
  local_df <- local_df[, !grepl("MM$|MM_NEW$", colnames(local_df))]

  #### Assign back to global environment ####
  assign(name, local_df, envir = .GlobalEnv)
}
#### Remove name and local_df from local environment ####
rm(name, local_df)

## Capacity, replace NA's with 0's ####
cap_files <- basename(file_list) %>% gsub(".csv", "", .)
cap_files <- cap_files[!grepl("gedrag_houding|interesse", cap_files)]

### itereate and replace NA's with 0's ####
for (name in cap_files) {
  local_df <- get(name)

  #### selected columns ####
  cols_to_exclude <- c("gender", "student_number", "student_name", "birth_date")
  if (any(grepl("package_duration_raw", colnames(local_df)))) {
    cols_to_exclude <- c(cols_to_exclude, "package_duration_raw")
  }

  #### Replace NA's with 0's in selected columns #####
  local_df <- local_df %>%
    mutate_at(vars(-one_of(cols_to_exclude)), ~ ifelse(is.na(.), 0, .))

  #### Assign back to global environment ####
  assign(name, local_df, envir = .GlobalEnv)
}

### Remove rows of impossible values in metenenmeetkunde ####
metenenmeetkunde <- metenenmeetkunde[apply(metenenmeetkunde[, grepl("^ASL", colnames(metenenmeetkunde))], 1, function(x) all(x %in% c(0, 1))), ]

## Metadata ####
meta_df <- meta_df[, c(
  "ID", "PackageFriendlyName", "Factor", "CorrectAnswer"
)]

## Item analyses fork ####
if (item_analyses == TRUE) {
  ### save all dfs ####
  save_all_dfs("./data/data_interrim")
  rm(list = ls())

  ### Full factor Analyses ####
  sink("/dev/null")
  suppressWarnings(suppressMessages(source(paste0("./R/code_extra/factor_analyses.R"), echo = FALSE)))
  sink()

  ### Simple factor Analyses ####
  sink("/dev/null")
  suppressWarnings(suppressMessages(source(paste0("./R/code_extra/factor_analyses_simple.R"), echo = FALSE)))
  sink()

  ### IRT Analyses ####
  sink("/dev/null")
  suppressWarnings(suppressMessages(source(paste0("./R/code_extra/IRT.R"), echo = FALSE)))
  sink()

  ### Item Analyses for non-binary items (gedrag_houding & interesse) ####
  sink("/dev/null")
  suppressWarnings(suppressMessages(source(paste0("./R/code_extra/item_analyses_personality.R"), echo = FALSE)))
  sink()
} else {

  # Data Exportation ####
  save_all_dfs("./data/data_interrim")


  # Script Clean-up ####
  rm(list = ls())
}

# File Report ####
