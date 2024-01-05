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
item_analyses <- TRUE



# TODO: Meetenmeetkunde

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
  print(name)
  # get df localy
  df <- get(name)

  # filter df
  df <- df[, grepl("student_name|birth_date|student_number|gender|package_origin|package_duration_raw|item.SCORE", colnames(df))]
  df <- df[, !grepl("intro_prac|intro_info|_wrong", colnames(df))]

  # Remove completely empty columns
  df <- df[, colSums(!is.na(df)) > 0]

  # Remove completely empty rows
  df <- df[rowSums(is.na(df)) != ncol(df), ]

  # Remove ".SCORE" from column names
  colnames(df) <- gsub("_item.SCORE|_item.SCORES", "", colnames(df))

  assign(name, df, envir = .GlobalEnv)
}
rm(name, df)

# NA was used for wrong. Replace with 0
woordenrelateren[is.na(woordenrelateren)] <- 0
sommenmaken$ASL_sommenmaken_012_NEW[is.na(sommenmaken$ASL_sommenmaken_012_NEW)] <- 0


'
colnames(metenenmeetkunde)
summary(metenenmeetkunde)
head(metenenmeetkunde)
# Count occurrences of 0s and 1s for specific variables in metenenmeetkunde
test <- metenenmeetkunde
df <- metenenmeetkunde[, grepl("0F_007", colnames(metenenmeetkunde))]

count_table <- data.frame(variable = character(), zeros = integer(), ones = integer(), stringsAsFactors = FALSE)
for (variable in colnames(df)) {
  count <- table(df[[variable]])
  zeros <- count[["0"]]
  ones <- count[["1"]]
  count_table <- rbind(count_table, data.frame(variable = variable, zeros = zeros, ones = ones))
}



'


if (item_analyses == TRUE) {
  save_all_dfs("./data/data_interrim")
  rm(list = ls())

  sink("/dev/null")
  suppressWarnings(suppressMessages(source(paste0("./R/code_extra/factor_analyses.R"), echo = FALSE)))
  sink()

  sink("/dev/null")
  suppressWarnings(suppressMessages(source(paste0("./R/code_extra/factor_analyses_simple.R"), echo = FALSE)))
  sink()

  sink("/dev/null")
  suppressWarnings(suppressMessages(source(paste0("./R/code_extra/IRT.R"), echo = FALSE)))
  sink()

  sink("/dev/null")
  suppressWarnings(suppressMessages(source(paste0("./R/code_extra/item_analyses_personality.R"), echo = FALSE)))
  sink()
}













missing_rows <- data.frame()
percentage_threshold <- 0.2 # Change this value to your desired percentage threshold

for (name in basename(file_list) %>% gsub(".csv", "", .)) {
  df <- get(name)
  missing_count <- sum(rowSums(is.na(df)) > ncol(df) * percentage_threshold)
  total_rows <- nrow(df)
  percentage_missing <- missing_count / total_rows * 100
  missing_rows <- rbind(missing_rows, data.frame(Dataframe = name, MissingRows = missing_count, PercentageMissing = percentage_missing))
}
missing_rows

missing_columns <- data.frame()
percentage_threshold <- 0.7 # Change this value to your desired percentage threshold

for (name in basename(file_list) %>% gsub(".csv", "", .)) {
  df <- get(name)
  missing_count <- sum(colSums(is.na(df)) > nrow(df) * percentage_threshold)
  total_columns <- ncol(df)
  percentage_missing <- missing_count / total_columns * 100
  missing_columns <- rbind(missing_columns, data.frame(Dataframe = name, MissingColumns = missing_count, PercentageMissing = percentage_missing, ColumnNames = paste(colnames(df)[colSums(is.na(df)) > nrow(df) * percentage_threshold], collapse = ", ")))
}
missing_columns


## Metadata ####
meta_df <- meta_df[, c(
  "ID", "PackageFriendlyName", "Factor", "CorrectAnswer"
)]


# Data Exportation ####
save_all_dfs("./data/data_interrim")

# Script Clean-up ####
rm(list = ls())

# File Report ####
