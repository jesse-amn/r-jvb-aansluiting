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
interesse <- read.csv("./data/data_interrim/interesse.csv")
gedrag_houding <- read.csv("./data/data_interrim/gedrag_houding.csv")
gedrag_houding_ouder <- read.csv("./data/data_interrim/gedrag_houding_ouder.csv")
gedrag_houding_leerkracht <- read.csv("./data/data_interrim/gedrag_houding_leerkracht.csv")


# Data Preparation ####
for (name in c("interesse", "gedrag_houding", "gedrag_houding_ouder", "gedrag_houding_leerkracht")) {
  print(name)
  ### Get data ####
  local_df <- get(name)

  ### Append number of questions, sum correct, and mean ###
  local_df$total <- rowSums(!is.na(local_df[, !grepl("package_duration|student|gender|birth_date|total|mean|sum", colnames(local_df))]))
  local_df$sum <- rowSums(local_df[, !grepl("package_duration|student|gender|birth_date|total|mean|sum", colnames(local_df))], na.rm = TRUE)
  local_df$mean <- ifelse(is.na(local_df$sum), NA, local_df$sum / local_df$total)
  local_df$mean <- ifelse(is.na(local_df$sum), NA, local_df$mean)

  if (name == "gedrag_houding") {
    for (sub in c("concentratie", "contact_met_leeftijdsgenoten", "interesse_in_school", "motivatie_om_te_presteren", "open", "samenwerken", "stabiel", "zeflvertrouwen", "zorgvuldig")) {

    }
  }
  if (name == "gedrag_houding_ouder") {
    for (sub in c("concentratie", "contact_met_leeftijdsgenoten", "interesse_in_school", "motivatie_om_te_presteren", "open", "samenwerken", "stabiel", "zeflvertrouwen", "zorgvuldig")) {

    }
  }
  if (name == "gedrag_houding_leerkracht") {
    for (sub in c("concentratie", "contact_met_leeftijdsgenoten", "interesse_in_school", "motivatie_om_te_presteren", "open", "samenwerken", "stabiel", "zeflvertrouwen", "zorgvuldig")) {

    }
  } # LEFT OFF: CREATE SUBDOMAIN SUM/MEAN/TOTAL

  ### Assign data ####
  assign(name, local_df)

  ### Export data ####
  write_csv(local_df, paste0("./data/data_interrim/", name, ".csv"))

  ### remove local data ####
  rm(name, local_df)
}


## Create final personality score df ####
for (name in c("interesse", "gedrag_houding", "gedrag_houding_ouder", "gedrag_houding_leerkracht")) {
  ##### Print ####
  print(name)
  ##### Get data ####
  local_df <- get(name)

  ##### Filter local_df rows ####
  local_df <- local_df[, c("student_name", "sum", "mean")]
  local_df <- local_df[!duplicated(local_df$student_name), ]

  ##### Assign data ####
  assign(name, local_df)

  ##### Export data ####
  write_csv(local_df, paste0("./data/data_interrim/", name, "_final.csv"))

  ##### remove local data ####
  rm(name, local_df)
}

# Script Clean-up ####
rm(list = ls())

# File Report ####
