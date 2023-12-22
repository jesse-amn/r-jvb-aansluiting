# File Documentation ####
"
Author description:

  J.F.J. (Jesse) van Bussel, methodologist/psychometrician at AMN

Project description:

  Talentscan norm-generation and application.

File description:

  This script deletes all files of the norm generation processs located in
  ./data/data_input/, ./data/data_interrim/, and ./data/data_output/.

Table of Contents:
  1. File Documentation:  Script purpose and explanation
  2. Environment Set-up:  Options, directories, and packages
  3. Data Importation:    Loading and formatting of data
  4. Script Clean-up      Object removal and process terminations
  5. File Report          Notations, comments, and references

"


# Environment Set-up ####
# General
library(here)
options(scipen = 999)


# Get helpers
if (any(grepl("/", list.files(recursive = TRUE, pattern = "amn_helpers")))) {
  source(here(list.files(recursive = TRUE, pattern = "amn_helpers")))
} else {
  source(list.files(recursive = TRUE, pattern = "amn_helpers"))
}


# Load necessary packages: Fill into c() with comma-seperated quotation marks
packages <- c()
pkg_loader(packages)


# Change directory: if not .Rproj, it will set working directory to script filepath
wd_set_current()


# Data Importation ####
list <- c(
  list.files(path = "./data/data_input", pattern = ".csv", recursive = TRUE, full.names = TRUE),
  list.files(path = "./data/data_interrim", pattern = ".csv", recursive = TRUE, full.names = TRUE),
  list.files(path = "./data/data_output", pattern = ".csv", recursive = TRUE, full.names = TRUE)
)


# Script Clean-up ####
for (csv in list) {
  file.remove(csv)
  cat(paste("Deleted", basename(csv), "\n"))
}


# File Report ####
rm(list = ls())
