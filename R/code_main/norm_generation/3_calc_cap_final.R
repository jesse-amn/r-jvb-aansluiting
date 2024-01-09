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

calc_cap_final <- function() {
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
  ## File list ####
  file_list <- list.files("./data/data_interrim", pattern = ".csv", full.names = TRUE) %>% .[!grepl("meta", .)]
  for (df in file_list) {
    assign(
      gsub(
        ".csv", "", basename(df)
      ),
      read.csv(df)
    )
  }
  rm(df)

  # Data Preparation ####
  ## Append Capacity ####
  for (name in basename(file_list[!grepl("gedrag_houding|interesse", file_list)]) %>% gsub(".csv", "", .)) {
    print(name)
    ### Get data ####
    local_df <- get(name)

    ### Append number of questions, sum correct, and mean ###
    local_df$total <- ncol(local_df[, !grepl("package_duration|student|gender|birth_date|total|mean|sum", colnames(local_df))])
    local_df$sum <- rowSums(local_df[, !grepl("package_duration|student|gender|birth_date|total|mean|sum", colnames(local_df))], na.rm = TRUE)
    local_df$mean <- ifelse(is.na(local_df$sum), NA, local_df$sum / local_df$total)
    local_df$mean <- ifelse(is.na(local_df$sum), NA, local_df$mean)

    ### Assign data ####
    assign(name, local_df)

    ### Export data ####
    write_csv(local_df, paste0("./data/data_interrim/", name, ".csv"))

    ### remove local data ####
    rm(name, local_df)
  }

  ## Create final capacity score df ####
  ### Create init df ####
  #### Get unique names ####
  unique_names <- list()
  for (name in basename(file_list[!grepl("gedrag_houding|interesse", file_list)]) %>% gsub(".csv", "", .)) {
    ##### Get data ####
    local_df <- get(name)

    ##### Remove duplicated rows based on student_name ###
    local_df <- local_df[!duplicated(local_df$student_name), ]

    ##### Append unique names to list ####
    unique_names <- c(unique_names, as.character(local_df$student_name))
    ##### remove duplicated ####
    unique_names <- unique(unique_names)

    ##### remove local data ####
    rm(name, local_df)
  }

  ##### unlist ####
  unique_names <- unlist(unique_names)

  #### Create df ####
  final_df <- data.frame(student_name = unique_names)

  for (name in basename(file_list[!grepl("gedrag_houding|interesse", file_list)]) %>% gsub(".csv", "", .)) {
    ##### Print ####
    print(name)
    ##### Get data ####
    local_df <- get(name)

    ##### Filter local_df rows ####
    local_df <- local_df[, c("student_name", "sum", "mean")]
    local_df <- local_df[local_df$student_name %in% unique_names, ]
    local_df <- local_df[!duplicated(local_df$student_name), ]

    ##### Merge data ####
    final_df <- merge(final_df, local_df[, c("student_name", "sum", "mean")], by = "student_name", all.x = TRUE)

    ##### Rename column in final_df ####
    names(final_df)[names(final_df) == "sum"] <- paste0(name, "_sum")
    names(final_df)[names(final_df) == "mean"] <- paste0(name, "_mean")

    ##### remove local data ####
    rm(name, local_df)
  }


  # Data Exportation ####
  ## save final_df ####
  write_csv(final_df, "./data/data_interrim/cap_final.csv")


  # Script Clean-up ####
  rm(list = ls())
}

# File Report ####
'
Number of duplicate students per df.

r$> for (name in basename(file_list[!grepl("gedrag_houding|interesse", file_list)]) %>% gsub(".csv", "", .)) {
      ### Get data ####
      local_df <- get(name)

      print(name)
      # Check for duplicates in a specific column
      print(sum(duplicated(local_df$student_name)))
    }
[1] "betekenissen"
[1] 157
[1] "cijfers"
[1] 159
[1] "componenten"
[1] 144
[1] "figuren"
[1] 147
[1] "getallen"
[1] 133
[1] "leestekens"
[1] 111
[1] "lezen_deel1"
[1] 140
[1] "lezen_deel2"
[1] 117
[1] "metenenmeetkunde"
[1] 111
[1] "opzoeken"
[1] 126
[1] "sommenmaken"
[1] 145
[1] "spelling"
[1] 14
[1] "verbanden"
[1] 125
[1] "verhoudingen"
[1] 111
[1] "verschillen"
[1] 156
[1] "werkwoordspelling"
[1] 112
[1] "woorden"
[1] 144
[1] "woordenrelateren"
[1] 149

'
