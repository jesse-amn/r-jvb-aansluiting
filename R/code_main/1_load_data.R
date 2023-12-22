# File Documentation ####
"
Author description:

  J.F.J. (Jesse) van Bussel, methodologist/psychometrician at AMN

Project description:



File description:

  This file loads all necessary data from the google drive that is accesible
  to all employees of AMN to the local device.

Table of Contents:
  # 1. File Documentation: Script purpose and explanation
  # 2. Environment Set-up: Options, directories, packages, and functions
  # 3. Personality Function
  # 4. Capacity Function
  # 5. Data Importation: Loading and formatting of data
  # 6. Data Exportation: Exportation of final data and related products
  # 7. Script Clean-up: Object removal and process terminations
"

path <- "/Export Results/Export Aansluiting Group 7 - 21.12.2023"

load_data <- function(path, output = NULL) {
  # Environment Set-up ####
  ## General ####
  library(here)
  options(scipen = 999)

  ## Get helpers ####
  if (any(grepl("/", list.files(recursive = TRUE, pattern = "amn_helpers")))) {
    source(here(list.files(recursive = TRUE, pattern = "amn_helpers")))
  } else {
    source(list.files(recursive = TRUE, pattern = "amn_helpers"))
  }

  ## Load necessary packages: Fill into c() with comma-seperated quotation marks ####
  packages <- c("readr")
  pkg_loader(packages)
  rm(packages)

  ## Change directory: if not .Rproj, it will set working directory to script filepath ####
  wd_set_current()


  # Data Importation ####
  ## Metadata ####
  meta_file <- read.csv(
    list.files(
      path = paste0(
        get_gdrive_sharedpath(),
        path, "/additional_files/"
      ),
      pattern = "aansluiting_items_metadata.csv", full.names = TRUE
    ),
    sep = ";"
  )

  ## Main data ####
  file_list <- list.files(
    path = paste0(
      get_gdrive_sharedpath(),
      path, "/merged_packages/"
    ),
    pattern = "merged_nl", full.names = TRUE
  )

  ### Load data ####
  for (df in file_list) {
    name <- gsub("merged_nl_ASL7_", "", basename(df))
    name <- gsub("_feb2018.txt", "", name)
    print(name)

    data <- read.csv(df, header = TRUE, sep = "\t")

    assign(name, data, envir = .GlobalEnv)
    rm(name, data)
  }

  ### Remove temporary files ####
  rm(file_list, df, path)


  # Data Finalization ####
  ## Change column names ####
  ### Leerkracht ####
  colnames(gedrag_houding_leerkracht1) <- gsub("leerkracht1", "leerkracht", colnames(gedrag_houding_leerkracht1))
  colnames(gedrag_houding_leerkracht2) <- gsub("leerkracht2", "leerkracht", colnames(gedrag_houding_leerkracht2))

  ### Ouder ####
  colnames(gedrag_houding_ouder_1) <- gsub("ouder_1", "ouder", colnames(gedrag_houding_ouder_1))
  colnames(gedrag_houding_ouder_2) <- gsub("ouder_2", "ouder", colnames(gedrag_houding_ouder_2))

  ## Create package origin column ####
  ### Leerkracht ####
  gedrag_houding_leerkracht1$package_origin <- "1"
  gedrag_houding_leerkracht2$package_origin <- "2"

  ### Ouder ####
  gedrag_houding_ouder_1$package_origin <- "1"
  gedrag_houding_ouder_2$package_origin <- "2"

  ## Combine df's when equal ####
  ### Leerkracht ####
  if (all.equal(colnames(gedrag_houding_leerkracht1), colnames(gedrag_houding_leerkracht2)) == TRUE) {
    # Combine df's
    gedrag_houding_leerkracht <- rbind(gedrag_houding_leerkracht1, gedrag_houding_leerkracht2)
    assign("gedrag_houding_leerkracht", gedrag_houding_leerkracht, envir = .GlobalEnv)

    # Remove temporary df's
    rm(gedrag_houding_leerkracht1, gedrag_houding_leerkracht2, envir = .GlobalEnv)
  } else {
    stop("Column names of gedrag_houding_leerkracht1 and gedrag_houding_leerkracht2 are not equal.")
  }

  ### Ouder ####
  if (all.equal(colnames(gedrag_houding_ouder_1), colnames(gedrag_houding_ouder_2)) == TRUE) {
    # Combine df's
    gedrag_houding_ouder <- rbind(gedrag_houding_ouder_1, gedrag_houding_ouder_2)
    assign("gedrag_houding_ouder", gedrag_houding_ouder, envir = .GlobalEnv)

    # Remove temporary df's
    rm(gedrag_houding_ouder_1, gedrag_houding_ouder_2, envir = .GlobalEnv)
  } else {
    stop("Column names of gedrag_houding_ouder_1 and gedrag_houding_ouder_2 are not equal.")
  }


  # Data Exportation ####
  assign("meta_file", meta_file, envir = .GlobalEnv)
  save_all_dfs("./data/data_input")


  # Script Clean-up ####
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
}

load_data(path = path)
