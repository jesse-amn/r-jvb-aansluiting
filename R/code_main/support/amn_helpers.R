# File Documentation & ToC ####
"
Author description:
J.F.J. (Jesse) van Bussel, psychometrician at AMN

Project description:
This is the master_helper file. Here all functions are combined and annotated.
From this file onwards, segment files may be created for case-specific projects,
where specific functions are needed. the seperate function scripts will be located
in the subfolder './single_functions'. Only finished functions should be added here.

File description:
This file will contain all helper functions, and should fungate as a database or
helper script. The packages are seperated by headers containing their function name.

Note: If the functions multiply and form natural categories, the headers may be
replaced with said categories.

Table of Contents:
- Environment set-up
  - pkg_loader(): loads (and if necessary, installs) a list of packages
  - wd_set_current(): changes dir to file location unless .Rproj is present
  - get_gdrive_basepath (): Returns the filepath for user's main google drive
  - get_gdrive_sharedpath (): Returns the filepath for shared drives from google drive
- Loading of files
  - find_gdrive(): Lists all absolute file paths based on pattern given
  - save_all_dfs(): Saves all dfs in global environment
- Other:
  - true_round(): true round
  - clear(): clears global env.
  - iqr_outlier(): removes iqr outliers

"

# Environment Set-up ####

# Loads packages, and installs if necessary. Takes a c().
pkg_loader <- function(packages) {
  options(warn = -1)
  loaded_packages <- character(0)
  error_packages <- character(0)
  suppressMessages(
    for (pkg in packages) {
      if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        tryCatch(
          {
            install.packages(pkg, dependencies = TRUE, quiet = TRUE)
            library(pkg, character.only = TRUE)
            loaded_packages <- c(loaded_packages, pkg)
          },
          error = function(e) {
            error_packages <- c(error_packages, pkg)
            library(crayon)
            cat("\n\033[31mFollowing package(s) raised an error:", pkg, "\033[97m\n")
          }
        )
      } else {
        loaded_packages <- c(loaded_packages, pkg)
      }
    }
  )

  if (length(loaded_packages) > 0) {
    cat("\nFollowing package(s) were loaded:", paste(loaded_packages, collapse = ", "), "\n")
  }
  options(warn = 0)
}


# Sets the working directory to the script's location, unless there is a .Rproj
# file in the current working directory.
wd_set_current <- function() {
  if (rstudioapi::isAvailable()) {
    current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
    proj_files <- list.files(current_dir, pattern = "\\.Rproj$", recursive = TRUE)

    while (length(proj_files) == 0 && current_dir != "/") {
      current_dir <- dirname(current_dir) # Move one level up
      proj_files <- list.files(current_dir, pattern = "\\.Rproj$", recursive = TRUE)
    }

    proj_files_absolute <- proj_files[!is.na(proj_files) & proj_files != ""]

    if (length(proj_files_absolute) > 0) {
      if (grepl("^/|^\\\\", proj_files_absolute[1])) {
        proj_files_absolute <- normalizePath(proj_files_absolute)
      } else {
        proj_files_absolute <- file.path(getwd(), proj_files_absolute)
      }
      cat("\033[31mA .Rproj file is found at:", proj_files_absolute, " \nWorking directory was not set.\033[97m\n")
    } else {
      setwd(dirname(rstudioapi::getSourceEditorContext()$path))
      cat("Current working directory set to:", dirname(rstudioapi::getSourceEditorContext()$path), "\n")
    }
  } else {
    cat("RStudio is not running. Cannot set the working directory using rstudioapi.\n")
  }
}


# getGoogleDriveBasePath() #####
# Returns the filepath for user's main google drive
get_gdrive_basepath <- function() {
  switch(Sys.info()[["sysname"]],
    Windows = {
      gDriveBase <- "G:\\"
    },
    Darwin = {
      gDriveBase <- paste0(
        "/Users/", Sys.getenv("LOGNAME"),
        "/Library/CloudStorage/GoogleDrive-",
        Sys.getenv("LOGNAME"), "@amn.nl"
      )
      if (!file.exists((gDriveBase))) {
        gDriveBase <- paste0(
          "/Users/", Sys.getenv("LOGNAME"),
          "/Library/CloudStorage/GoogleDrive-",
          tolower(Sys.getenv("LOGNAME")), "@amn.nl"
        )
      }
      if (!file.exists((gDriveBase))) {
        gDriveBase <- paste0(
          "/Users/", Sys.getenv("LOGNAME"),
          "/Downloads"
        )
      }
    }
  )
  return(gDriveBase)
}


# getGoogleSharedDrivePath ####
# Returns the filepath for shared drives from google drive
get_gdrive_sharedpath <- function() {
  for (sharedDriveAlias in c("Shared drives", "Gedeelde drives"))
  {
    if (file.exists(file.path(get_gdrive_basepath(), sharedDriveAlias))) {
      return(file.path(get_gdrive_basepath(), sharedDriveAlias))
    }
  }
  stop("\033[31mCouldn't find Google Drive's `Shared drives` folder.\033[97m\n")
}

# Loading of files ####

# Lists all absolute file paths based on pattern given
find_gdrive <- function(filename, foldername = NULL, shared = NULL) {
  if (!is.null(foldername)) {
    all_dirs <- list.dirs(get_gdrive_basepath(), recursive = TRUE, full.names = TRUE)
    main_dir <- grep(paste0("/", foldername, "$"), all_dirs, value = TRUE)
    if (length(main_dir) == 0) {
      cat(paste("\033[31mNo matching folder with name", foldername, "found.\033[97m\n"))
    }
  } else {
    main_dir <- get_gdrive_basepath()
  }

  if (!is.logical(shared)) {
    file_path <- list.files(main_dir, pattern = filename, recursive = TRUE, full.names = TRUE)
  } else if (shared == FALSE) {
    file_path <- list.files(paste0(main_dir, "/My Drive"), pattern = filename, recursive = TRUE, full.names = TRUE)
  } else if (shared == TRUE) {
    all_dirs <- list.dirs(get_gdrive_sharedpath(), recursive = TRUE, full.names = TRUE)
    main_dir <- grep(paste0("/", foldername, "$"), all_dirs, value = TRUE)
    file_path <- list.files(main_dir, pattern = filename, recursive = TRUE, full.names = TRUE)
  }

  if (length(file_path) == 0) {
    cat(paste("\033[31mNo matching file with name", filename, "found.\033[97m\n"))
  } else {
    return(file_path)
  }
}



# Save all existing df's in global environment: either specified path, else it will
# try ./data_input/ as is predefined in .RProj folder template
save_all_dfs <- function(path = NULL) {
  # Faster writing
  invisible(pkg_loader("data.table"))

  # Get list of all current df's and tibbles in global environment
  df_list <- ls(.GlobalEnv)[sapply(ls(.GlobalEnv), function(x) any(c("tbl_df", "tbl", "data.frame") %in% class(get(x))))]

  # If no path is specified, save to /data_input from regular .Rproj working dir
  if (is.null(path)) {
    for (df_name in df_list) {
      file_name <- paste0(df_name, ".csv")
      fwrite(get(df_name), file.path("./data_input/", file_name))
      print(file_name)
    }
  } else { # Saves at specified absolute path
    for (df_name in df_list) {
      file_name <- paste0(df_name, ".csv")
      fwrite(get(df_name), file.path(path, file_name))
      print(file_name)
    }
  }
}


# Other ####
# Fixes the Base-R round function to avoid the problem that round(2.5) = round (1.5)
# By appending smallest decimal that R will accept: 2.225074×10^−308, as found in
# .Machine$double.xmin
true_round <- function(x, digits = 0) {
  result <- round((x + 0.000000000000001), digits = digits)
  return(result)
}

# Clear the global environment
clear <- function() {
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
}

# Removes the IQR outliers, and returns
iqr_outliers <- function(x) {
  # Calculate IQR
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1

  # Identify outliers
  outliers <- x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)

  # Replace outliers with NA
  x[outliers] <- NA

  return(x)
}
