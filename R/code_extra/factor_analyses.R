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
    1.. Main:                Model creation and execution
  6. Data Exportation:    Exportation of final data and related products
  7. Script Clean-up:     Object removal and process terminations
  8 . File Report:         Notations, comments, and references
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
packages <- c("readr", "psych", "lavaan")
pkg_loader(packages)
rm(packages)

## Change directory: if not .Rproj, it will set working directory to script filepath ####
wd_set_current()

## Create script-specific functions

create_cfa_model <- function(df) {
  variables <- colnames(df[, !grepl("package_duration|student_number|student_name|birth_date|gender", colnames(df))])

  cfa_model <- paste0("f1", " =~ ", paste(variables, collapse = " + "))

  return(cfa_model)
}


# Data Importation ####
## Main data ####
for (df in list.files("./data/data_interrim", pattern = ".csv", full.names = TRUE)) {
  assign(
    gsub("./data/data_interrim/", "", gsub(".csv", "", df)),
    read_csv(df)
  )
  rm(df)
}


# Data Preparation ####
## Get object names ####
### Capture the state of the global environment at the start of the script ####
initial_env <- ls(envir = .GlobalEnv)

### Get the names of the data frames ####
data_frames <- sapply(initial_env, function(x) {
  classes <- class(get(x, envir = .GlobalEnv))
  any(grepl("spec_tbl_df|tbl_df|tbl|data.frame", classes))
})

### Filter names ####
data_frame_names <- initial_env[data_frames]
data_frame_names <- data_frame_names[!grepl("meta_df|metenenmeetkunde|gedrag|interesse", data_frame_names)]

# Remove df from recommended_factors if it exists
data_frame_names <- setdiff(data_frame_names, c("recommended_factors", "local_df"))

## Create Subdirectories ####
### Create subdirectory named "item_analyses<year>" in ./data ####
subdirectory <- paste0("./data/factor_analyses", format(Sys.Date(), "%Y"))
dir.create(subdirectory)

### Create subdirectory named "fa_graphs" in ./data/item_analyses<year> ####
graph_subdirectory <- paste0(subdirectory, "/fa_graphs")
dir.create(graph_subdirectory)

## Create df to hold recommended factors ####
### Create empty data frame ####
recommended_factors <- data.frame(
  name = character(length(data_frame_names)),
  scree_fa = numeric(length(data_frame_names)),
  scree_pc_eigen = numeric(length(data_frame_names)),
  parallel = numeric(length(data_frame_names))
)

### Assign values to the data frame ####
recommended_factors$name <- data_frame_names
recommended_factors$scree_fa <- NA
recommended_factors$scree_pc_eigen <- NA
recommended_factors$parallel <- NA





# Data Analyses - Main ####
## Factor Analyses ####
for (df in data_frame_names) {
  # Print df name
  print(df)

  # Append name to recommended_factors
  recommended_factors$name[which(data_frame_names == df)] <- df

  # Create list to save factor analyses
  factor_analyses <- list()

  # Get df locally
  local_df <- get(df)

  # Filter df
  local_df <- local_df[
    , !grepl("package_duration_raw|student_number|student_name|birth_date|gender", colnames(local_df))
  ]
  # Remove items with extreme missingness.
  local_df <- local_df[
    , !grepl("ASL_componenten_21|ASL_componenten_11", colnames(local_df))
  ]

  # Remove empty variables
  local_df <- local_df[, colSums(is.na(local_df)) != nrow(local_df)]

  # Remove variables with exactly 0 standard deviation
  local_df <- local_df[, apply(local_df, 2, sd, na.rm = TRUE) != 0]

  # Replace "Invalid Number" with NA
  local_df <- replace(local_df, local_df == "Invalid Number", NA)

  # Create lavaan model for 1-factor CFA
  model <- create_cfa_model(local_df)

  # Save factor analyses to list
  # Orthogonal rotation: factors assumed to be uncorrelated
  factor_analyses[[paste0(df, "_obli")]] <- cfa(model, data = local_df, missing = "FIML", rotation = "varimax", std.lv = TRUE)

  # Oblique rotation:factors assumed to be correlated
  factor_analyses[[paste0(df, "_orth")]] <- cfa(model, data = local_df, missing = "FIML", rotation = "promax", std.lv = TRUE)


  # Data Exportation ####
  # Create and save plots as pngs
  # Simple Scree plot
  png(file = paste0(graph_subdirectory, "/", df, "_scree_plot.png"))
  scree_plot <- scree(local_df, pc = TRUE)
  recommended_factors$scree_fa[which(data_frame_names == df)] <- sum(scree_plot$fv >= 1)
  recommended_factors$scree_pc_eigen[which(data_frame_names == df)] <- sum(scree_plot$pcv >= 1)
  factor_analyses[[paste0(df, "_ev")]] <- scree_plot$pcv
  dev.off()

  # Scree plot parallel analysis
  png(file = paste0(graph_subdirectory, "/", df, "_parallel_plot.png"))
  scree_parallel_plot <- fa.parallel(local_df, fa = "fa")
  recommended_factors$parallel[which(data_frame_names == df)] <- scree_parallel_plot$nfact
  dev.off()

  # Save factor analyses results as Rdata
  save(
    factor_analyses,
    file = paste0(subdirectory, "/", df, ".Rdata")
  )

  # Remove df from global environment
  # rm(scree_plot, scree_parallel_plot)
}


# create subdirectory for alternative factor analyses
alt_fac_name <- paste0("./data/factor_analyses", format(Sys.Date(), "%Y"), "/alternative_factors")
dir.create(alt_fac_name)

# get lowest recommended factors
recommended_factors$lowest <- apply(recommended_factors[, -1], 1, min)
write.csv(recommended_factors, paste0("./data/factor_analyses", format(Sys.Date(), "%Y"), "/recommended_factors.csv"))


# Create multi-factor lavaan models.
for (factor in recommended_factors$name[recommended_factors$lowest > 1]) {
  print(factor)
  print(recommended_factors$lowest[recommended_factors$name == factor])

  # Get df locally
  local_df <- get(factor)

  # Empty model
  model <- ""
  for (f in 1:recommended_factors$lowest[recommended_factors$name == factor]) {
    # Create the lavaan model syntax with two new lines
    int_model <- create_cfa_model(local_df)
    substr(int_model, 2, 2) <- as.character(f)
    int_model <- paste0(int_model, "\n\n")

    model <- paste0(model, int_model)
  }
  assign(paste0(factor, "_model"), model, envir = .GlobalEnv)
  rm(factor, f, int_model, model)
}


# run factor analyses with lowest recommended factors
for (df in recommended_factors$name[recommended_factors$lowest != 1]) {
  # Print df name
  print(df)

  # Create list to save factor analyses
  factor_analyses <- list()

  # Get df locally
  local_df <- get(df)

  # Filter df
  local_df <- local_df[
    ,
    !grepl("package_duration_raw|student_number|student_name|birth_date|gender", colnames(local_df))
  ]

  # Remove empty variables
  local_df <- local_df[, colSums(is.na(local_df)) != nrow(local_df)]

  # Remove variables with exactly 0 standard deviation
  local_df <- local_df[, apply(local_df, 2, sd, na.rm = TRUE) != 0]

  # Create lavaan model for 3-factor CFA
  model <- get(paste0(df, "_model"))

  # Save factor analyses to list
  # Orthogonal rotation: factors assumed to be uncorrelated
  factor_analyses[[paste0(df, "_obli")]] <- cfa(model, data = local_df, missing = "FIML", rotation = "varimax", std.lv = TRUE)

  # Oblique rotation: factors assumed to be correlated
  factor_analyses[[paste0(df, "_orth")]] <- cfa(model, data = local_df, missing = "FIML", rotation = "promax", std.lv = TRUE)

  # Data Exportation ####
  # Save factor analyses results as Rdata
  save(
    factor_analyses,
    file = paste0(alt_fac_name, "/", df, "_", recommended_factors$lowest[recommended_factors$name == df], ".Rdata")
  )
}

# Script Clean-up ####
rm(list = ls())


# File Report ####
' pkg_loader("lavaan")


# Define your model
model <- "
  f1 =~ ASL_betekenissen_001 + ASL_betekenissen_004 + ASL_betekenissen_005 +
      ASL_betekenissen_003 + ASL_betekenissen_007 +
      ASL_betekenissen_006 + ASL_betekenissen_002 + ASL_betekenissen_012 +
      ASL_betekenissen_008 + ASL_betekenissen_010 + ASL_betekenissen_011 +
      ASL_betekenissen_014 + ASL_betekenissen_009 + ASL_betekenissen_013 +
      ASL_betekenissen_015 + ASL_betekenissen_016
      "






# Fit the model with missing data using full information maximum likelihood (FIML)
fit <- cfa(model, data = betekenissen, missing = "FIML")

summary(fit, standardized = TRUE, fit.measures = TRUE)

factor_loadings <- lavInspect(fit, "std")$lambda
factor_loadings <- factor_loadings[order(factor_loadings,d), ]
factor_loadings

'
