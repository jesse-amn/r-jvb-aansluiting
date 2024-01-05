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
packages <- c("readr", "mirt", "dplyr", "ggplot2", "randomcoloR", "lavaan")
pkg_loader(packages)
rm(packages)

## Load devtools package ####
if (!requireNamespace("ggmirt", quietly = TRUE)) {
  devtools::install_github("masurp/ggmirt")
}
library(ggmirt)

## Change directory: if not .Rproj, it will set working directory to script filepath ####
wd_set_current()

## Create script-specific functions
### Function to generate CFA model specification for multiple factors ####
generate_cfa_model <- function(factors) {
  model <- ""

  for (factor in names(factors)) {
    # Start the model string with the factor name
    factor_model <- paste0(factor, " =~ ")

    # Add each item to the model string
    for (item in factors[[factor]]) {
      factor_model <- paste0(factor_model, item, " + ")
    }

    # Remove the last " + " from the model string
    factor_model <- substr(factor_model, 1, nchar(factor_model) - 3)

    # Add the factor model to the overall model
    # Add a space at the end to prevent concatenation with the next factor name
    model <- paste0(model, factor_model, "\n ")
  }

  return(model) # trimws() is used to remove the trailing white space
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
data_frame_names <- data_frame_names[grepl("gedrag_houding", data_frame_names)]

# Remove df from recommended_factors if it exists
data_frame_names <- setdiff(data_frame_names, c("recommended_factors", "local_df"))

## Create Subdirectories ####
### Create subdirectory named "item_analyses<year>" in ./data ####
subdirectory <- paste0("./data/item_analyses_pers", format(Sys.Date(), "%Y"))
dir.create(subdirectory)

### Create subdirectory named "fa_graphs" in ./data/item_analyses<year> ####
graph_subdirectory <- paste0(subdirectory, "/graphs")
dir.create(graph_subdirectory)


# Create list to save factor analyses
factor_analyses <- list()
irt_fits <- list() # Create an empty list to store the irt_fit objects

# Data Analyses - Main ####
## Factor Analyses ####
for (df in data_frame_names) {
  # Print df name
  print(df)

  # Append name to recommended_factors
  recommended_factors$name[which(data_frame_names == df)] <- df

  # Get df locally
  local_df <- get(df)

  # Filter df
  local_df <- local_df[
    , !grepl("package_duration_raw|student_number|student_name|birth_date|gender", colnames(local_df))
  ]

  # Remove empty variables
  local_df <- local_df[, colSums(is.na(local_df)) != nrow(local_df)]

  # Remove variables with exactly 0 standard deviation
  local_df <- local_df[, apply(local_df, 2, sd, na.rm = TRUE) != 0]

  # Replace "Invalid Number" with NA
  local_df <- replace(local_df, local_df == "Invalid Number", NA)

  # Create lavaan model
  model <- generate_cfa_model(list(
    betrokkenheid_thuis = colnames(local_df[grepl("betrokkenheid_thuis", colnames(local_df))]),
    concentratie = colnames(local_df[grepl("concentratie", colnames(local_df))]),
    contact_met_leeftijdsgenoten = colnames(local_df[grepl("contact_met_leeftijdsgenoten", colnames(local_df))]),
    interesse_in_schoolvakken = colnames(local_df[grepl("interesse_in_school", colnames(local_df))]),
    motivatie_om_te_presteren = colnames(local_df[grepl("motivatie_om_te_presteren", colnames(local_df))]),
    samenwerken = colnames(local_df[grepl("samenwerken", colnames(local_df))]),
    zelfvertrouwen = colnames(local_df[grepl("zelfvertrouwen", colnames(local_df))]),
    stabiel = colnames(local_df[grepl("stabiel", colnames(local_df))]),
    zorgvuldig = colnames(local_df[grepl("zorgvuldig", colnames(local_df))]),
    open = colnames(local_df[grepl("open", colnames(local_df))])
  ))

  # Save factor analyses to list
  print("CFA")
  factor_analyses[[df]] <- cfa(model, data = local_df, std.lv = TRUE)

  # Data Exportation ####
  save(
    factor_analyses,
    file = paste0(subdirectory, "/", df, ".Rdata")
  )

  vars <- c(
    "betrokkenheid_thuis",
    "concentratie",
    "contact_met_leeftijdsgenoten",
    "interesse_in_school",
    "motivatie_om_te_presteren",
    "samenwerken",
    "zelfvertrouwen",
    "stabiel",
    "zorgvuldig",
    "open"
  )

  for (var in vars) {
    data <- local_df[, grepl(var, colnames(local_df))]

    # IRT
    # Create irt_fit object
    print("IRT")
    irt_fit <- mirt(
      data = data,
      model = 1, # Number of factors
      method = "EM", # can be changed: MCEM (monte-carlo EM)
      itemtype = "graded", # can be changed: RASCH, 3PL
      verbose = TRUE,
      emcycles = 500, # Increase the maximum number of EM cycles to N
      technical = list(NCYCLES = 500)
    )

    ### Item fit Plot ####
    # # For whatever reason, this plot needs to be made directly with the object (i.e., the object cannot be stored prior to making the plot)
    # png(file = paste0(normalizePath(graph_subdirectory), "/", df, "_", var, "_item_fit.png"), width = 1200, height = 800)
    # plot <- itemfitPlot(irt_fit)
    # print(plot)
    # dev.off()

    ### Store irt_fit object ####
    irt_fits[[paste0(df, "_", var, "_irt_fit")]] <- irt_fit # Store the irt_fit object in the list
  }
  saveRDS(irt_fits, file = paste0("./data/item_analyses_pers", format(Sys.Date(), "%Y"), "/irt_fit_", df, ".rds"))
  ### Remove local objects ####
  rm(local_df)
}

# Data Analyses - Explication ####
for (i in seq_along(irt_fits)) {
  ## Get the fit object ####
  fit <- irt_fits[[i]]

  ## Get the name of the fit ####
  name <- names(irt_fits)[i]

  ## Get the parameters ####
  params <- coef(fit, IRTpars = TRUE, simplify = TRUE)

  ## Create and save Graphs ####
  ### Item characteristics curves, separate ####
  png(file = paste0(normalizePath(graph_subdirectory), "/", name, "_ICC_separate.png"), width = 1200, height = 800)
  plot_separate <- tracePlot(fit, facet = TRUE)
  print(plot_separate)
  dev.off()

  ### Item characteristics curves, combined ####
  # Increase resolution of graphs
  png(file = paste0(normalizePath(graph_subdirectory), "/", name, "_ICC_combined.png"), width = 1200, height = 800)
  plot_combined <- tracePlot(fit, facet = FALSE, legend = TRUE)
  plot_combined <- plot_combined + scale_color_manual(values = randomColor(length(params$items))) # Use random colors for each item
  print(plot_combined)
  dev.off()

  ### Item info curve, separate ####
  png(file = paste0(normalizePath(graph_subdirectory), "/", name, "_IIC_separate.png"), width = 1200, height = 800)
  plot <- itemInfoPlot(fit, facet = TRUE)
  print(plot)
  dev.off()

  ### Item info curve, combined ####
  png(file = paste0(normalizePath(graph_subdirectory), "/", name, "_IIC_combined.png"), width = 1200, height = 800)
  plot <- itemInfoPlot(fit, legend = TRUE)
  plot <- plot + scale_color_manual(values = randomColor(length(params$items))) # Use random colors for each item
  print(plot)
  dev.off()

  # ### Item-Person fit ####
  # png(file = paste0(normalizePath(graph_subdirectory), "/", name, "_item_person_fit.png"), width = 1200, height = 800)
  # plot <- itempersonMap(fit)
  # print(plot)
  # dev.off()

  ### Test information curve ####
  png(file = paste0(normalizePath(graph_subdirectory), "/", name, "_TIC.png"), width = 1200, height = 800)
  plot <- testInfoPlot(fit, adj_factor = 2)
  print(plot)
  dev.off()

  ### Test characteristic curve ####
  png(file = paste0(normalizePath(graph_subdirectory), "/", name, "_TCC.png"), width = 1200, height = 800)
  plot <- scaleCharPlot(fit)
  print(plot)
  dev.off()
}









data_frame_names <- initial_env[data_frames]
data_frame_names <- data_frame_names[grepl("interesse", data_frame_names)]


# Create list to save factor analyses
factor_analyses <- list()
irt_fits <- list() # Create an empty list to store the irt_fit objects

# Data Analyses - Main ####
## Factor Analyses ####
for (df in data_frame_names) {
  # Print df name
  print(df)

  # Append name to recommended_factors
  recommended_factors$name[which(data_frame_names == df)] <- df

  # Get df locally
  local_df <- get(df)

  # Filter df
  local_df <- local_df[
    , !grepl("package_duration_raw|student_number|student_name|birth_date|gender|_2$", colnames(local_df))
  ]

  # Remove empty variables
  local_df <- local_df[, colSums(is.na(local_df)) != nrow(local_df)]

  # Remove variables with exactly 0 standard deviation
  local_df <- local_df[, apply(local_df, 2, sd, na.rm = TRUE) != 0]

  # Replace "Invalid Number" with NA
  local_df <- replace(local_df, local_df == "Invalid Number", NA)

  # Create lavaan model
  model <- generate_cfa_model(list(
    betrokkenheid_thuis = colnames(local_df[grepl("realistisch", colnames(local_df))]),
    concentratie = colnames(local_df[grepl("conventioneel", colnames(local_df))]),
    contact_met_leeftijdsgenoten = colnames(local_df[grepl("ondernemend", colnames(local_df))]),
    interesse_in_schoolvakken = colnames(local_df[grepl("intellectueel", colnames(local_df))]),
    motivatie_om_te_presteren = colnames(local_df[grepl("artistiek", colnames(local_df))]),
    samenwerken = colnames(local_df[grepl("sociaal", colnames(local_df))])
  ))

  # Save factor analyses to list
  print("CFA")
  factor_analyses[[df]] <- cfa(model, data = local_df, std.lv = TRUE)

  # Data Exportation ####
  save(
    factor_analyses,
    file = paste0(subdirectory, "/", df, ".Rdata")
  )
  colnames(local_df)
  vars <- c(
    "realistisch",
    "conventioneel",
    "ondernemend",
    "intellectueel",
    "artistiek",
    "sociaal"
  )

  for (var in vars) {
    data <- local_df[, grepl(var, colnames(local_df))]

    # IRT
    # Create irt_fit object
    print("IRT")
    irt_fit <- mirt(
      data = data,
      model = 1, # Number of factors
      method = "EM", # can be changed: MCEM (monte-carlo EM)
      itemtype = "graded", # can be changed: RASCH, 3PL
      verbose = TRUE,
      emcycles = 500, # Increase the maximum number of EM cycles to N
      technical = list(NCYCLES = 500)
    )

    ### Item fit Plot ####
    # # For whatever reason, this plot needs to be made directly with the object (i.e., the object cannot be stored prior to making the plot)
    # png(file = paste0(normalizePath(graph_subdirectory), "/", df, "_", var, "_item_fit.png"), width = 1200, height = 800)
    # plot <- itemfitPlot(irt_fit)
    # print(plot)
    # dev.off()

    ### Store irt_fit object ####
    irt_fits[[paste0(df, "_", var, "_irt_fit")]] <- irt_fit # Store the irt_fit object in the list
  }
  saveRDS(irt_fit, file = paste0("./data/item_analyses_pers", format(Sys.Date(), "%Y"), "/irt_fit_", df, ".rds"))
  ### Remove local objects ####
  rm(local_df)
}

# Data Analyses - Explication ####
for (i in seq_along(irt_fits)) {
  ## Get the fit object ####
  fit <- irt_fits[[i]]

  ## Get the name of the fit ####
  name <- names(irt_fits)[i]

  ## Get the parameters ####
  params <- coef(fit, IRTpars = TRUE, simplify = TRUE)

  ## Create and save Graphs ####
  ### Item characteristics curves, separate ####
  png(file = paste0(normalizePath(graph_subdirectory), "/", name, "_ICC_separate.png"), width = 1200, height = 800)
  plot_separate <- tracePlot(fit, facet = TRUE) # Set facet to FALSE to plot separately
  print(plot_separate)
  dev.off()

  ### Item characteristics curves, combined ####
  # Increase resolution of graphs
  png(file = paste0(normalizePath(graph_subdirectory), "/", name, "_ICC_combined.png"), width = 1200, height = 800)
  plot_combined <- tracePlot(fit, facet = FALSE, legend = TRUE)
  plot_combined <- plot_combined + scale_color_manual(values = randomColor(length(params$items))) # Use random colors for each item
  print(plot_combined)
  dev.off()

  ### Item info curve, separate ####
  png(file = paste0(normalizePath(graph_subdirectory), "/", name, "_IIC_separate.png"), width = 1200, height = 800)
  plot <- itemInfoPlot(fit, facet = TRUE)
  print(plot)
  dev.off()

  ### Item info curve, combined ####
  png(file = paste0(normalizePath(graph_subdirectory), "/", name, "_IIC_combined.png"), width = 1200, height = 800)
  plot <- itemInfoPlot(fit, legend = TRUE)
  plot <- plot + scale_color_manual(values = randomColor(length(params$items))) # Use random colors for each item
  print(plot)
  dev.off()

  # ### Item-Person fit ####
  # png(file = paste0(normalizePath(graph_subdirectory), "/", name, "_item_person_fit.png"), width = 1200, height = 800)
  # plot <- itempersonMap(fit)
  # print(plot)
  # dev.off()

  ### Test information curve ####
  png(file = paste0(normalizePath(graph_subdirectory), "/", name, "_TIC.png"), width = 1200, height = 800)
  plot <- testInfoPlot(fit, adj_factor = 2)
  print(plot)
  dev.off()

  ### Test characteristic curve ####
  png(file = paste0(normalizePath(graph_subdirectory), "/", name, "_TCC.png"), width = 1200, height = 800)
  plot <- scaleCharPlot(fit)
  print(plot)
  dev.off()
}

# Data Finalization ####

# Data Exportation ####


# Script Clean-up ####
rm(list = ls())
