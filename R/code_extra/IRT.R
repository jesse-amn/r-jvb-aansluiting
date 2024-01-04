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
options(scipen = 999, repos = c(CRAN = "https://cloud.r-project.org/"))

## Get helpers ####
if (any(grepl("/", list.files(recursive = TRUE, pattern = "amn_helpers.R")))) {
  source(here(list.files(recursive = TRUE, pattern = "amn_helpers.R")))
} else {
  source(list.files(recursive = TRUE, pattern = "amn_helpers.R"))
}

## Load necessary packages: Fill into c() with comma-seperated quotation marks ####
packages <- c("readr", "mirt", "dplyr", "ggplot2", "randomcoloR")
pkg_loader(packages)
rm(packages)

## Load devtools package ####
if (!requireNamespace("ggmirt", quietly = TRUE)) {
  devtools::install_github("masurp/ggmirt")
}
library(ggmirt)

## Change directory: if not .Rproj, it will set working directory to script filepath ####
wd_set_current()

## Set seed ####
set.seed(42)


# Data Importation ####
## Get data
for (df in list.files("./data/data_interrim", pattern = ".csv", full.names = TRUE)) {
  assign(
    gsub("./data/data_interrim/", "", gsub(".csv", "", df)),
    read_csv(df)
  )
  rm(df)
}
initial_env <- ls(envir = .GlobalEnv)

### Get the names of the data frames ####
data_frames <- sapply(initial_env, function(x) {
  classes <- class(get(x, envir = .GlobalEnv))
  any(grepl("spec_tbl_df|tbl_df|tbl|data.frame", classes))
})

### Filter names ####
data_frame_names <- initial_env[data_frames]
data_frame_names <- data_frame_names[!grepl("meta_df|metenenmeetkunde|gedrag_houding|interesse", data_frame_names)]
data_frame_names <- data_frame_names[data_frame_names != "local_df" & data_frame_names != "recommended_factors"]


## Create Subdirectories ####
### Create subdirectory named "item_analyses<year>" in ./data ####
subdirectory <- paste0("./data/irt_analyses", format(Sys.Date(), "%Y"))
dir.create(subdirectory)

### Create subdirectory named "fa_graphs" in ./data/factor_analyses_simple<year> ####
graph_subdirectory <- paste0(subdirectory, "/irt_graphs")
dir.create(graph_subdirectory)


# Data Analyses - Main ####
irt_fits <- list() # Create an empty list to store the irt_fit objects

## Loop through the data frames ####
for (df in data_frame_names) {
  print(df)
  ### Get and filter data ####
  local_df <- get(df)
  local_df <- local_df[, !grepl("package_duration_raw|student_number|student_name|birth_date|gender", colnames(local_df))]

  ### Create irt_fit object ####
  irt_fit <- mirt(
    data = local_df,
    model = 1, # Number of factors
    method = "EM", # can be changed: MCEM (monte-carlo EM)
    itemtype = "2PL", # can be changed: RASCH, 3PL
    verbose = TRUE,
    emcycles = 500, # Increase the maximum number of EM cycles to N
  )

  ### Item fit Plot ####
  # For whatever reason, this plot needs to be made directly with the object (i.e., the object cannot be stored prior to making the plot)
  png(file = paste0(normalizePath(graph_subdirectory), "/", df, "_item_fit.png"), width = 1200, height = 800)
  plot <- itemfitPlot(irt_fit)
  print(plot)
  dev.off()

  ### Store irt_fit object ####
  irt_fits[[df]] <- irt_fit # Store the irt_fit object in the list
  saveRDS(irt_fit, file = paste0("./data/irt_analyses", format(Sys.Date(), "%Y"), "/irt_fit_", df, ".rds"))

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
  plot <- tracePlot(fit)
  print(plot)
  dev.off()

  ### Item charactfristics curves, combined ####
  # Increase resolution of graphs
  png(file = paste0(normalizePath(graph_subdirectory), "/", name, "_ICC_combined.png"), width = 1200, height = 800)
  plot <- tracePlot(fit, facet = FALSE, legend = TRUE)
  plot <- plot + scale_color_manual(values = randomColor(length(params$items))) # Use random colors for each item
  print(plot)
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

  ### Item-Person fit ####
  png(file = paste0(normalizePath(graph_subdirectory), "/", name, "_item_person_fit.png"), width = 1200, height = 800)
  plot <- itempersonMap(fit)
  print(plot)
  dev.off()

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

# File Report ####
"
# How to get data:

summary(irt_fit)

params <- coef(fit, IRTpars = TRUE, simplify = TRUE)
round(params$items, 2) # g = c = guessing parameter

# Model fit
M2(irt_fit)

# item fit
itemfit(irt_fit)


# person fit
head(personfit(irt_fit))

personfit(irt_fit) %>%
  reframe(
    infit.outside = prop.table(table(z.infit > 1.96 | z.infit < -1.96)),
    outfit.outside = prop.table(table(z.outfit > 1.96 | z.outfit < -1.96))
  ) # lower row = non-fitting people


# How to get seperate ICC graphs for a few items (from the fit):

tracePlot(fit, items = c(1:5), facet = FALSE, legend = TRUE) + f
scale_color_manual(values = randomColor(length(params$items))) # Use random colors for each item

"
