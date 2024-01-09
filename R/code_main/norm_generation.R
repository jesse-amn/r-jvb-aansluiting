# File Documentation ####
"
Author description:

  J.F.J. (Jesse) van Bussel, methodologist/psychometrician at AMN

Project description:

  Talentscan norm-generation and application.

File description:

  This is the main/norm_generation master script, which, if everything works, should automatically
  chain all underlying scripts.
"




# Evironment set-up ####
"-------------------------------------------------------------------------------"
path <- "/Export Results/Export Aansluiting Group 7 - 21.12.2023"
"-------------------------------------------------------------------------------"


# Load Data ####
source("./R/code_main/norm_generation/1_load_data.R")
load_data(path)

# Prep Data ####
source("./R/code_main/norm_generation/2_prep_data.R")
prep_data(item_analyses = FALSE)

# Calc final capacity ####
source("./R/code_main/norm_generation/3_calc_cap_final.R")
calc_cap_final()

# Calc capacity norms ####
source("./R/code_main/norm_generation/4_cap_norms.R")
cap_norms()

# Calc final personality ####
