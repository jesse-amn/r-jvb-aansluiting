# Import the devtools package ####
library(devtools)

# Create temp file for building package ####
templib <- tempfile()
dir.create(templib)
.libPaths(templib)

# Install package ####
install.packages(getwd(),
  lib = templib, type = "source", repos = NULL
)

# Load package ####
library(aansluiting)

## Test function ####
hello()

# Document roxygen2 changes in ./man based on ./R ####
devtools::document()
