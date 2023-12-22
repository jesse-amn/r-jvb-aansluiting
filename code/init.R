"
Ensure that the parent directory (which will be the root of the project/repo) is
named appropriately. This is the directory that will be used to create the
package. The package will be created in the parent directory of the current
working directory. This name can only contain letters and numbers. It cannot
contain spaces or special characters. It is recommended that the name be
descriptive of the project/repo.

Note: the parent directory name will be used as the package name. This means that,
initially, the directory name should be the same as the package name. However, this
also means that the package name must contain no special characters or spaces at
initialization. The package name can be changed later, but this will require
additional steps.

After initialization, DESCRIPTION information can be changed, and the
directory name can be changed. This will not affect the package loading name,
which can be found in ./DESCRIPTION -> package -> name.
"
# Environment Set-up ####
library(here)
options(scipen = 999)

## Get helpers ####
### Create function to load packages ####
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
            cat(
              "\n\033[31mFollowing package(s) raised an error:",
              pkg, "\033[97m\n"
            )
          }
        )
      } else {
        loaded_packages <- c(loaded_packages, pkg)
      }
    }
  )

  if (length(loaded_packages) > 0) {
    cat(
      "\nFollowing package(s) were loaded:",
      paste(loaded_packages, collapse = ", "), "\n"
    )
  }
  options(warn = 0)
}

### Load packages ####
pkg_loader(c("devtools", "testthat", "roxygen2", "usethis"))

## Set working directory ####
setwd(dirname(dirname(this.path::this.path())))


# Initate Project ####
## Create package environment ####
devtools::create(getwd())
usethis::create_package(getwd())


## Create testing environment ####
usethis::use_testthat()
usethis::use_test("template")
testthat::test_env()

## Create hello.R script ####
system2("sh", args = "./code/init_test_function.sh")

## Create documentation environment ####
devtools::document()


## Create reproducibly environment ####
# renv::init()
# devtools::document()

## Set-up directory structure ####
## move code_extra and code_main into ./R ####
file.rename(
  here("code_main"),
  paste0(dirname(here("code_main")), "/R/", basename(here("code_main")))
)
file.rename(
  here("code_extra"),
  paste0(dirname(here("code_extra")), "/R/", basename(here("code_extra")))
)

## Append .gitignore file ####
gitignore_content <- "
# History files
.Rhistory
.Rapp.history

# Session Data files
.RData
.RDataTmp

# User-specific files
.Ruserdata

# Example code in package build process
*-Ex.R

# Output files from R CMD build
/*.tar.gz

# Output files from R CMD check
/*.Rcheck/

# RStudio files
.Rproj.user/

# produced vignettes
vignettes/*.html
vignettes/*.pdf

# R-related
.DS_Store

# Specific paths
data/data_input/*.csv
data/data_interrim/*.csv
data/data_output/*.csv
data/**/*.csv
"
cat(gitignore_content, file = ".gitignore", append = FALSE)


## Create .lintr file ####
lintr_content <- "linters: linters_with_defaults(
\tcommented_code_linter = NULL,
\tobject_usage_linter = NULL,
\tline_length_linter = NULL
\t)
"
cat(lintr_content, file = ".lintr", append = FALSE)
