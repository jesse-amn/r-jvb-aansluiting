# Project Template and Structure

#### Author description:

  J.F.J. (Jesse) van Bussel, methodologist/psychometrician at AMN.

#### Project description:

  Talentscan norm-generation and application.

#### File description:

  This file explains the general and specific structure of this project.

## General Project Structure

The projects within AMN will follow a similar template, so that
readability and accessibility is maintained. Following will be the
template set-up, whereafter a short explanation will be given regarding
the specific set-up of the current project.

### Directories

- Project Directory: Name will differ
  - .Rproj.user (hidden): this folder contains user-specific project settings, 
    caches, and temporary files related to your RStudio session for that particular project
  - .vscode (hidden): contains project specific VSCode settings.
  - code: directory where code is stored that is not related to data-processing or
    analyses, or otherwise is not used within the R folder
      - init.R: R-script that intializes certain functionalities within the project
        folder, as well as other update functions; required for automatic testing,
        package export, and function description; should be run at the start of
        every project, and it should be ensured that the project parent directory
        has an appropriate name
  - data: directory for (temporary) data storage
    - data_input: the raw data that is received for the project
    - data_interrim: manipulated and transformed data, will contain data
      for main analyses
    - data_output: final data in .csv form of the analyses
    - Subdirectories may exist here depending on project requirements
  - docs:
    - docs_explain: documents related to the project, but not derivative of
      main scripts, necessary for current iterative process, nor downloaded
      from our database as prerequisite for the main analyses
    - docs_output: explanation, evaluations, and justification documents
      for the analyses, meant for export otherwise mean for external purposes
  - man: contains function descriptions of the scripts located in the ./R directory,
    which should be automatically generated with roxygen2
  - R: Holds R code
    - code_extra: directory for the code that is not related to the final
      product
      - support: help functions and sub-help functions that are imported
        into main scripts
      - Subdirectories may exist here depending on project requirements
    - code_main: directory for the code that is related to the final
      product
      - support: help functions and sub-help functions that are imported
        into main scripts
      - Subdirectories may exist here, depending on project requirements
      - Function scripts located in this folder (non-recursively) can be called
        by loading the project as a package.
  - tests: functional testing directory, initialized via testthat
    - test_data: holds required data for unit/integration tests
    - testthat: holds the R-scripts which contain the automatic tests
    - testthat.R: This file is part of the standard setup for testthat

### Files

- .gitignore: contains instructions for git for ignoring files on push
- .lintr: contains auto-format and linting specifications
- .Rbuildignore: package ignore instructions, and temporary files for package generation
- .Rhistory (hidden): contains terminal history
- DESCRIPTION: contains package main information page (?<package_name>)
- NAMESPACE: specifies the export and import behavior of functions and objects in package
- README.MD: current document, used for explaining template
- <project_name>.Rproj (may be invisible, and should be ignored)
  

### Notes

1.  all (sub)directories will contain a name.Rmd for visibility, and/or
  further explanation of the contents of the directory. These files may be empty, 
  and may be deleted if they are not needed. Similarly, the (sub)directories may
  be deleted if they are not needed. 
2. after creating project folder, a .Rproj. should be generated.
3. the newest version amn_helpers.R should be added to /code_main/support.
  Only one file called amn_helpers.R should be present in the project.
4. the ./code/init.R file should be run at the beginning of every project, and 
   it should be ensured that the project framework (as partially described above)
   is instantiated prior to starting. 


<to be filled>
