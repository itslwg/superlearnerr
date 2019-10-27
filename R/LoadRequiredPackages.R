#' LoadRequiredPackages
#'
#' Imports the required packages for the project.
LoadRequiredPackages <- function() {
    ## Vector of names of required packages
    packages <- c("methods",
                  "SuperLearner",
                  "data.table",
                  "nricens",
                  "dplyr",
                  "boot",
                  "knitr",
                  "tableone",
                  "xtable",
                  "lattice",
                  "RColorBrewer",
                  "glmnet",
                  "randomForest",
                  "xgboost",
                  "gam",
                  "ggplot2",
                  "foreach",
                  "doParallel",
                  "extrafont",
                  "gridExtra",
                  "ggpubr",
                  "gridExtra",
                  "caret",
                  "bengaltiger",
                  "boot",
                  "ROCR",
                  "kableExtra",
                  "rmarkdown")
    ## Require those packages using a loop
    for (package in packages) require(package, character.only = TRUE)
    ## Save list of packages to parent environment
    .package_list <<- packages
}

