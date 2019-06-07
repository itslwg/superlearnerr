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
                  "ggpubr")
    ## Require those packages using a loop
    for (p in packages) require(p, character.only = TRUE)
    ## Save list of packages to parent environment
    .package_list <<- packages
}
