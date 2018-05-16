#' Load required packages function
#' @export
load.required.packages <- function()
{
    ## Vector of names of required packages
    packages <- c("SuperLearner",
                  "data.table",
                  "nricens",
                  "dplyr",
                  "boot",
                  "knitr",
                  "tableone",
                  "xtable")
    ## Require those packages using a loop
    for (p in packages) require(p, character.only = TRUE)
}
