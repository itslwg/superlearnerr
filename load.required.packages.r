#' Load required packages function
#' @export
load.required.packages <- function()
{
    ## Vector of names of required packages
    packages <- c("SuperLearner",
                  "nricens",
                  "dplyr",
                  "boot",
                  "knitr")
    ## Require those packages using a loop
    for (p in packages) require(p, character.only = TRUE)
}
