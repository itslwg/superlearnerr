#' Function to generate bootstrap samples
#'
#' This function generates bootstrap samples from the study_data.
#' @param study_data The study data as data frame. No default
#' @param bs_samples Integer. The number of bootstrap samples to be generated. No default.
#' @export
generate.bootstrap.samples <- function(
                                       study_data,
                                       bs_samples
                                       )
{
    ## Error handling
    if (!is.data.frame(study_data)) stop("Study_data is not a data frame.")
    # Generate bootstrap samples
    bootstrap_samples <- lapply(1:bs_samples,
                                function(i) splitstackshape::stratified(study_data,
                                                                        "s30d",
                                                                        size = 0.9999,
                                                                        replace = TRUE))
}
