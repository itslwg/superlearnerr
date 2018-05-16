#' Function to generate bootstrap samples
#'
#' This function generates bootstrap samples from the study_data.
#' @param study_data The study data as a data frame. No default
#' @param bs_samples Integer value describing the number of bootstrap samples to be generated. Specified as argument in main.study()
#' @export
generate.bootstrap.samples <- function(
                                       study_data,
                                       bs_samples
                                       )
{
    #Generate bootstrap samples
    bootstrap_samples <- lapply(1:bs_samples,
                                function(i) study_data[sample(1:nrow(study_data),
                                                              replace = TRUE),]
                                )

    return (bootstrap_samples)
}
