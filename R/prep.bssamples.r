#' Function to prepare data on bootstrap samples
#'
#' This function prepares every bootstrap sample for SuperLearner computations.
#' @param samples Bootstrap samples as list of data frames. No default.
#' @export
prep.bssamples <- function(
                           samples
                           )
{
    ##Prepare every bootstrap sample
    prepped_samples <- lapply(samples,
                              function(df) to.dummy.variables(prep.data.for.superlearner(df, test = TRUE)))
    return (prepped_samples)
}
