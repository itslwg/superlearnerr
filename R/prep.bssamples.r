#' Function to prepare data on bootstrap samples
#'
#' This function prepares every bootstrap sample for SuperLearner computations with prep.data.for.superlearner.
#' @param samples Bootstrap samples as list of data frames. No default.
#' @export
prep.bssamples <- function(
                           samples
                           )
{
    ## Error handling
    if (!is.list(samples)) stop("Samples argument is not a list.")
    for (l in samples){
        ## Check if samples are data frames.
        if(!is.data.frame(l)) stop("Samples are not data frames.")
    }
    ##Prepare every bootstrap sample
    prepped_samples <- lapply(samples,
                              function(df) to.dummy.variables(prep.data.for.superlearner(df)))

    return (prepped_samples)
}
