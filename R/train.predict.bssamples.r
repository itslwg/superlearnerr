#' Function to train and predict SuperLearner on each sample
#'
#' This trains SuperLearner on every sample training set and reviews SuperLearner on every sample review set.
#' @param samples Bootstrap samples as prepared by prep.bssamples. No default.
#' @export
train.predict.bssamples <- function(
                                    samples
                                    )
{
    ## Error handling
    if (!is.list(samples)) stop("Samples argument is not a list.")
    ## Train and predict on each bootstrap sample dataframe
    predictions <- lapply(samples,
                          function (df) predictions.with.superlearner(df))

    return(predictions)
}
