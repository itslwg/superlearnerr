#' Function to train and predict SuperLearner on each sample
#'
#' This trains SuperLearner on every sample training set and reviews SuperLearner on every sample review set.
#' @param samples Bootstrap samples as list of data frames. No default.
#' @export
train.predict.bssamples <- function(
                                    samples
                                    )
{
    ## Train and predict on each bootstrap sample dataframe
    predictions <- lapply(samples,
                          function (df) predictions.with.superlearner(df))

    return(predictions)
}
