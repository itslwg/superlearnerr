#' Function to train and predict SuperLearner on each sample
#'
#' This trains SuperLearner on every sample training set and reviews SuperLearner on every sample review set.
#' @param samples Bootstrap samples as prepared by prep.bssamples. No default.
#' @param parallel Logical. If TRUE the training will be run in parallel. Defaults to FALSE.
#' @param n_cores Integer. The number of cores to use for parallel computing. Defaults to NULL.
#' @export
train.predict.bssamples <- function(
                                    samples,
                                    parallel = FALSE,
                                    n_cores = NULL
                                    )
{
    ## Error handling
    if (!is.list(samples)) stop("Samples argument is not a list")
    ## Train and predict on each bootstrap sample dataframe
    if (parallel) {
        if (is.null(n_cores)) {
            n_cores <- 2
            message("You did not specify the number of cores so 2 will be used")
        }
        cl <- makeCluster(n_cores)
        registerDoParallel(cl)
        message("Running SuperLearner on bootstrap samples in parallel on ", n_cores, " cores")
        predictions <- foreach(sample = samples, .packages = .package_list, .export = c("predictions.with.superlearner", "gridsearch.breaks")) %dopar% predictions.with.superlearner(sample)
        stopCluster(cl)
    }
    else predictions <- lapply(samples, predictions.with.superlearner)
    return(predictions)
}
