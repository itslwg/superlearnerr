#' Partition Sample
#'
#' Partitions the sample using the caret package, i.e. splits the dataset in train and test sets.
#' @param study.sample Data frame. The study sample. No default
#' @param outcome.variable.name Character vector of length 1. The name of the outcome variable of interest. Defaults to "s30d"
#' @param ... Additional arguments for caret::createDataPartition.
#' @export
PartitionSample <- function(study.sample, outcome.variable.name = "s30d",
                            ...) {
    ## Error handling
    if (!is.character(outcome.variable.name) | !bengaltiger::IsLength1(outcome.variable.name))
        stop("outcome.variable.name has to be a character vector of length 1")
    ## Get the indices for the training set
    train.indices <- caret::createDataPartition(y = study.sample[, outcome.variable.name], ...)
    outcome <- study.sample[, outcome.variable.name]
    ## Change factor levels of the outcome to 0s and 1s, and make numeric
    levels(outcome) <- c(0, 1)
    outcome <- as.numeric(as.character(outcome))
    set.list <- lapply(list(train = train.indices, test = -train.indices), function(indices) {
        ## Remove clinicians triage and outcome from the features
        x <- study.sample[indices, ]
        ## Subset tc from the samples
        tc <- x[, "tc"]
        x <- x[, !(names(x) %in% c(outcome.variable.name, "tc"))]
        return (list(x = x, y = outcome[indices], tc = tc))
    })
    return (set.list)
}
