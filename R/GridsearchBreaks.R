#' Gridsearch breaks function
#'
#' Performs a gridsearch to identify breaks to optimise a provided loss function
#' @param predictions Numeric vector. Predicted probabilites. No default.
#' @param outcome.vector Numeric vecotr. The outcome of interest. No default.
#' @param grid Numeric vector. All values to be combined and searched. Defaults to seq(0.01, 0.99, 0.01).
#' @param n.breaks Numeric vector of length 1. The number of breaks in the predicted probabilites, i.e. cut-points. Defaults to 3.
#' @param loss.function Character vector of length 1. The name of the loss metric, e.g. auc. For now any performance measure included in ROCR is accepted. Defaults to "auc".
#' @param sample Logical. If TRUE loss is only calculated for 10% of possible breaks. Defaults to TRUE.
#' @param parallel Logical. If TRUE the gridsearch is done in parallel. Defaults to FALSE.
#' @param n.cores Numeric vector of length 1. The number of cores to use for parallel computing. Defaults to NULL.
#' @param save_plot Logical. If TRUE a wireframe plot is saved to disk. Defaults to FALSE.
#' @param verbose Logical. If TRUE then information is printed as the gridsearch runs. Defaults to FALSE. Currently ignored.
#' @param maximise Logical. If TRUE, grid search maximizes performance metric. Defaults to TRUE.
#' @importFrom foreach %dopar%
#' @export
GridsearchBreaks <- function(predictions, outcome.vector,
                             grid = seq(0.01, 0.99, 0.01),
                             n.breaks = 3, loss.function = "auc", sample = TRUE,
                             parallel = FALSE, n_cores = NULL, save_plot = FALSE,
                             verbose = FALSE, maximise = TRUE) {
    ## Verify that outcomes is a vector of binary data with 0 and 1
    if (!all(levels(as.factor(outcome.vector)) %in% c("0", "1"))) stop("outcome.vector is not a vector of binary data with only 0 and 1")
    ## Verify that there are no missing values in predictions or outcomes
    if (!all(!is.na(predictions)) | !all(!is.na(outcome.vector))) stop("There is missing data in predictions or outcomes")
    ## Define grid
    breaks.to.search <- combn(grid, n.breaks, simplify = FALSE)
    ## Draw a random sample if sample is TRUE
    if (sample) breaks.to.search <- breaks.to.search[sample(1:length(breaks.to.search), ceiling(length(breaks.to.search) * 0.1))]
    if (verbose)
        message("Estimating loss for each of ", length(breaks.to.search), " possible cutpoints combinations")
    ## Define function to estimate loss for each set of cut points (breaks)
    EstimateLoss <- function(breaks) {
        breaks <- c(-Inf, breaks, Inf)
        groupings <- as.numeric(cut(predictions, breaks = breaks))
        loss <- EvaluateWithRocr(predictions = groupings, outcome.vector = outcome.vector,
                                 measure = loss.function)
        return(loss)
    }
    ## Do gridsearch
    if (parallel) {
        if (is.null(n_cores)) {
            message("You have not specified the number of cores so 2 will be used")
            n_cores <- 2
        }
        parallel_breaks <- split(breaks_to_search,
                                 cut(1:length(breaks_to_search),
                                     breaks = n_cores,
                                     include.lowest = TRUE))
        cl <- parallel::makeCluster(n_cores)
        doParallel::registerDoParallel(cl)
        message("Running gridsearch for optimal cutpoints in parallel on ", n_cores, " cores")
        parallel_list <- foreach::foreach(breaks_to_search = parallel_breaks) %dopar% lapply(breaks_to_search, EstimateLoss)
        parallel::stopCluster(cl)
        loss_list <- do.call(c, parallel_list)
    } else {
        loss.list <- lapply(breaks.to.search, EstimateLoss)   
    }
    loss.data <- data.frame(breaks = do.call(rbind, breaks.to.search), auc = unlist(loss.list))
    ## Make and save plot
    if (save_plot) create.and.save.gridsearch.plot(loss_data)
    ## Get function of max or min
    optimise.function <- get("max")
    if (!maximise) optimise.function <- get("min")
    ## Get optimal cutoffs
    optimal.cutpoints <- unlist(loss.data[order(-loss.data$auc), ][1, 1:n.breaks])
    return(optimal.cutpoints)
}
