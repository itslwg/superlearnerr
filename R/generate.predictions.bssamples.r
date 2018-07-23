#' Function to train and predict SuperLearner on each sample
#'
#' This trains SuperLearner on every sample training set and reviews SuperLearner on every sample review set.
#' @param samples Bootstrap samples as prepared by prep.bssamples. No default.
#' @param prediction_func Name of function that generates predictions. Defaults to "predictions.with.superlearner"
#' @param parallel Logical. If TRUE the training will be run in parallel. Defaults to FALSE.
#' @param n_cores Integer. The number of cores to use for parallel computing. Defaults to NULL.
#' @param log Logical. Passed to predictions.with.superlearner. If TRUE progress is logged in logfile. Defaults to TRUE.
#' @param boot Logical. Passed to predictions.with.superlearner. Affects only what is printed to logfile. If TRUE prepped_sample is assumed to be a bootstrap sample. Defaults to FALSE.
#' @param write_to_disk Logical. Passed to predictions.with.superlearner. If TRUE the prediction data is saved as RDS to disk. Defaults to FALSE
#' @export
generate.predictions.bssamples <- function(
                                           samples,
                                           prediction_func = "predictions.with.superlearner",
                                           parallel = FALSE,
                                           n_cores = NULL,
                                           log = TRUE,
                                           boot = FALSE,
                                           write_to_disk = FALSE
                                           )
{
    ## Error handling
    if (!is.list(samples)) stop("Samples argument is not a list")
    ## Get package list
    SupaLarna_imports <- desc::desc(package = "SupaLarna")$get_deps()
    .package_list <- SupaLarna_imports[grep("Imports", SupaLarna_imports$type),]$package
    ## Define predictions func
    pred_func <- get(prediction_func)
    ## Log
    starttime <- Sys.time()
    if (log) write(paste0("Started running analysis of ", length(samples), " bootstrap samples on ", starttime), "logfile", append = TRUE)
    ## Train and predict on each bootstrap sample dataframe
    if (parallel) {
        if (is.null(n_cores)) {
            n_cores <- 2
            message("You did not specify the number of cores so 2 will be used")
        }
        files <- list.files("./R", pattern = ".r$", full.names = TRUE)
        files_wo_ext <- unlist(lapply(files,
                                      function (file_name)
                                          gsub("./R/", "",
                                               tools::file_path_sans_ext(file_name))))
        cl <- parallel::makeCluster(n_cores)
        doParallel::registerDoParallel(cl)
        message(paste0("Running predictions func on bootstrap samples in parallel on ", n_cores, " cores"))
        predictions <- foreach::foreach(sample = samples, .packages = .package_list, .export = c(files_wo_ext, "gridsearch.breaks")) %dopar% pred_func(sample, n_cores = n_cores, log = TRUE, boot = TRUE, write_to_disk = TRUE)
        parallel::stopCluster(cl)
    }
    else predictions <- lapply(samples, pred_func, log = TRUE, boot = TRUE, write_to_disk = TRUE)
    ## Log
    stoptime <- Sys.time()
    timediff <- as.numeric(difftime(stoptime, starttime, units = "mins"))
    if (log) write(paste0("Stopped running analysis of ", length(samples), " bootstrap samples on ", stoptime, "\n", "It took ", timediff, " minutes"), "logfile", append = TRUE)
    return(predictions)
}
