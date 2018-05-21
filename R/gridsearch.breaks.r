#' Gridsearch breaks function
#'
#' Performs a gridsearch to identify breaks to optimise a provided loss function
#' @param predictions A numeric vector of predicted probabilites. No default.
#' @param outomes A numeric vector of 0 and 1. No default.
#' @param n_breaks Number of breaks, i.e. cutpoints. Defaults to 3.
#' @param loss_function The name of the loss function as a character vector. For now any performance measure included in ROCR is accepted. Defaults to "auc".
#' @param sample Logical. If TRUE loss is only calculated for 10% of possible breaks. Defaults to TRUE. 
#' @param save_plot Logical. If TRUE a wireframe plot is saved to disk. Defaults to FALSE.
#' @param verbose Logical. If TRUE then information is printed as the gridsearch runs. Defaults to FALSE. Currently ignored.
#' @export
gridsearch.breaks <- function(
                              predictions,
                              outcomes,
                              n_breaks = 3,
                              loss_function = "auc",
                              sample = TRUE,
                              save_plot = FALSE,
                              verbose = FALSE
                              )
{
    ## Verify that predictions appear to be probabilites
    if (min(predictions) < 0 | max(predictions) > 1) stop("Predictions are not probabilites")
    ## Verify that outcomes is a vector of binary data with 0 and 1
    if (!all(levels(as.factor(outcomes)) %in% c("0", "1"))) stop("Outcomes is not a vector of binary data with only 0 and 1")
    ## Verify that there are no missing values in predictions or outcomes
    if (!all(!is.na(predictions)) | !all(!is.na(predictions))) stop("There is missing data in predictions or outcomes")
    ## Loss functions
    loss_functions <- list(auc = function(pred) ROCR::performance(pred, "auc", x.measure =  "cutoff")@y.values[[1]])
    ## Define grid
    breaks_to_search <- combn(seq(0.01, 0.99, 0.01), n_breaks, simplify = FALSE)
    ## Draw a random sample if sample is TRUE
    if (sample) breaks_to_search <- breaks_to_search[sample(1:length(breaks_to_search), ceiling(length(breaks_to_search) * 0.1))]
    ## Do gridsearch
    loss_list <- lapply(breaks_to_search, function(breaks) {
        breaks <- c(-Inf, breaks, Inf)
        groupings <- as.numeric(cut(predictions, breaks = breaks))
        rocr_pred <- ROCR::prediction(groupings, outcomes)
        loss <- loss_functions[[loss_function]](rocr_pred)
        return(loss)
    })
    ## Loss data
    loss_data <- data.frame(do.call(rbind, breaks_to_search), loss = unlist(loss_list))
    ## Make and save plot
    if (save_plot) create.and.save.gridsearch.plot(loss_data)
    ## Get optimal cutoffs
    optimal_cutpoints <- loss_data[loss_data$loss == max(loss_data$loss), 1:n_breaks]
    optimal_cutpoints <- as.numeric(optimal_cutpoints[sample(1:nrow(optimal_cutpoints), 1), ]) # Select one random set of cutpoints if more than one has the same loss
    return(optimal_cutpoints)
}
