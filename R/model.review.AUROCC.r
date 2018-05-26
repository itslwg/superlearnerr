#' Area Under Receiver Operating Characteristics Curve (AUROCC) function
#'
#' This function calculates AUROCC of predictions of the specified models.
#' @param study_sample The study sample list. No default.
#' @param which_preds Character vector with list name of predictions. Default: c('pred_cat', 'tc')
#' @param outcome_name The name of the outcome variable. No default.
#' @export
 model.review.AUROCC <- function(
                                study_sample,
                                which_preds = c('pred_cat', 'tc'),
                                outcome_name
                                )
{
    ## Set up prediction obejects for ROCR
    pred_rocr <- lapply(which_preds,
                        function(pred) ROCR::prediction(
                                                 as.numeric(
                                                     study_sample[[pred]]),
                                                 study_sample[[outcome_name]]))
    ## Set names for AUROCCs
    names(pred_rocr) <- which_preds
    ## Calculate the Area Under the Receiver Operating Charecteristics Curve
    AUROCC <- lapply(pred_rocr,
                     function(model) ROCR::performance(model,
                                                       measure = 'auc',
                                                       x.measure =  'cutoff')@y.values[[1]])

    return (AUROCC)
 }
