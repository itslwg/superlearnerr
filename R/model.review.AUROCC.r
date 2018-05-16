#' Area Under Receiver Operating Characteristics Curve (AUROCC) function
#'
#' This function calculates AUROCC of the specified predictions.
#' @param study_sample The study sample as a data frame. No default
#' @param which_preds Character vector with predictions. Default: c('pred_cat', 'clinicians_predictions')
#' @export
 model.review.AUROCC <- function(
                                study_sample,
                                which_preds
                                )
{
    ### Stop if user inputs more than two predicitons
    #if (length(which_preds) > 2) stop('Input two predictions')
    ### Stop if study_sample isn't a dataframe
    #if (!(is.data.frame(study_sample))) stop('Input study_sample as dataframe')
    ## Set up prediction obejects for ROCR
    pred_rocr <- lapply(which_preds,
                        function(pred) ROCR::prediction(
                                                 as.numeric(
                                                     study_sample$preds[[pred]]),
                                                 study_sample$preds$outcome_review))
    ## Set names for AUROCCs
    names(pred_rocr) <- which_preds
    ## Calculate the Area Under the Receiver Operating Charecteristics Curve
    AUROCC <- lapply(pred_rocr,
                     function(model) ROCR::performance(model,
                                                       measure = 'auc',
                                                       x.measure =  'cutoff')@y.values[[1]])

    return (AUROCC)
 }
