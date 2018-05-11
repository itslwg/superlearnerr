#' Generate reclassification tables and Net Reclassification Index (NRI)
#'
#' This function compares categorisation of categorised SuperLearner predictions and clinician's predictions. Analysis is conducted with nribin which creates cross tabulations of categorisation of the two, description of net proportions patient movements upwards and downwards in categories, and NRI.
#' @param study_sample Sample as data frame. No default
#' @param which_point_estimates Character vector describing which reclassification proportions to return, for example NRI+ and NRI. No default.
#' @param for_tables Boolean value determining whether function should return all nribin elements or solely point estimates. Default: FALSE, i.e. not to include all nribin elements.
#' @export
model.review.reclassification <- function(
                                          study_sample,
                                          which_point_estimates,
                                          for_tables = FALSE
                                          )
{
    ## Compute reclassification of SuperLearner model and clinicians
    reclassification <- nricens::nribin(event = study_sample$data$outcome$y_review,
                                        p.std = study_sample$data$sets_w_tc$x_review$tc,
                                        p.new = as.numeric(study_sample$preds$pred_cat),
                                        cut = c(2,3,4),
                                        niter = 0,
                                        msg = for_tables)
    ## Create list of point_estimates
    list_of_estimates <- lapply(which_point_estimates,
                                function(p_est)
                                    reclassification$nri[p_est,])
    ## Set names of point estimates in list
    names(list_of_estimates) <- which_point_estimates

    if (for_tables == TRUE){
        return(reclassification)
    } else {
        return (list_of_estimates)
    }
}
