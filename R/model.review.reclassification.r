#' Generate reclassification tables and Net Reclassification Index (NRI)
#'
#' This function compares categorisation of categorised SuperLearner (SL) predictions and clinician's predictions. Analysis is conducted with nribin.
#' @param study_sample Sample as data frame. No default
#' @param which_point_estimates Character vector describing which reclassification proportions to return, for example NRI+ and NRI. No default.
#' @param outcome_name Currently ignored.
#' @param for_tables Boolean value determining whether function should return all nribin elements or solely point estimates. Default: FALSE, i.e. not to include all nribin elements.
#' @export
model.review.reclassification <- function(
                                          study_sample,
                                          which_point_estimates,
                                          outcome_name = NULL,
                                          for_tables = FALSE
                                          )
{
    ## Safely convert character vector to numeric vector
    study_sample$tc <- as.numeric(study_sample$tc)
    study_sample$pred_cat_test <- as.numeric(study_sample$pred_cat_test)
    ## Compute reclassification of SuperLearner model and clinicians
    reclassification <- with(study_sample, nricens::nribin(event = outcome_test,
                                                           p.std = tc,
                                                           p.new = pred_cat_test,
                                                           cut = unique(tc),
                                                           niter = 0,
                                                           msg = for_tables))
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
