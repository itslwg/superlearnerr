#' Generate reclassification tables and Net Reclassification Index (NRI)
#'
#' This function compares categorisation of categorised SuperLearner predictions and clinician's predictions. Analysis is conducted with nribin which creates cross tabulations of categorisation of the two, description of net proportions patient movements upwards and downwards in categories, and NRI.
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
    ## Change levels of tc
    levels(study_sample$tc) <- c("1","2","3","4")
    ## Safely convert character vector to numeric vector
    study_sample$tc <- as.numeric(as.character(study_sample$tc))
    ## Compute reclassification of SuperLearner model and clinicians
    reclassification <- with(study_sample, nricens::nribin(event = outcome_test,
                                                           p.std = as.numeric(pred_cat_test),
                                                           p.new = tc,
                                                           cut = c(1,2,3),
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
