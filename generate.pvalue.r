#' Generate p-values function
#'
#' This function generate p-values of difference of AUROCC between categorised SuperLearner predictions and clinicians triage category, as well as between continous and categorised SuperLearner predictions - the latter to evaluate the chosen cutoffs for the cut predicitons.
#' @param study_sample The study data as a data frame. No default
#' @param bs_samples The number of bootstrap samples to be generated. Specified in main.study()
#' @export
generate.pvalue <- function(
                            study_sample,
                            bs_samples
                            )
{
    ## Generate p-value on difference of AUROCC of SuperLearner and clinicians
    p1 <- significance.testing(study_sample,
                               which_models = c('pred_cat',
                                                'clinicians_predictions'),
                               bs_samples)
    ## Generate p-value on difference of AUROCC of SuperLearner's continous
    ## predictions and categorised predictions
    p2 <- significance.testing(study_sample,
                               which_models = c('pred_con',
                                                'pred_cat'),
                               bs_samples)
    return (c(p1,p2))
}
