#' P-value generating function
#'
#' This function generates p-value of difference in AUROCC using a permutation test.
#' @param study_sample The study sample as a data frame. No default
#' @param which_models Character vector describing models to compare in AUROCCs. Default: c('pred_cat', 'clinicians_predictions').
#' @param bs_samples The number of bootstrap samples to be generated. Specified in main.study()
#' @export
significance.testing <- function(
                                 study_sample,
                                 which_models = c('pred_cat',
                                                  'clinicians_predictions'),
                                 bs_samples
                                 )
{
    ## Get data for significance testing
    data <- predictions.with.superlearner(study_sample)
    ## Create dataframe for significance testing
    df <- data.frame(x = c(rep('model_1',
                               each = length(data[[which_models[1]]])),
                           rep('model_2',
                               each = length(data[[which_models[2]]]))),
                     Variable = c(data[[which_models[1]]],
                                  data[[which_models[2]]]),
                     s30d = as.factor(rep(data$outcome_review, 2)))
    ## Bootstrap diff on bs_samples bootstrap samples
    boot_strapped <- boot::boot(data = df,
                                statistic = diff,
                                R = bs_samples)
    ##Generate p-value
    p_value <- mean(abs(boot_strapped$t) > abs(boot_strapped$t0))

    return (p_value)
}
