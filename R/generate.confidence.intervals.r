#' Confidence interval function
#'
#' This function generates confidence intervals around difference of two given point estimates using empirical bootstrapping.
#' @param study_data The study data as data frame. No default
#' @param func Function that generates key statistic. For example, model.review.AUROCC that generates AUROCC of a given model, or model.review.reclassification that generates reclassification elements. No default.
#' @param model_or_pointestimate Character vector describing predictions or point estimates to analyse. For example, pred_cat and clinicians_predictions predictions for model.review.AUROCC or NRI+ and NRI- point estimates for model.review.reclassification. No default.
#' @param samples Samples as prepared with train.predict.bssamples. No default.
#' @export
generate.confidence.intervals <- function(
                                          study_data,
                                          model_or_pointestimate,
                                          func,
                                          samples
                                          )
{
    ## Get point estimates of func
    study_point_estimates <- func(study_data, model_or_pointestimate)
    ## Calculate difference of point estimates
    diff <- study_point_estimates[[1]] - study_point_estimates[[2]]
    ## Generate statistic on every bootstrap samples
    generate_statistic_bssamples <- lapply(samples,
                                           function (df) func(df, model_or_pointestimate))
    ## Matrixify samples
    matrixify <- sapply(generate_statistic_bssamples, unlist)
    ## Calculate difference between AUROCCs in every sample
    diff_samples <- matrixify[1,] - matrixify[2,]
    ## Calculate deltastar
    deltastar <- diff_samples - diff
    ## Get 2.5% and 97.5% percentiles from difference of samples
    quantiles <- quantile(deltastar, c(.025, 0.975))
    ## Generate confidence intervals
    confidence_intervals <- diff - quantiles

    return(confidence_intervals)
}
