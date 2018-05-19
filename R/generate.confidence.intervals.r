#' Confidence interval function
#'
#' This function generates confidence intervals around difference of two given point estimates using empirical bootstrapping.
#' @param measure The name of the measure to be estimated. No default.
#' @param study_sample The study sample list. No default.
#' @param func Function that generates key statistic. For example, model.review.AUROCC that generates AUROCC of a given model, or model.review.reclassification that generates reclassification elements. No default.
#' @param model_or_pointestimate Character vector describing predictions or point estimates to analyse. For example, pred_cat and clinicians_predictions predictions for model.review.AUROCC or NRI+ and NRI- point estimates for model.review.reclassification. No default.
#' @param samples Samples as prepared with train.predict.bssamples. No default.
#' @export
generate.confidence.intervals <- function(
                                          measure,
                                          study_sample,
                                          model_or_pointestimate,
                                          func,
                                          samples
                                          )
{
    ## Get point estimates of func
    study_point_estimates <- func(study_sample, model_or_pointestimate)
    ## Save point estimates to results
    # results[[paste0(measure, "_point_estimate")]] <<- 
    ## Calculate difference of point estimates
    diff <- study_point_estimates[[1]] - study_point_estimates[[2]]
    ## Generate statistic on every bootstrap samples
    bssamples_statistics <- lapply(samples,
                                   function (sample) func(sample, model_or_pointestimate))
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
