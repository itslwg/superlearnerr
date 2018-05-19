#' Confidence interval function
#'
#' This function generates confidence intervals around difference of two given point estimates using empirical bootstrapping.
#' @param measure The name of the measure to be estimated. No default.
#' @param study_sample The study sample list. No default.
#' @param func Function that generates key statistic. For example, model.review.AUROCC that generates AUROCC of a given model, or model.review.reclassification that generates reclassification elements. No default.
#' @param model_or_pointestimate Character vector describing predictions or point estimates to analyse. For example, pred_cat and clinicians_predictions predictions for model.review.AUROCC or NRI+ and NRI- point estimates for model.review.reclassification. No default.
#' @param samples Samples as prepared with train.predict.bssamples. No default.
#' @param diffci_or_ci String. Whether to return confidence interval on difference of model_or_pointestimates, or return confidence intervals on model_or_pointestimates separately. Accepted characters are "diff" or "ci".
#' @export
generate.confidence.intervals <- function(
                                          measure,
                                          study_sample,
                                          model_or_pointestimate,
                                          func,
                                          samples,
                                          diffci_or_ci
                                          )
{
    ## Error handling
    if (length(diffci_or_ci) != 1) stop("Argument diffci_or_ci > length 1")
    if (!diffci_or_ci %in% c("diff","ci")) stop("Accepted strings are diff or ci")
    ## Return confidence intervals around difference of point estimates
    if (diffci_or_ci == "diff"){
        ## Get point estimates of func
        study_point_estimates <- func(study_sample, model_or_pointestimate)
        ## Calculate difference of point estimates
        diff <- study_point_estimates[[1]] - study_point_estimates[[2]]
        ## Generate statistic on every bootstrap samples
        generate_statistics_bssamples <- lapply(samples,
                                                function (sample) func(sample,
                                                                       model_or_pointestimate))
        ## Matrixify samples
        matrixify <- sapply(generate_statistics_bssamples, unlist)
        ## Calculate difference between AUROCCs in every sample
        diff_samples <- matrixify[1,] - matrixify[2,]
        ## Calculate deltastar
        deltastar <- diff_samples - diff
        ## Get 2.5% and 97.5% percentiles from difference of samples
        quantiles <- quantile(deltastar, c(.025, 0.975))
        ## Generate confidence intervals
        confidence_intervals <- diff - quantiles
        ## Return confidence intervals with study_sample point_estimates
        return(list(CI_diff = confidence_intervals,
                    study_point_estimates = study_point_estimates))
    }
    if (diffci_or_ci == "ci"){
        ## Get point estimates of func
        study_point_estimates <- unlist(func(study_sample, model_or_pointestimate))
        ## Generate statistic on every bootstrap samples
        generate_statistics_bssamples <- lapply(samples,
                                                function (sample) func(sample,
                                                                       model_or_pointestimate))
        ## Matrixify samples
        matrixify <- sapply(generate_statistics_bssamples, unlist)
        ## Calculate deltastar
        deltastar <- apply(matrixify,
                           2,
                           function (col) col - study_point_estimates)
        ## Get 2.5% and 97.5% percentiles from difference of samples
        quantiles <- apply(deltastar,
                           1,
                           function(row) quantile(row, c(.025,0.975)))
        ## Generate confidence_intevals
        confidence_intervals <- apply(quantiles,
                                      1,
                                      function(row) study_point_estimates - row)
        ## Return confidence intervals with study_sample point_estimates
        confidence_intervals <- cbind(confidence_intervals, study_point_estimates)
        return(confidence_intervals)
    }
}
