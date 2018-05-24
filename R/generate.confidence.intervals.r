#' Confidence interval function
#'
#' This function generates confidence intervals around difference of two given point estimates using empirical bootstrapping.
#' @param study_sample The study sample list. No default.
#' @param func Function that generates key statistic. For example, model.review.AUROCC that generates AUROCC of a given model, or model.review.reclassification that generates reclassification elements. No default.
#' @param model_or_pointestimate Character vector describing predictions or point estimates to analyse. For example, pred_cat and clinicians_predictions predictions for model.review.AUROCC or NRI+ and NRI- point estimates for model.review.reclassification. No default.
#' @param samples Samples as prepared with train.predict.bssamples. No default.
#' @param diffci_or_ci String. Whether to return confidence interval on difference of model_or_pointestimates, or return confidence intervals on model_or_pointestimates separately or no confidence intervals. Must be one of c("diff", "ci", "none")
#' @param outcome_name Name of outcome variable. No default.
#' @export
generate.confidence.intervals <- function(
                                          study_sample,
                                          model_or_pointestimate,
                                          func,
                                          samples,
                                          diffci_or_ci,
                                          outcome_name
                                          )
{
    ## Error handling
    if (length(diffci_or_ci) != 1) stop("Argument diffci_or_ci > length 1")
    if (!diffci_or_ci %in% c("diff","ci","none")) stop("Accepted strings are diff, ci, or none")
    ## Get point estimates
    performance_point_estimates <- func(study_sample, model_or_pointestimate, outcome_name)
    if (diffci_or_ci == "none") return_object <- list(performance_point_estimates = performance_point_estimates)
    ## Return confidence intervals around difference of point estimates
    if (diffci_or_ci == "diff"){
        ## Calculate difference of point estimates
        diff <- performance_point_estimates[[1]] - performance_point_estimates[[2]]
        ## Generate statistic on every bootstrap samples
        generate_statistics_bssamples <- lapply(samples,
                                                function (sample) func(sample,
                                                                       model_or_pointestimate,
                                                                       outcome_name))
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
        ## Format confidence intervals
        confidence_intervals <- c(lb = min(confidence_intervals), ub = max(confidence_intervals))
        ## Return confidence intervals with study_sample point_estimates
        return_object <- list(diff_point_estimate = diff,
                              CI_diff = confidence_intervals,
                              performance_point_estimates = performance_point_estimates)
    }
    if (diffci_or_ci == "ci"){
        ## Get point estimates of func
        performance_point_estimates <- unlist(performance_point_estimates)
        ## Generate statistic on every bootstrap samples
        generate_statistics_bssamples <- lapply(samples,
                                                function (sample) func(sample,
                                                                       model_or_pointestimate,
                                                                       outcome_name))
        ## Matrixify samples
        matrixify <- sapply(generate_statistics_bssamples, unlist)
        ## Make point estimate matrix
        pe_matrix <- matrix(rep(performance_point_estimates, ncol(matrixify)), ncol = ncol(matrixify))
        ## Calculate deltastar
        deltastar <- data.frame(t(matrixify - pe_matrix))
        ## Get 2.5% and 97.5% percentiles from difference of samples
        quantiles <- do.call(rbind, lapply(deltastar, quantile, probs = c(.025,0.975)))
        ## Generate confidence_intevals
        confidence_intervals <- matrix(rep(performance_point_estimates, 2), ncol = 2) - quantiles
        ## Format confidence intervals
        fmt_confidence_intervals <- t(apply(confidence_intervals,
                                            1,
                                            function(row) c(lb = min(row), ub = max(row))))
        ## Return confidence intervals with study_sample point_estimates
        confidence_intervals <- as.data.frame(cbind(fmt_confidence_intervals, performance_point_estimates))
        rownames(confidence_intervals) <- model_or_pointestimate
        return_object <- confidence_intervals
    }
    return(return_object)
}
