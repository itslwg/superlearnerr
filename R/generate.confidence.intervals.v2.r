#' Confidence interval function
#'
#' This function generates confidence intervals around point estimate(s) or around difference of two point estimates.
#' @param study_sample The study sample list. No default.
#' @param model_names Character vector. Names model names. No default.
#' @param the_func Function that generates key statistic. For example, model.review.AUROCC that generates AUROCC of given model(s), or model.review.reclassification that generates reclassification elements of given model(s). No default.
#' @param samples Samples prepared as the study_sample. List of study_sample lists. No default.
#' @param diffci_or_ci String. Whether to return confidence interval on difference of model_or_pointestimates, or return confidence intervals on model_or_pointestimates separately or no confidence intervals. Must be one of c("diff", "ci", "none")
#' @param outcome_name Name of outcome variable. No default.
#' @param digits Integer. Number of decimals when rounded. Defaults to 2.
#' @export
generate.confidence.intervals.v2 <- function(
                                             study_sample,
                                             model_names,
                                             the_func,
                                             samples,
                                             diffci_or_ci,
                                             outcome_name,
                                             digits = 2,
                                             ...
                                             )
{
    ## Get function name
    review_function_name <- as.character(substitute(the_func))[[3]]
    ## Error handling
    if (length(diffci_or_ci) != 1) stop("Argument diffci_or_ci > length 1")
    if (!diffci_or_ci %in% c("diff","ci","none")) stop("Accepted strings are diff, ci, or none")
    if (review_function_name == "model.review.reclassification" && diffci_or_ci == "diff") stop ("Diff ci not useful for reclassification.")
    if (!(length(model_names) == 2) && diffci_or_ci == "diff") stop ("Input two models for diff ci.")
    ## Get func and get study_sample point estimates
    performance_point_estimates <- the_func(study_sample = study_sample,
                                            model_names = model_names,
                                            outcome_name = outcome_name,
                                            ...)
    if (diffci_or_ci == "diff" && !all(lengths(performance_point_estimates) == length(performance_point_estimates[[1]]))) stop("Measure is not useful for diff, rocr estimates not of same length")
    ## Return point estimates untouched if argument diffci_or_ci equals is "none"
    if (diffci_or_ci == "none") return_object <- list(performance_point_estimates = performance_point_estimates)
    ## Return confidence intervals around difference of point estimates
    if (diffci_or_ci == "diff"){
        ## Calculate difference of point estimates
        diff <- performance_point_estimates[[1]] - performance_point_estimates[[2]]
        ## Generate statistic on every bootstrap samples
        generate_statistics_bssamples <- lapply(samples, function (sample)
            the_func(study_sample = sample,
                     model_names = model_names,
                     outcome_name = outcome_name,
                     ...))
        ## Matrixify samples, i.e. generate matrix with point estimate names
        ## as rows and samples as cols. Model estimates are binded rowwise.
        matrixify <- sapply(generate_statistics_bssamples, unlist)
        ## Calculate difference between AUROCCs in every sample
        diff_samples <- matrixify[1,] - matrixify[2,]
        ## Calculate deltastar, i.e difference between sample estimates
        ## and study_sample estimates.
        deltastar <- diff_samples - diff
        ## Get 2.5% and 97.5% percentiles from difference of samples
        quantiles <- quantile(deltastar, c(.025, 0.975))
        ## Generate confidence intervals
        confidence_intervals <- diff - quantiles
        ## Format confidence intervals
        confidence_intervals <- c(lb = round(min(confidence_intervals), digits),
                                  ub = round(max(confidence_intervals), digits))
        ## Return confidence intervals with study_sample point_estimates
        return_object <- list(diff_point_estimate = round(diff, digits),
                              CI_diff = confidence_intervals,
                              performance_point_estimates = performance_point_estimates)
    }
    ## Get confidence_intervals around point estimates
    if (diffci_or_ci == "ci"){
        ## Merge point estimates from each model into numeric vector
        performance_point_estimates <- lapply(performance_point_estimates,
                                              function (p_ests)
                                                  unlist(p_ests))
        ## Generate statistic on every bootstrap samples
        generate_statistics_bssamples <- lapply(samples, function (sample) {
            func_sample <- the_func(study_sample = sample,
                                    model_names = model_names,
                                    outcome_name = outcome_name,
                                    ...)
        }
        )
        ## Matrixify. NRI estimates are merged into one vector.
        ## Vector for each sample as columns
        matrixify <- sapply(generate_statistics_bssamples, unlist)
        ## Adjust for model_names input of length 1
        if (length(model_names) == 1 && !is.matrix(matrixify)) {
            ## Make point estimate matrix
            pe_matrix <- rep(performance_point_estimates[[model_names]],
                             length(matrixify))
            ## Calculate deltastar, i.e difference between sample estimates
            ## and study_sample estimates.
            deltastar <- data.frame(t(matrixify - pe_matrix))
            ## Get 2.5% and 97.5% percentiles from difference of samples
            quantiles <- t(apply(deltastar, 1,
                                 quantile, probs = c(.025,0.975)))
            ## Generate confidence_intevals
            confidence_intervals <- performance_point_estimates[[model_names]] - quantiles
        } else {
            ## Point estimate matrix for each model. Each sample as column. 
            pe_matrices <- lapply(performance_point_estimates,
                                  function(nri_estimates){
                                      matrix(rep(nri_estimates,
                                                 ncol(matrixify)),
                                             ncol = ncol(matrixify))})
            ## Merge point estimate matrices of each model to one matrix
            pe_matrix <- do.call(rbind, pe_matrices)
            ## Calculate deltastar, i.e difference between sample estimates
            ## and study_sample estimates.
            deltastar <- data.frame(t(matrixify - pe_matrix))
            ## Get percentiles for each model estimate, i.e. percentiles column wise.
            ## Then bind list elements into data frame
            quantiles <- do.call(rbind,
                                 lapply(deltastar, quantile, probs = c(0.025,0.975)))
            ## Subtract point estimates from quantiles to get confidence intervals
            confidence_intervals <- pe_matrix[, 1:ncol(quantiles)] - quantiles
        }
        ## Format confidence intervals
        fmt_confidence_intervals <- t(apply(confidence_intervals,
                                            1,
                                            function(row) c(lb = round(min(row), digits),
                                                            ub = round(max(row), digits))))
        ## Return confidence intervals with study_sample point_estimates
        cis <- as.data.frame(cbind(fmt_confidence_intervals,
                                   round(unlist(performance_point_estimates),
                                         digits)),
                             row.names = rownames(matrixify))
        ## Split confidence_intervals data frame in list for each model
        cis <- lapply(setNames(model_names,model_names),
                      function(name){
                          model_cis <- cis[grep(name,
                                                rownames(cis)),]
                          return (model_cis)
                      })
        for (iter in 1:length(cis)){
            ## Set more appropriate colnames
            colnames(cis[[iter]]) <- c("lb", "ub", "point_estimate")
            ## Remove model names from row names if in rownames
            if (any(grepl(model_names[iter], rownames(cis[[iter]])))) {
                rownames(cis[[iter]]) <- gsub(pattern = ".*\\.",
                                              replacement = "",
                                              rownames(cis[[iter]]))
            } else {
                rownames(cis[[iter]]) <- "perf"
            }
        }
        ## Set return object
        return_object <- cis
    }
    return(return_object)
}
