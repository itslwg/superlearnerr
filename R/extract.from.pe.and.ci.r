#' Extract from point estimates and confidence intervals list function
#'
#' This function extracts data from the point estimates and confidence intervals
#' list and puts them in results
#' @param pe_and_ci The point estimates and confidence intervals list
#' @export
extract.from.pe.and.ci <- function(
                                   pe_and_ci
                                   )
{
    ## Error handling
    if (!is.list(pe_and_ci)) stop("pe_and_ci has to be a list")
    ## Formatting function
    format <- function(x, fmt = "%.4f") sprintf(fmt, x)
    ## Extract
    ci_str <- " (95\\% CI "
    results <- list(superlearner_cat_auc_train = format(pe_and_ci$auc_train$performance_point_estimates$pred_cat_train),
                    superlearner_con_auc_train = format(pe_and_ci$auc_train$performance_point_estimates$pred_con_train),
                    superlearner_cat_auc_test = format(pe_and_ci$auc_main$performance_point_estimates$pred_cat_test),
                    superlearner_con_auc_test = format(pe_and_ci$auc_test$performance_point_estimates$pred_con_test),
                    tc_auc_test = format(pe_and_ci$auc_main$performance_point_estimates$tc),
                    diff_superlearner_cat_tc_auc = paste0(format(pe_and_ci$auc_main$diff_point_estimate),
                                                          ci_str,
                                                          format(pe_and_ci$auc_main$CI_diff["lb"]),
                                                          " - ",
                                                          format(pe_and_ci$auc_main$CI_diff["ub"]),
                                                          ")"),
                    nre = paste0(format(pe_and_ci$nri_main["NRI+", "performance_point_estimates"]),
                                  ci_str,
                                  format(pe_and_ci$nri_main["NRI+", "lb"]),
                                  " - ",
                                  format(pe_and_ci$nri_main["NRI+", "ub"]),
                                  ")"),
                    nrne = paste0(format(pe_and_ci$nri_main["NRI-", "performance_point_estimates"]),
                                  ci_str,
                                  format(pe_and_ci$nri_main["NRI-", "lb"]),
                                  " - ",
                                  format(pe_and_ci$nri_main["NRI-", "ub"]),
                                  ")"))
    return(results)
}
