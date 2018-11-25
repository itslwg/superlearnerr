#' Review model with rocr package function
#'
#' This function reviews a model using the rocr package.
#' @param study_sample The study sample list. No default.
#' @param outcome_name The name of the outcome variable. No default.
#' @param model_names Character vector with list name of predictions. Default: c('pred_cat', 'tc')
#' @param measure String. Performance measure for ROCR prediction object. Defaults to "auc".
#' @param invert Logical. If TRUE, 1 - measure is returned. Defaults to FALSE.
#' @param models_to_invert Character vector. Names of models to invert. Defaults to NULL.
#' @export
 model.review.with.rocr <- function(
                                    study_sample,
                                    outcome_name,
                                    model_names= c('pred_cat', 'tc'),
                                    measure = "f",
                                    models_to_invert = NULL
                                    )
 {
     ## Generate AUROCC of models
     perf_lst <- lapply(setNames(nm = model_names), function(model_name,
                                                             models_to_invert) {
         ## Setup ROCR prediction object
         pred <- ROCR::prediction(as.numeric(study_sample[[model_name]]),
                                  study_sample[[outcome_name]])
         ## Generate perf object and extract estimates
         perf <-  ROCR::performance(pred,
                                    measure = measure,
                                    x.measure = "cutoff")
         ## Subset y value(s) and x.value(s)
         y.values <- perf@y.values[[1]]
         x.values <- NULL
         ## Modify y values if invert
         if (model_name %in% models_to_invert) y.values <- 1 - y.values
         ## Append to x.values if length is not 0, i.e.
         ## names of cut points exist
         if (length(perf@x.values) != 0){
             x.values <- perf@x.values[[1]]
         }
         if (!is.null(x.values) && any(!is.finite(x.values))){
             finit_logic <- is.finite(x.values)
             x.values <- x.values[finit_logic]
             y.values <- y.values[finit_logic]
         }
         ## Adjust for output without x.values, without
         ## for example x cut points
         if (is.null(x.values)){
             return (y.values)
         } else {
             ## Coerce to individual lsts for each value
             ind_lst <- setNames(as.list(y.values),
                                 nm = paste0("cp_at_", x.values))
             return (ind_lst)
         }
     }, models_to_invert = models_to_invert)
     return (perf_lst)
}
