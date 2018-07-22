#' Review model with rocr package function
#'
#' This function reviews a model using the rocr package.
#' @param study_sample The study sample list. No default.
#' @param outcome_name The name of the outcome variable. No default.
#' @param model_names Character vector with list name of predictions. Default: c('pred_cat', 'tc')
#' @param measure String. Performance measure for ROCR prediction object. Defaults to "auc".
#' @export
 model.review.with.rocr <- function(
                                    study_sample,
                                    outcome_name,
                                    model_names= c('pred_cat', 'tc'),
                                    measure = "f"
                                    )
 {
     ## Generate AUROCC of models
     perf_lst <- lapply(setNames(nm = model_names), function(model_name) {
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
             return (perf@y.values[[1]])
         } else {
             ## Coerce to individual lsts for each value
             ind_lst <- setNames(as.list(y.values),
                                 nm = paste0("cp_at_", x.values))
             return (ind_lst)
         }
     })
     return (perf_lst)
}
