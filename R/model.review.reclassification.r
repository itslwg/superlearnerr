#' Generate reclassification tables and Net Reclassification Index (NRI)
#'
#' This function compares categorisation of categorised SuperLearner (SL) predictions and clinician's predictions. Analysis is conducted with nribin.
#' @param study_sample Sample as data frame.  No default
#' @param model_names Names of list elements with model predictions in study_sample. Character vector. No default.
#' @param outcome_name Name of outcome variable. As string. Defauls to "s30d".
#' @param models_to_invert Character vector. Names of models to invert. Defaults to NULL.
#' @export
model.review.reclassification <- function(
                                          study_sample,
                                          model_names,
                                          outcome_name = "s30d",
                                          models_to_invert = NULL
                                          )
{
    ## Error handling
    if (!("tc" %in% names(study_sample))) stop ("Triage category variable not in dataset.")
    ## Compute reclassification of models and clinicians
    reclassification_objs <- lapply(model_names, function(model){
        ## Invert models given by models_to_invert
        p.new <- study_sample[[model]]
        if (model %in% models_to_invert){
            p.new <- factor(study_sample[[model]])
            levels(p.new) <- c(4,3,2,1)
            p.new <- as.numeric(as.character(p.new))
        }
        reclass <- nricens::nribin(event = study_sample[[outcome_name]],
                                   p.std = study_sample$tc,
                                   p.new = p.new,
                                   cut = sort(unique(study_sample$tc)),
                                   niter = 0,
                                   msg = FALSE)
        return (reclass)
    })
    ## Extract nri dfs from nri objects and coerce to lists
    reclassification_lsts <- lapply(setNames(reclassification_objs, nm = model_names), function (obj) {
        ## Subset list from nri object
        nri_df <- obj$nri
        ## Corce nri data frame to list
        as_list <- setNames(as.list(nri_df[, "Estimate"]),
                            nm = rownames(nri_df))

        return (as_list)
    }
    )

    return (reclassification_lsts)
}
