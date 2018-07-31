#' Create ROCR plots
#'
#' Plots reciever operating charecteristiqs curves or precision/recall curves.
#' @param study_sample Study sample list. No default.
#' @param device Character vector with the name of the image device to use. Passed to rocr.plot (in turn, passed to save.plot). Defaults to "eps".
#' @param train_test Logical. Is the dataset splitted in train and test set? Defaults to TRUE.
#' @param split_var The variable used to split plots. As string. Defaults to "train".
#' @param ROC_or_precrec String. To perform ROC or precision/recall analysis. Accepted values are "ROC" or "prec_rec". No default.
#' @param models Model names as character vector. Defaults to c("pred_con_train", "pred_cat_train", "pred_con_test", "pred_cat_test", "tc")
#' @param pretty_names Names to be used in plots. As character vector. Defaults to c("SuperLearner continuous prediction", "SuperLearner priority levels", "SuperLearner continuous prediction", "SuperLearner priority levels", "Clinicians priority levels")
#' @param subscript Logical. If TRUE, underscores in pretty names in converted to expression. Passed to rocr.plots. Defaults to FALSE.
#' @param models_to_invert Character vector. Names of models to invert. Defaults to NULL.
#' @export
create.ROCR.plots.v2 <- function(
                              study_sample,
                              outcome_name,
                              device = "eps",
                              split_var = "train",
                              train_test = TRUE,
                              ROC_or_precrec = "ROC",
                              models = c("pred_con_train",
                                         "pred_cat_train",
                                         "pred_con_test",
                                         "pred_cat_test",
                                         "tc"),
                              pretty_names = c("SuperLearner continuous prediction",
                                               "SuperLearner priority levels",
                                               "SuperLearner continuous prediction",
                                               "SuperLearner priority levels",
                                               "Clinicians priority levels"),
                              subscript = FALSE,
                              models_to_invert = NULL
                              )
{
    ## Error handling
    if (!(ROC_or_precrec %in% c("ROC", "prec_rec"))) stop("Accepted values for ROC_or_precrec argument is ROC and prec_rec")
    ## Define setting depending on type of plot
    if (ROC_or_precrec == "ROC") measures <- list(tpr = "tpr",
                                                  fpr = "fpr",
                                                  TPR = "True positive rate",
                                                  FPR = "False positive rate")
    if (ROC_or_precrec == "prec_rec") measures <- list(prec = "prec",
                                                       rec = "rec",
                                                       PREC = "Precision",
                                                       REC = "True positive rate (recall)")
    ## Get tpr and fpr (or precision)
    analysis_comps <- lapply(setNames(nm = models), function(model) {
        ## Set outcome according to set outcomes (necessary when sample is split)
        outcome <- outcome_name
        if (train_test) {
            outcome <- "outcome_test"
            if (grep("test", model)) outcome <- "outcome_test"
        }
        pred <- ROCR::prediction(as.numeric(study_sample[[model]]), study_sample[[outcome]])
        perf <- ROCR::performance(pred, measure = measures[[1]], x.measure = measures[[2]])
        return(perf)
    })
    ## Create plot data
    plot_data <- do.call(rbind, lapply(setNames(nm = models), function(model) {
        data <- analysis_comps[[model]]
        pretty_name <- pretty_names[grep(model, models)]
        set <- "B"
        if (!grepl(split_var, model)) set <- "A"
        y.values <- data@y.values[[1]]
        x.values <- data@x.values[[1]]
        if (model %in% models_to_invert) {
            x.values <- 1 - x.values
            y.values <- 1 - y.values
        }
        new_data <- cbind(y.values, x.values)
        new_data <- data.frame(new_data, rep(set, nrow(new_data)), rep(pretty_name, nrow(new_data)))
        colnames(new_data) <- c(measures[[1]], measures[[2]], "set", "pretty_name")
        return(new_data)
    }))
    ## Create and save plots
    if (ROC_or_precrec  == "ROC") plot_name <- "roc_plot"
    if (ROC_or_precrec  == "prec_rec") plot_name <- "prec_rec_plot"
    rocr.plot(plot_data = plot_data,
              y_name = measures[[1]],
              x_name = measures[[2]],
              ylab = measures[[3]],
              xlab = measures[[4]],
              device = device,
              file_name = plot_name,
              subscript = subscript)
}
