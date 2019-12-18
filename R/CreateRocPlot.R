#' Create Roc Plot
#'
#' Plots receiver operating charecteristiqs curves or precision/recall curves using ROCR.
#' @param predictions.outcome.and.tc List. Predicted probabilities, cut predicted probabilites, clinicians triage category and the outcome for both the training and test partitions. Assumes that the list elements have suffix "train" and "test" indicating study sample partition it belongs to. No default.
#' @param model.labels Character vector. The model labels in the predictions.outcome.and.tc list. Defaults to c("con.model.train","cut.model.train","con.model.test","cut.model.test", "tc.train", "tc.test")
#' @param pretty.names Character vector. The pretty labels for the model labels. The labels that are used in the plot. Defaults to c("SuperLearner continuous prediction","SuperLearner priority levels","SuperLearner continuous prediction", "SuperLearner priority levels", "Clinicians priority levels", "Clinicians priority levels")
#' @param roc.or.precrec String. To perform ROC or precision/recall analysis. Accepted values are "ROC" or "prec.rec". No default.
#' @param ... Arguments for helper function PlotRoc.
#' @export
CreateRocPlot <- function(predictions.outcome.and.tc,
                          model.labels = c("con.model.train",
                                           "cut.model.train",
                                           "con.model.validation",
                                           "cut.model.validation",                                           
                                           "con.model.test",
                                           "cut.model.test",
                                           "tc.train",
                                           "tc.validation",
                                           "tc.test"),
                          pretty.names = c("SuperLearner continuous prediction",
                                           "SuperLearner priority levels",
                                           "SuperLearner continuous prediction",
                                           "SuperLearner priority levels",
                                           "SuperLearner continuous prediction",
                                           "SuperLearner priority levels",
                                           "Clinicians priority levels",
                                           "Clinicians priority levels",
                                           "Clinicians priority levels"),
                          roc.or.precrec = "roc", ...) {
    ## Error handling
    if (!is.list(predictions.outcome.and.tc))
        stop("predictions.outcome.and.tc must be of type list")
    if (length(model.labels) != length(pretty.names))
        stop("pretty.names vector must be the same length as model.labels")
    if (!(roc.or.precrec %in% c("roc", "prec.rec")))
        stop("Accepted values for roc.or.precrec is roc and prec.rec")
    ## Define setting depending on type of plot
    if (roc.or.precrec == "roc")
        measures <- list(tpr = "tpr",
                         fpr = "fpr",
                         TPR = "True positive rate",
                         FPR = "False positive rate")
    if (roc.or.precrec == "prec.rec")
        measures <- list(prec = "prec",
                         rec = "rec",
                         PREC = "Precision",
                         REC = "True positive rate (recall)")
    ## Get list of ROCR performance objects. One for each model.label
    tpr.fpr <- lapply(setNames(nm = model.labels), function(model) {
        ## Get the model extension
        outcome.label <- "y.train"
        if (grepl("validation", model)) {
            outcome.label <- "y.validation"
        } else if (grepl("test", model)) {
            outcome.label <- "y.test"
        }
        pred <- ROCR::prediction(predictions = predictions.outcome.and.tc[[model]],
                                 labels = predictions.outcome.and.tc[[outcome.label]])
        perf <- ROCR::performance(pred, measure = measures[[1]], x.measure = measures[[2]])
        return(perf)
    })
    ## Create plot data
    plot.data <- do.call(rbind, lapply(setNames(nm = model.labels), function(model) {
        rocr.data <- tpr.fpr[[model]]
        pretty.name <- pretty.names[grep(model, model.labels)]
        set <- "A"
        if (grepl("validation", model)){
            set <- "B"
        } else if (grepl("test", model)) {
            set <- "C"
        }
        new.data <- cbind(rocr.data@y.values[[1]], rocr.data@x.values[[1]])
        new.data <- data.frame(new.data, rep(set, nrow(new.data)), rep(pretty.name, nrow(new.data)))
        colnames(new.data) <- c(measures[[1]], measures[[2]], "set", "pretty.name")
        return(new.data)
    }))
    ## Create and save plots
    plot.name <- ifelse(roc.or.precrec == "roc", yes = "roc.plot", no = "prec.rec.plot")
    plt <- PlotRoc(plot.data = plot.data, y.name = measures[[1]],
                   x.name = measures[[2]], ylab = measures[[3]],
                   xlab = measures[[4]], file.name = plot.name,
                   ...)
    if (!is.null(file.name))
        SavePlot(plot.object = plot.object,
                 file.name = file.name,
                 device = device)
    return (plt)
}
