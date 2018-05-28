#' Create ROCR plots
#'
#' Plots reciever operating charecteristiqs curves or precision/recall curves.
#' @param study_sample Study sample list. No default.
#' @param ROC_or_precrec String. To perform ROC or precision/recall analysis. Accepted values are "ROC" or "prec_rec". No default
#' @export
create.ROCR.plots <- function(
                              study_sample,
                              ROC_or_precrec = "ROC"
                              )
{
    ## Error handling
    if (!(ROC_or_precrec %in% c("ROC", "prec_rec"))) stop("Accepted values for ROC_or_precrec argument is ROC and prec_rec")
    ## Models
    models <- c("pred_con_train",
                "pred_cat_train",
                "pred_con_test",
                "pred_cat_test",
                "tc")

    pretty_names <- c("SuperLearner continuous prediction",
                      "SuperLearner priority levels",
                      "SuperLearner continuous prediction",
                      "SuperLearner priority levels",
                      "Clinicians priority levels")

    if (ROC_or_precrec == "ROC") measures <- list(tpr = "tpr",
                                                  fpr = "fpr",
                                                  FPR = "False Positive Rate",
                                                  TPR = "True Positive Rate")
    if (ROC_or_precrec == "prec_rec") measures <- list(prec = "prec",
                                                       rec = "rec",
                                                       PREC = "Precision",
                                                       REC = "Recall")
    ## Get tpr and fpr
    tpr_fpr <- lapply(setNames(nm = models), function(model) {
        outcome <- "outcome_test"
        if (grepl("train", model)) outcome <- "outcome_train"
        pred <- ROCR::prediction(as.numeric(study_sample[[model]]), study_sample[[outcome]])
        perf <- ROCR::performance(pred, measure = measures[[1]], x.measure = measures[[2]])
        return(perf)
    })
    ## Create plot data
    plot_data <- do.call(rbind, lapply(setNames(nm = models), function(model) {
        data <- tpr_fpr[[model]]
        pretty_name <- pretty_names[grep(model, models)]
        set <- "B"
        if (grepl("train", model)) set <- "A"
        new_data <- cbind(data@y.values[[1]], data@x.values[[1]])
        new_data <- data.frame(new_data, rep(set, nrow(new_data)), rep(pretty_name, nrow(new_data)))
        colnames(new_data) <- c(measures[[1]], measures[[2]], "set", "pretty_name")
        return(new_data)
    }))
    ## Create plots
    colors <- brewer.pal(3, "Set2")
    linetypes <- c("solid", "dashed", "dotted")
    roc.plot <- function(plot_data) {
        plot_object <- ggplot(data = plot_data) +
            geom_line(aes_string(x = measures[[2]],
                                 y = measures[[1]],
                                 col = "pretty_name",
                                 linetype = "pretty_name"),
                      size = 1,
                      alpha = 0.8) +
            ylab(measures[[3]]) +
            xlab(measures[[4]]) +
            scale_color_manual(name = "", values = colors) +
            scale_linetype_manual(name = "", values = linetypes) +
            theme(legend.position = "bottom",
                  strip.background = element_rect(fill="white"),
                  strip.text = element_text(size = 20, hjust = 0)) +
            facet_wrap(~set) +
            coord_fixed()
        return(plot_object)
    }
    roc_plot <- roc.plot(plot_data)
    ## Save plots
    if (ROC_or_precrec  == "ROC") plot_name <- "roc_plot.pdf"
    if (ROC_or_precrec  == "prec_rec") plot_name <- "prec_rec_plot.pdf"
    ggsave(plot_name, roc_plot, units = "mm")
    if (grepl("pdf", plot_name)) system(paste("pdfcrop", plot_name, plot_name))
}
