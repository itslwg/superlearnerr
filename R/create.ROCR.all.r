#' Generate plot
#'
#' Plots receiver operating characteristics curves of all models.
#' @param study_sample Study sample object. No default.
#' @export
create.ROCR.all <- function(
                            study_sample
                            )
{
    models_data <- readRDS("C:\\Users\\Ludvig Gerdin\\Documents\\Projects\\SuperLearner\\SupaLarna\\superlearner.rds")$library.predict
    models <- colnames(models_data)
    pretty_names <- c("GLMnet",
                      "GLM",
                      "Random Forest",
                      "XGboost",
                      "GAM")
    ## Initiate list to populate with dataframe columns
    l_of_predictions <- list()
    ## Data frame columns to list
    for (i in colnames(models_data)){
        l_of_predictions[[i]] <- models_data[, i]
    }
    ## Set names of list
    names(l_of_predictions) <- models
    tpr_fpr <- lapply(setNames(nm = models), function(model) {
        pred <- ROCR::prediction(as.numeric(l_of_predictions[[model]]), study_sample[["outcome_train"]])
         perf <- ROCR::performance(pred, measure = "tpr", x.measure = "fpr")
         return(perf)
    })
    plot_data <- do.call(rbind, lapply(setNames(nm = models), function(model) {
        data <- tpr_fpr[[model]]
        pretty_name <- pretty_names[grep(model, models)]
        new_data <- cbind(data@y.values[[1]], data@x.values[[1]])
        new_data <- data.frame(new_data,
                               rep(model, nrow(new_data)))
        colnames(new_data) <- c("tpr", "fpr","pretty_name")
        return(new_data)
    }))
    ## Create plots
    colors <- brewer.pal(length(models), "Set2")
    linetypes <- c("solid", "dashed", "dotted")
    roc.plot <- function(plot_data) {
        plot_object <- ggplot(data = plot_data) +
            geom_line(aes(x = fpr, y = tpr, col = pretty_name), size = 0.5, alpha = 0.8) +
            xlab("False positive rate") +
            ylab("True positive rate") +
            scale_color_manual(name = "", values = colors) +
            theme(legend.position = "bottom",
                  strip.background = element_rect(fill="white"),
                  strip.text = element_text(size = 20, hjust = 0))
        return(plot_object)
    }
    roc.plot(plot_data)
    roc <- roc.plot(plot_data)
    ## Save plots
    plot_name <- "roc_plot_all.pdf"
    ggsave(plot_name, roc, units = "mm")
    if (grepl("pdf", plot_name)) system(paste("pdfcrop", plot_name, plot_name))
}
