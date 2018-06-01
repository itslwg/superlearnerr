#' Generate ROCR plot for all SL learners
#'
#' Plots receiver operating characteristics curves of all learners included in SL.
#' @param prepped_sample The prepped_sample list. No default.
#' @export
create.ROCR.all <- function(
                            prepped_sample
                            )
{
    ## Load model object
    superlearner <- readRDS("./superlearner.rds")
    ## Get predictions of SL learners from training set
    model_data <- data.frame(do.call(cbind, predict(superlearner_object, newdata = prepped_sample$x_review)))
    ## Initiate vector with titles for plot
    pretty_names <- c("SuperLearner",
                      "GLMnet",
                      "GLM",
                      "Random Forest",
                      "XGboost",
                      "GAM")
    ## Initiate list to populate with dataframe columns and, then, fill
    predictions_list <- lapply(setNames(model_data, nm = pretty_names), function(x) x)
    ## Define functions
    get.perf.list <- function(predictions_list, measures, outcome) {
        lapply(setNames(nm = names(predictions_list)), function(model) {
            pred <- ROCR::prediction(as.numeric(predictions_list[[model]]), outcome)
            perf <- ROCR::performance(pred, measure = measures$measure, x.measure = measures$x.measure)
            return(perf)
        })
    }
    create.plot.data <- function(perf_list, set) {
        do.call(rbind, lapply(setNames(nm = names(perf_list)), function(model) {
            data <- perf_list[[model]]
            new_data <- cbind(data@y.values[[1]], data@x.values[[1]])
            y_name <- gsub(" ", "_", data@y.name)
            x_name <- gsub(" ", "_", data@x.name)
            if (x_name == "Recall") x_name <- paste0("True_positive_rate_BP", tolower(x_name), "EP")
            new_data <- data.frame(new_data,
                                   rep(model, nrow(new_data)),
                                   rep(set, nrow(new_data)))
            colnames(new_data) <- c(y_name, x_name, "pretty_name", "set")
            return(new_data)
        }))
    }
    ## Get true positive and false positive rates
    outcome <- prepped_sample$y_review
    measures <- list(measure = "tpr", x.measure = "fpr")
    tpr_fpr <- get.perf.list(predictions_list, measures, outcome)
    roc_plot_data <- create.plot.data(tpr_fpr, "A")
    ## Get recall and precision
    prec_rec <- get.perf.list(predictions_list, list(measure = "prec", x.measure = "rec"), outcome)
    prec_plot_data <- create.plot.data(prec_rec, "B")
    ## Create plots
    roc_plot <- rocr.plot(plot_data = roc_plot_data, return_plot = TRUE)
    prec_rec_plot <- rocr.plot(plot_data = prec_plot_data, return_plot = TRUE)
    ## Arrange plot grid
    combined_plot <- ggarrange(roc_plot, prec_rec_plot,
                               ncol = 2,
                               common.legend = TRUE,
                               legend = "bottom",
                               align = "hv")
    ## Save plot
    save.plot(combined_plot, "roc_prec_plots", device = "eps")
}
