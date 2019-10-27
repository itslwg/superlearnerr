#' CreateMortalityPlot
#'
#' Creates a stacked bar plot representing under-triage and over-triage in each triage category.
#' @param predictions.outcome.and.tc List. Predictions, outcome and clinicians triage as list. No default.
#' @param model.labels Character vector of length 1. Element names for model predictions in predictions.outcome.and.tc. If NULL those model predictions whose list label contain "test" but also "cut", "CUT", or "tc". Defaults to NULL.
#' @param outcome.label Character vector of length 1. List label of the outcome.variable. Defaults to "y.test". 
#' @param pretty.names Character vector of length 1. Pretty names for the models to use in the plot. If NULL, Defaults to c("SuperLearner", "Clinicians") 
#' @param save.plot.data.to.results Logical. If TRUE the plot data is saved to results. Defaults to TRUE.
#' @export
CreateMortalityPlot <- function(predictions.outcome.and.tc,
                                model.labels = NULL,
                                outcome.label = "y.test",
                                pretty.names = c("SuperLearner", "Clinicians"),
                                save.plot.data.to.results = TRUE) {
    ## Error handling
    if (!is.list(predictions.outcome.and.tc))
        stop("predictions.outcome.and.tc must be list")
    nms <- names(predictions.outcome.and.tc)
    if (is.null(model.labels))
        model.labels <- nms[grepl("CUT|cut|tc", nms) & grepl("test", nms)]
    ## Make plot data
    plot.data <- do.call(rbind, mapply(function(x, pretty.name) {
        pred <- predictions.outcome.and.tc[[x]]
        outcome <- predictions.outcome.and.tc[[outcome.label]]
        mortality.to.category <- table(pred, outcome)
        n.survived <- mortality.to.category[, 1]
        n.died <- mortality.to.category[, 2]
        perc.died.y <- n.survived + n.died
        perc.died <- paste0(round(prop.table(mortality.to.category, margin = 1)[, 2] * 100), "%")
        data <- data.frame(levels = rep(rownames(mortality.to.category), 2),
                           y = c(n.survived, n.died), strata = rep(c("Survived", "Died"), each = 4),
                           perc.died.y = c(perc.died.y, rep(NA, 4)), perc.died = c(perc.died, rep(NA, 4)))
        data <- data.frame(data, x = rep(letters[1:4], 2), pretty.name = rep(pretty.name, nrow(data)))
        rownames(data) <- NULL
        return(data)
    }, model.labels, pretty.names, SIMPLIFY = FALSE))
    print (plot.data)
    ## Save plot data to results
    if (save.plot.data.to.results)
        bengaltiger::SaveToResults(plot.data, "mortality.plot.data")
    ## Plot function
    colors <- RColorBrewer::brewer.pal(3, "Set2")
    MortalityPlot <- function(plot.data) {
        levels <- c("Green", "Yellow", "Orange", "Red")
        plot.object <- ggplot2::ggplot(data = plot.data) +
            ggplot2::geom_col(ggplot2::aes(y = y, x = x, fill = strata), position = "stack") +
            ggplot2::geom_text(ggplot2::aes(y = perc.died.y + 1,
                                            x = x,
                                            label = perc.died), size = 2, vjust = "bottom") +
            ggplot2::xlab("Priority level") +
            ggplot2::ylab("Number of patients") +
            ggplot2::scale_x_discrete(labels = setNames(levels, letters[1:length(levels)])) + 
            ggplot2::scale_fill_manual(name = "", values = colors[2:3]) +
            ggplot2::theme(legend.position = "bottom",
                           strip.background = ggplot2::element_rect(fill="white"),
                           strip.text = ggplot2::element_text(size = 10, hjust = 0),
                           text = ggplot2::element_text(size = 8),
                           legend.key.size = ggplot2::unit(4, "mm"),
                           legend.key = ggplot2::element_rect(size = 1, colour = "white", linetype = "solid"),
                           plot.margin = ggplot2::unit(c(2,2,2,2),"pt")) +
            ggplot2::facet_wrap(~pretty.name) 
        return(plot.object)
    }
    ## Create plot
    fig <- MortalityPlot(plot.data)
    print (fig)
    ## Save plot
    save.plot(fig, "mortality.plot")
}
