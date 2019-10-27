#' CreateMortalityPlot
#'
#' Creates a stacked bar plot representing under-triage and over-triage in each triage category.
#' @param predictions.outcome.and.tc List. Predictions, outcome and clinicians triage as list. No default.
#' @param save_plot_data_to_results Logical. If TRUE the plot data is saved to results. Defaults to TRUE.
#' @export
CreateMortalityPlot <- function(predictions.outcome.and.tc,
                                save.plot.data.to.results = TRUE) {
    ## Error handling
    if (!is.list(predictions.outcome.and.tc))
        stop("predictions.outcome.and.tc must be list")
    ## Make plot data
    plot.data <- do.call(rbind, lapply(c("cut.model.test", "tc.test"), function(x) {
        pred <- predictions.outcome.and.tc[[x]]
        outcome <- predictions.outcome.and.tc$y.test
        mortality.to.category <- table(pred, outcome)
        n.survived <- mortality.to.category[, 1]
        n.died <- mortality.to.category[, 2]
        perc.died.y <- n.survived + n.died
        perc.died <- paste0(round(prop.table(mortality.to.category, margin = 1)[, 2] * 100), "%")
        pretty.name <- "SuperLearner"
        if (x == "tc.test")
            pretty.name <- "Clinicians"
        data <- data.frame(levels = rep(rownames(mortality.to.category), 2),
                           y = c(n.survived, n.died), strata = rep(c("Survived", "Died"), each = 4),
                           perc.died.y = c(perc.died.y, rep(NA, 4)), perc.died = c(perc.died, rep(NA, 4)))
        data <- data.frame(data, x = rep(letters[1:4], 2), pretty.name = rep(pretty.name, nrow(data)))
        print (data)
        rownames(data) <- NULL
        return(data)
    }))
    ## Save plot data to results
    if (save.plot.data.to.results)
        bengaltiger::SaveToResults(plot.data, "mortality.plot.data")
    ## Plot function
    colors <- RColorBrewer::brewer.pal(3, "Set2")
    MortalityPlot <- function(plot.data) {
        levels <- c("Green", "Yellow", "Orange", "Red")
        plot.object <- ggplot(data = plot.data) +
            geom_col(aes(y = y, x = x, fill = strata), position = "stack") +
            geom_text(aes(y = perc.died.y + 1, x = x, label = perc.died), size = 2, vjust = "bottom") +
            xlab("Priority level") +
            ylab("Number of patients") +
            scale_x_discrete(labels = setNames(levels, letters[1:length(levels)])) + 
            scale_fill_manual(name = "", values = colors[2:3]) +
            theme(legend.position = "bottom",
                  strip.background = element_rect(fill="white"),
                  strip.text = element_text(size = 10, hjust = 0),
                  text = element_text(size = 8),
                  legend.key.size = unit(4, "mm"),
                  legend.key = element_rect(size = 1, colour = "white", linetype = "solid"),
                  plot.margin = unit(c(2,2,2,2),"pt")) +
            facet_wrap(~pretty.name) 
        return(plot.object)
    }
    ## Create plot
    fig <- MortalityPlot(plot.data)
    print (fig)
    ## Save plot
    save.plot(fig, "mortality.plot")
}
