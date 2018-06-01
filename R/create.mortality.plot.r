#' Create mortality plots
#'
#' Mortality plots
#' @param study_sample Study sample object. No default.
#' @param save_plot_data_to_results Logical. If TRUE the plot data is saved to results. Defaults to TRUE.
#' @export
create.mortality.plot <- function(
                                  study_sample,
                                  save_plot_data_to_results = TRUE
                                  )
{
    ## Error handling
    if (!is.list(study_sample)) stop("Study sample has to be a list")
    ## Make plot data
    plot_data <- do.call(rbind, lapply(c("pred_cat_test", "tc"), function(x) {
        pred <- study_sample[[x]]
        outcome <- study_sample$outcome_test
        data_table <- table(pred, outcome)
        n_survived <- data_table[, 1]
        n_died <- data_table[, 2]
        perc_died_y <- n_survived + n_died
        perc_died <- paste0(round(prop.table(data_table, margin = 1)[, 2] * 100), "%")
        pretty_name <- "SuperLearner"
        if (x == "tc") pretty_name <- "Clinicians"
        data <- data.frame(levels = rep(rownames(data_table), 2), y = c(n_survived, n_died), strata = rep(c("Survived", "Died"), each = 4), perc_died_y = c(perc_died_y, rep(NA, 4)), perc_died = c(perc_died, rep(NA, 4)))
        data <- data.frame(data, x = rep(letters[1:4], 2), pretty_name = rep(pretty_name, nrow(data)))
        rownames(data) <- NULL
        return(data)
    }))
    ## Save plot data to results
    if (save_plot_data_to_results) results$mortality_plot_data <<- plot_data
    ## Plot function
    colors <- brewer.pal(3, "Set2")
    mortality.plot <- function(plot_data) {
        levels <- levels(study_sample$tc)
        plot_object <- ggplot(data = plot_data) +
            geom_col(aes(y = y, x = x, fill = strata), position = "stack") +
            geom_text(aes(y = perc_died_y + 1, x = x, label = perc_died), size = 2, vjust = "bottom") +
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
            facet_wrap(~pretty_name) 
        return(plot_object)
    }
    ## Create plot
    mortality_plot <- mortality.plot(plot_data)
    ## Save plot
    save.plot(mortality_plot, "mortality_plot")
}
