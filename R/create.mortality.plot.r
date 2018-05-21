#' Create mortality plots
#'
#' Mortality plots
#' @param study_sample Study sample object. No default.
#' @export
create.mortality.plot <- function(
                                  study_sample
                                  )
{
    ## Error handling
    if (!is.list(study_sample)) stop("Study sample has to be a list")
    ## Make plot data
    plot_data <- do.call(rbind, lapply(c("pred_cat_test", "tc"), function(x) {
        pred <- study_sample[[x]]
        outcome <- study_sample$outcome_test
        prop_table <- prop.table(table(pred, outcome), margin = 1)
        pretty_name <- "SuperLearner"
        if (x == "tc") pretty_name <- "Clinicians"
        data <- data.frame(levels = rownames(prop_table), y = round(prop_table[, 2] * 100, digits = 2))
        data <- data.frame(data, x = letters[1:nrow(data)], pretty_name = rep(pretty_name, nrow(data)))
        rownames(data) <- NULL
        return(data)
    }))
    ## Plot function
    colors <- brewer.pal(3, "Set2")
    mortality.plot <- function(plot_data) {
        levels <- levels(study_sample$tc)
        plot_object <- ggplot(data = plot_data) +
            geom_col(aes(y = y, x = x, fill = pretty_name), position = "dodge") +
            xlab("Priority level") +
            ylab("All cause 30-day mortality %") +
            scale_x_discrete(labels = setNames(levels, letters[1:length(levels)])) + 
            scale_fill_manual(name = "", values = colors[2:3]) +
            theme(legend.position = "bottom") 
        return(plot_object)
    }
    ## Create plot
    mortality_plot <- mortality.plot(plot_data)
    ## Save plot
    ggsave("mortality_plot.pdf", mortality_plot)
}
