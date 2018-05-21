#' Create calibration plots
#'
#' Calibration plots
#' @param study_sample Study sample object. No default.
#' @export
create.calibration.plots <- function(
                                     study_sample
                                     )
{
    ## Error handling
    if (!is.list(study_sample)) stop("Study sample has to be a list")
    ## Make plot data
    plot_data <- do.call(rbind, lapply(c("pred_con_train", "pred_con_test"), function(x) {
        pred <- study_sample[[x]]
        grouping <- cut(pred, breaks = seq(0,1,0.1), include.lowest = TRUE)
        outcome_name <- "outcome_train"
        strip_title <- "A"
        if (grepl("test", x)) {
            outcome_name <- "outcome_test"
            strip_title <- "B"
        }
        outcome <- study_sample[[outcome_name]]
        mean_pred <- unlist(lapply(levels(grouping), function(i) mean(pred[grouping == i])))
        mean_outcome <- unlist(lapply(levels(grouping), function(i) mean(outcome[grouping == i])))
        data <- cbind(mean_pred, mean_outcome)
        data <- round(data * 100)
        data <- data.frame(data, seq(0,100, length.out = nrow(data)), rep(strip_title, nrow(data)))
        colnames(data) <- c("x", "y", "ref", "strip_title")
        data <- na.omit(data)
        return(data)
    }))
    ## Calibration plot function
    colors <- brewer.pal(3, "Set2")
    calibration.plot <- function(plot_data) {
        plot_object <- ggplot(data = plot_data) +
            geom_line(aes(y = ref, x = ref), size = 0.5, linetype = "dotted", color = "black") + 
            geom_smooth(aes(y = y, x = x), method = "loess", se = FALSE, color = colors[2], size = 0.5) +
            geom_point(aes(y = y, x = x, shape = strip_title, col = strip_title)) +
            scale_color_manual(values = rep(colors[1], 2)) +
            scale_shape_manual(values = c(2, 2)) +
            theme(strip.background = element_rect(fill="white"),
                  strip.text = element_text(size = 20, hjust = 0),
                  legend.position = "none") +
            facet_wrap(~strip_title) +
            ylab("Observed all cause 30-day mortality %") +
            xlab("Predicted all cause 30-day mortality %") +
            coord_fixed()
        return(plot_object)
    }
    ## Create plot
    calibration_plot <- calibration.plot(plot_data)
    ## Save plot
    ggsave("calibration_plot.pdf", calibration_plot)
}

    
