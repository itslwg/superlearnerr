#' Create and save gridsearch plot function
#'
#' Plots the results of the gridsearch for optimal cutpoints
#' @param loss_data The data frame with cutpoints and loss. No default.
#' @param sample Logical. If TRUE only a sample of loss data is plotted. Defaults to TRUE.
#' @export
create.and.save.gridsearch.plot <- function(
                                            loss_data,
                                            sample = TRUE
                                            )
{
    ## Error handling
    if (!is.data.frame(loss_data)) stop("Loss data has to be a data frame")
    ## Sample data if sample is TRUE
    if (sample) loss_data <- loss_data[sample(1:nrow(loss_data), 10000), ]
    ## Define colors
    col <- colorRampPalette(brewer.pal(9,"YlOrRd"))(10000)
    ## Order data
    loss_data <- loss_data[order(loss_data[, 4]), ]
    ## Build plot
    gridsearch_plot <- cloud(loss_data[, 1] ~ loss_data[, 3] * loss_data[, 2],
                             scales = list(arrows = FALSE),
                             zlab = "Cutoff 1",
                             xlab = "Cutoff 3",
                             ylab = "Cutoff 2",
                             col = col)
    ## Print and save plot
    pdf("gridsearch_plot.pdf")
    print(gridsearch_plot)
    dev.off()
}
