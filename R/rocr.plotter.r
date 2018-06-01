#' ROCR plotter
#'
#' Plots ROCROCR
#' @param plot_data The plot data object. No default.
#' @param y_name The name of the y variable. Defaults to NULL in which case the first variable is used.
#' @param x_name The name of the x variable. Defaults to NULL in which case the second variable is used.
#' @param ylab The y label. Defaults to NULL in which case the variable name is used.
#' @param xlab The x label. Defaults to NULL in which case the variable name is used.
#' @param file_name If not NULL the plot object will be saved to disk using the name provided. Defaults to NULL.
#' @param return_plot Logical. If TRUE the plot object is returned. Defaults to FALSE.
#' @export
rocr.plot <- function(
                      plot_data,
                      y_name = NULL,
                      x_name = NULL,
                      ylab = NULL,
                      xlab = NULL,
                      file_name = NULL,
                      return_plot = FALSE
                      )
{
    ## Number of colors and linetypes
    n <- length(levels(as.factor(plot_data$pretty_name)))
    ## Defina colors  
    colors <- brewer.pal(n, "Set2")
    ## Define linetypes
    linetypes <- c(1:n)
    ## Get y and x variables
    if (is.null(y_name)) y_name <- names(plot_data)[1]
    if (is.null(x_name)) x_name <- names(plot_data)[2]
    ## Define axis labels
    if (is.null(ylab)) ylab <- gsub("EP", ")", gsub("BP", "(", gsub("_", " ", y_name)))
    if (is.null(xlab)) xlab <- gsub("EP", ")", gsub("BP", "(", gsub("_", " ", x_name)))
    ## Define axis limits
    lim <- c(0,1)
    ## Create plot object
    plot_object <- ggplot(data = plot_data) +
        geom_line(aes_string(x = x_name, y = y_name, col = "pretty_name", linetype = "pretty_name"), size = 0.5, alpha = 0.8) +
        xlab(xlab) +
        ylab(ylab) +
        xlim(lim) +
        ylim(lim) +
        scale_color_manual(name = "", values = colors) +
        scale_linetype_manual(name = "", values = linetypes) +
        theme(legend.position = "bottom",
              strip.background = element_rect(fill="white"),
              strip.text = element_text(size = 12, hjust = 0),
              text = element_text(size = 8),
              legend.key.size = unit(4, "mm"),
              legend.key = element_rect(size = 1, colour = "white", linetype = "solid"),
              plot.margin = unit(c(2,2,2,2),"pt")) +
        guides(colour = guide_legend(nrow = ceiling(n/3))) +
        facet_wrap(~set) +
        coord_equal(ratio=1)
    ## Save 
    if (!is.null(file_name)) save.plot(plot_object, file_name)
    ## Return
    if (return_plot) return(plot_object)
} 
