#' ROCR plotter
#'
#' Plots ROCROCR
#' @param plot_data The plot data object. No default.
#' @param device Character vector with the name of the image device to use. Passed to save.plot. Defaults to "eps".
#' @param y_name The name of the y variable. Defaults to NULL in which case the first variable is used.
#' @param x_name The name of the x variable. Defaults to NULL in which case the second variable is used.
#' @param ylab The y label. Defaults to NULL in which case the variable name is used.
#' @param xlab The x label. Defaults to NULL in which case the variable name is used.
#' @param file_name If not NULL the plot object will be saved to disk using the name provided. Defaults to NULL.
#' @param return_plot Logical. If TRUE the plot object is returned. Defaults to FALSE.
#' @param subscript Logical. If TRUE, underscores in pretty names in converted to expression. Defaults to FALSE.
#' @export
rocr.plot <- function(
                      plot_data,
                      device = eps,
                      y_name = NULL,
                      x_name = NULL,
                      ylab = NULL,
                      xlab = NULL,
                      file_name = NULL,
                      return_plot = FALSE,
                      subscript = FALSE
                      )
{
    ## Number of colors and linetypes
    n <- length(levels(as.factor(plot_data$pretty_name)))
    ## Defina colors
    colors <- RColorBrewer::brewer.pal(n, "Set1")
    ## Define linetypes
    linetypes <- c(1:n)
    ## Get y and x variables
    if (is.null(y_name)) y_name <- names(plot_data)[1]
    if (is.null(x_name)) x_name <- names(plot_data)[2]
    ## Define axis labels
    if (is.null(ylab)) ylab <- gsub("EP", ")", gsub("BP", "(", gsub("_", " ", y_name)))
    if (is.null(xlab)) xlab <- gsub("EP", ")", gsub("BP", "(", gsub("_", " ", x_name)))
    ## Subset pretty names
    pretty_name <- as.list(as.character(unique(plot_data$pretty_name)))
    ## Subscript if argument is given
    if (subscript) {
        ## Sub to create math mode strings
        pretty_name <- gsub(" ",
                            "$ $", ## For text spaces
                            paste0("$",
                                   gsub("_",
                                        "_{",
                                        pretty_name),
                                   "}$"))
        ## Coerce to TeX format
        pretty_name <- unique(lapply(pretty_name, latex2exp::TeX))
    }
    ## Define axis limits
    lim <- c(0,1)
    ## Create plot object
    plot_object <- ggplot2::ggplot(data = plot_data) +
        ggplot2::geom_line(ggplot2::aes_string(x = x_name,
                                               y = y_name,
                                               col = "pretty_name",
                                               linetype = "pretty_name"), size = 0.5, alpha = 0.8) +
        ggplot2::xlab(xlab) +
        ggplot2::ylab(ylab) +
        ggplot2::xlim(lim) +
        ggplot2::ylim(lim) +
        ggplot2::scale_colour_manual(name = "",
                                     values = colors,
                                     labels = pretty_name) +
        ggplot2::scale_linetype_manual(name = "",
                                       values = linetypes,
                                       labels = pretty_name) +
        ggplot2::theme(legend.position = "bottom",
                       strip.background = ggplot2::element_rect(fill="white"),
                       strip.text = ggplot2::element_text(size = 12, hjust = 0),
                       text = ggplot2::element_text(size = 8),
                       legend.key.size = ggplot2::unit(4, "mm"),
                       legend.key = ggplot2::element_rect(size = 1, colour = "white", linetype = "solid"),
                       plot.margin = ggplot2::unit(c(2,2,2,2),"pt")) +
        ggplot2::guides(colour = ggplot2::guide_legend(nrow = ceiling(n/3))) +
        ggplot2::facet_wrap(~set) +
        ggplot2::coord_equal(ratio=1)
    ## Save
    if (!is.null(file_name)) save.plot(plot_object, file_name, device = device)
    ## Return
    if (return_plot) return(plot_object)
}
