#' Save plot
#'
#' Wrapper for ggsave that also crops and converts to eps if necessary
#' @param plot_object The ggplot object. No default.
#' @param file_name The plot file name. No default.
#' @param width The width of the plot in cm. Defaults to 13.2.
#' @param height The height of the plot in cm. Default to 9.
#' @param device Character vector with the name of the image device to use. Defaults to "eps".
#' @param convert_pdf_to_eps Logical. If TRUE and device = "pdf" then an attempt to convert that pdf using pdftops to eps is made. Defaults to TRUE.
#' @export
save.plot <- function(
                      plot_object,
                      file_name,
                      width = 13.2,
                      height = width * 0.65,
                      device = "eps",
                      convert_pdf_to_eps = TRUE
                      )
{
    ## Setup device
    font_device <- device
    if (device == "eps") font_device = "postscript"
    ## Generate full file name
    full_file_name <- paste0(file_name, ".", device)
    ## Setup ggplot arguments depending on device
    ggargs <- list(filename = full_file_name,
                   plot = plot_object,
                   device = font_device,
                   width = width,
                   height = height,
                   units = "cm",
                   family = "ArialMT")
    if (device == "eps") {
        ggargs$fallback_resolution <- 300
        ggargs$device <- cairo_ps
    }
    ## Save plot
    loadfonts(device = font_device, quiet = TRUE)
    do.call(ggsave, ggargs)
    ## Embed fonts
    ## embed_fonts(full_file_name, outfile = full_file_name, options = "-dEPSCrop")
    ## Report that plot is saved
    message(paste0(full_file_name, " saved to disk"))
    ## Convert to eps
    if (convert_pdf_to_eps & device == "pdf") {
        eps_name <- paste0(file_name, ".eps")
        convert.to.eps <- function() {
            system(paste("pdftops -eps", full_file_name, eps_name))
            message(paste0(full_file_name, " converted to ", eps_name))
        }
        tryCatch(convert.to.eps(),
                 warning = function(w) print(w),
                 error = function(e) print(e))
    }
}
    
