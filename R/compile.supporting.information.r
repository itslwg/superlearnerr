#' Compile supporting information function
#'
#' This function compiles supporting information
#' @param file_name_prefix The file name prefix of the supporting information file.
#' @param results_path The path to the results list rds object. Defaults to "./". 
#' @param file_format The format of the supporting information file. Defaults to "rtex".
#' @param compiler The compiler to use when compiling the rtex manuscript into a pdf. Defaults to pdflatex.
#' @export
compile.supporting.information <- function(
                                           file_name_prefix,
                                           results_path = "./",
                                           file_format = "rtex",
                                           compiler = "pdflatex"
                                           )
{
    ## Error handling
    results_file <- paste0(results_path, "results.rds")
    if (!file.exists(results_file)) stop ("No results list rds exist in that path")
    ## Get results list
    results_list <- readRDS(results_file)
    ## Attached results list
    attach(results_list)
    ## Get list of files matching the manuscript file name prefix
    files <- list.files(pattern = paste0("^", file_name_prefix))
    file <- grep(paste0(file_format, "$"), files, value = TRUE)
    if (length(file) > 1) stop("There are more than 1 file matching both prefix and format. Maybe there are several versions in this directory?")
    ## Compile manuscript using knit2pdf
    knit2pdf(file, compiler = compiler)
}
