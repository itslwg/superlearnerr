#' Compile manuscript function
#'
#' This function compiles the Superlearner vs clinicians manuscript
#' @param manuscript_file_name_prefix The file name prefix of the manuscript as a character vector of length 1. No default.
#' @param manuscript_file_format The format of the manuscript file. Defaults to "rtex".
#' @param compiler The compiler to use when compiling the rtex manuscript into a pdf. Defaults to pdflatex.
#' @export
compile.manuscript <- function(
                               manuscript_file_name_prefix,
                               manuscript_file_format = "rtex",
                               compiler = "pdflatex"
                               )
{
    ## Get list of files matching the manuscript file name prefix
    files <- list.files(pattern = manuscript_file_name_prefix)
    file <- grep(manuscript_file_format, files, value = TRUE)
    if (length(file) > 1) stop("There are more than 1 file matching both prefix and format. Maybe there are several manuscript versions in this directory?")
    ## Compile manuscript using knit2pdf
    knit2pdf(file, compiler = compiler)
}
