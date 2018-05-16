#' Compile manuscript function
#'
#' This function compiles the Superlearner vs clinicians manuscript
#' @param manuscript_file_name The file name of the manuscript as a character vector of length 1. No default.
#' @param compiler The compiler to use when compiling the rtex manuscript into a pdf. Defaults to pdflatex.
#' @export
compile.manuscript <- function(
                               manuscript_file_name,
                               compiler = "pdflatex"
                               )
{
    ## Compile manuscript using knit2pdf
    knit2pdf(manuscript_file_name, compiler = compiler)
}
