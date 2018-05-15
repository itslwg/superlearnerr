#' Add star caption
#'
#' Adds caption*
#' @param xtable The xtable object as latex, no default.
#' @param star_caption The caption to be added as caption*, no default.
#' @export
add.star.caption <- function(
                             xtable,
                             star_caption
                             )
{
    ## Add star caption to xtable
    table_with_added_caption <- sub("\\end{table}",
                                    paste0("\\caption*{", star_caption, "} \n\\end{table}"),
                                    xtable,
                                    fixed = TRUE)
    return(table_with_added_caption)
}
