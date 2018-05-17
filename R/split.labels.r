#' Split labels
#'
#' Splits labels
#' @param labels String of labels separated by comma, no default
#' @export
split.labels <- function(
                         labels
                         )
{
    ## Remove quotation marks and split on commas
    labels <- trimws(gsub("\\\"", "", unlist(strsplit(labels, ","))))
    return (labels)
}
