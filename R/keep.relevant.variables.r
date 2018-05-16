#' Keep relevant variables
#'
#' This function takes as only input the study data data frame and keeps only
#' those variables that are relevant to the study
#' @param study_data The study data as a data frame. No default
#' @param data_dictionary The data dictionary. Defaults to NULL. If provided, variables_to_keep will be defined as unlist(lapply(data_dictionary, function(x) if(x$c == "No") x$vn))
#' @param variables_to_keep Character vector of variables to keep. Defaults to NULL. Ignored if data_dictionary is not NULL.
#' @param variables_to_drop Character vector of variables to drop. Defaults to NULL. Currently ignored.
#' @export
keep.relevant.variables <- function(
                                    study_data,
                                    data_dictionary = NULL,
                                    variables_to_keep = NULL,
                                    variables_to_drop = NULL
                                    )
{
    ## Error handling
    if (!is.data.frame(study_data)) stop("Study data is not a data frame")
    if (is.null(data_dictionary) & is.null(variables_to_keep)) stop("Either data dictionary or variables to keep has to be provided")
    ## Define variables to keep
    if (!is.null(data_dictionary)) variables_to_keep <- unlist(lapply(data_dictionary, function(x) if(x$c == "No") x$vn))
    ## Keep only relevant variables
    study_data <- study_data[, variables_to_keep]
    return (study_data)
}
