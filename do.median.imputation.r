#' Do median imputation function
#'
#' Performs median imputation on variables with missing data
#' @param study_data The study data frame. No default.
#' @param missing_indicator A character vector of length 1 with the string used to idenfity variables with missing data to be imputed. Defaults to "_missing"
#' @export
do.median.imputation <- function(
                                 study_data,
                                 missing_indicator = "_missing"
                                 )
{
    ## Error handling
    if (!is.data.frame(study_data)) stop("Study data is not a data frame")
    if (length(missing_indicator) != 1) stop("Missing indicator length is not 1")
    ## Find variables to impute
    variables_to_impute <- sub(missing_indicator, "", grep(missing_indicator, colnames(study_data), value = TRUE))
    ## Impute those variables
    study_data[, variables_to_impute] <- lapply(study_data[, variables_to_impute], function(x) {
        if (is.factor(x)) most_common_value <- names(which.max(table(x))) # If the variable is a factor replace missing with the most common level
        if (is.numeric(x)) most_common_value <- median(x, na.rm = TRUE) # If the variable is numeric replace with median
        x[is.na(x)] <- most_common_value # Replace missing
        return(x)
    })
    return(study_data)
}
