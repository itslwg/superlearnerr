#' Add missing indicator variables function
#'
#' This function creates missing indicator variables for each predictor variable
#' (feature) with missing values
#' @param study_data The study data as a data frame. No default
#' @param outcome_name The name of outcome variable. Defaults to "m30d"
#' @param features The names of the feature variables, as a character vector. Defaults to NULL, in which case all columns except outcome_name and those potentially listed in excluded_columns are treated as features.
#' @param excluded_columns The names of columns that should not be treated as features. Defaults to NULL and is then ignored. Is also ignored if features is specified.
#' @export
add.missing.indicator.variables <- function(
                                            study_data,
                                            outcome_name = "s30d",
                                            features = NULL,
                                            excluded_columns = c("tc")
                                            )
{
    ## Test that all columns specified in outcome_name, features and excluded
    ## features (if any) are in study_data
    column_names <- colnames(study_data)
    stopifnot(all(c(outcome_name, features, excluded_columns) %in% column_names))
    ## Create vector of features
    if (is.null(features)) features <- column_names[!(column_names %in% c(outcome_name, excluded_columns))]
    ## Get feature data only
    feature_data <- study_data[, features]
    ## Define function to get either number of missing values or indicator variable
    get.missing.info <- function(x, n = TRUE, variable = FALSE)
    {
        n_missing <- sum(is.na(x))
        indicator_variable <- NULL
        if (n_missing > 0) indicator_variable <- as.numeric(!is.na(x))
        if (n & !variable) return(n_missing)
        if (variable) return(indicator_variable)
    }
    ## Count number of missing values in each feature
    missing_data_counts <- lapply(feature_data, get.missing.info)
    missing_data_matrix <- do.call(rbind, missing_data_counts)
    ## results$missing_data_table <<- create.missing.data.table(missing_data_matrix)
    ## Create a dummy variable for each feature with missing
    indicator_variables <- lapply(feature_data, get.missing.info, variable = TRUE)
    indicator_data <- do.call(cbind, indicator_variables)
    colnames(indicator_data) <- paste0(colnames(indicator_data), "_missing")
    study_data <- data.frame(study_data, indicator_data)
    return(study_data)
}
