#' Report function
#'
#' Function to format results for reporting in the results section
#' @param variable The name of the variable to report on. No default.
#' @param level The name of the level to report on. Defaults to NULL.
#' @param strata The name of the strata from which to extract data. Defaults to "overall". 
#' @param data_object The data object. Defaults to study_data.
#' @param table_object The table object. Defaults to results$raw_table_of_sample_characteristics.
#' @export
report <- function(
                   variable,
                   level = NULL,
                   strata = "overall",
                   data_object = study_data,
                   table_object = results$raw_table_of_sample_characteristics
                   )
{
    ## Error handling
    stopifnot(!is.null(variable) | !is.null(level))
    ## Get data
    data <- extract.from.table(variable = variable,
                               level = level,
                               strata = strata,
                               table_object = table_object,
                               data_object = data_object)
    ## Display data
    return(data)
}
