#' Extract from table function
#'
#' Extracts and formats data from supplied table
#' @param variable The variable on which to extract data. No default.
#' @param level The variable level on which to extract data. Defaults to NULL.
#' @param strata The name of the strata from which to extract data. Defaults to "overall".
#' @param format The format to use when formatting the numbers as characters. Defaults to "%.0f".
#' @param data_object The data object. No default.
#' @param table_object The table object. No default
#' @export
extract.from.table <- function(
                               variable,
                               level = NULL,
                               strata = "overall",
                               format = "%.0f",
                               data_object,
                               table_object
                               )
{
    ## Get column index
    column <- grep(strata, colnames(table_object), ignore.case = TRUE)
    ## Get row index
    row <- min_row <- grep(paste0("^", variable), rownames(table_object))
    if (!is.null(level)) {
        levels <- levels(data_object[, variable])
        level <- gsub("[ \t\r\n]+", " ", level)
        if (!(level %in% levels)) stop ("The level you provided is not valid")
        row <- min_row + grep(level, levels) - 1
    }
    ## Get data from table
    data <- table_object[row, column]
    ## Format new data based on the type of data extracted
    if (length(grep("[", data, fixed = TRUE)) > 0) {
        numbers <- as.numeric(trimws(unlist(strsplit(data, "\\[|,|\\]"))))
        formatted_numbers <- as.list(setNames(sprintf(format, numbers), c("pe", "lb", "ub")))
        new_data <- with(formatted_numbers, paste0(pe, " (IQR ", lb, "-", ub, ")"))
    } else {
        numbers <- as.numeric(trimws(unlist(strsplit(data, "\\(|\\)"))))
        formatted_numbers <- as.list(setNames(sprintf(format, numbers), c("pe", "perc")))
        new_data <- with(formatted_numbers, paste0(pe, " (", perc, "\\%", ")"))
    }
    ## Remove *
    new_data <- sub("*", "", new_data, fixed = TRUE)
    return(new_data)
}
