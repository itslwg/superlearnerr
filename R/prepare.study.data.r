#' Prepare study data function
#'
#' Prepares the study data using the data dictionary
#' @param study_data The study data frame. No default.
#' @param data_dictionary The data dictionary object. No default.
#' @param test Logical. If TRUE seqn is preserved in study_data. Defaults to FALSE.
#' @export
prepare.study.data <- function(
                               study_data,
                               data_dictionary,
                               test = FALSE
                               )
{
    ## Error handling
    if (!is.data.frame(study_data)) stop ("Study data has to be a data frame")
    ## If test, remove seqn to later bind it to the dataframe
    if (test) {
        seqn <- study_data$seqn # Get seqn as object
        seqn[is.na(seqn)] <- 999 # seqn = 999 should not be NA
        study_data$seqn <- NULL # Remove from study_data
    }
    ## Prepare study data using the data dictionary
    study_data[] <- lapply(names(study_data), function(n) {
        vdd <- data_dictionary[[n]] # Get variable specific data dictionary and assign that to vdd
        data <- study_data[, n]
        if (vdd$t == "qual") {
            values <- vdd$vs # Get values
            if (values != ""){
                split_labels <- split.labels(values) # Get values without quotation marks and split on commas
                value_labels <- split.labels(vdd$vls) # Get value labels
                value_label_list <- lapply(strsplit(value_labels, "="), trimws, "both") # Make a list of value labels without white space
                for (i in seq_along(value_label_list)) data[data == value_label_list[[i]][1]] <- value_label_list[[i]][2] # Assign new value labels
                data <- as.factor(data) # Make data a factor
            }
        }
        return(data)
    })
    ## If test is TRUE add seqn again
    if (test) study_data <- data.frame(study_data, seqn = seqn)
    return(study_data)
}

