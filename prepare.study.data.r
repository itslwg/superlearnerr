#' Prepare study data function
#'
#' Prepares the study data using the data dictionary
#' @param study_data The study data frame. No default.
#' @param data_dictionary The data dictionary object. No default.
#' @export
prepare.study.data <- function(
                               study_data,
                               data_dictionary
                               )
{
    ## Error handling
    if (!is.data.frame(study_data)) stop ("Study data has to be a data frame")
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
    return(study_data)
}    

