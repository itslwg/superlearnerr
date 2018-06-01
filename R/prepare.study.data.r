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
    ## Remove seqn to later bind it to the dataframe
    seqn <- study_data$seqn # Get seqn as object
    seqn[is.na(seqn)] <- 999 # seqn = 999 should not be NA
    study_data$seqn <- NULL # Remove from study_data
    ## Prepare study data using the data dictionary
    study_data[] <- lapply(names(study_data), function(n) {
        vdd <- data_dictionary[[n]] # Get variable specific data dictionary and assign that to vdd
        data <- study_data[, n]
        if (vdd$t == "qual") {
            values <- vdd$vs # Get values
            if (values != ""){
                data <- as.factor(data)
                value_labels <- split.labels(vdd$vls) # Get value labels
                value_label_list <- lapply(strsplit(value_labels, "="), trimws, "both") # Make a list of value labels without white space
                labels <- unlist(lapply(value_label_list, function(x) x[2])) # Get labels
                levels(data) <- labels # Assign those to data
            }
        }
        return(data)
    })
    ## Add seqn again
    study_data <- data.frame(study_data, seqn = seqn)
    return(study_data)
}

