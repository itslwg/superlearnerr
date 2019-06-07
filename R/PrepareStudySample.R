#' PrepareStudySample
#'
#' Prepares the study data using the data dictionary.
#' @param study.sample The study data frame. No default.
#' @param data.dictionary The data dictionary object. No default.
#' @export
PrepareStudySample <- function(study.sample, data.dictionary) {
    ## Error handling
    if (!is.data.frame(study.data)) stop ("Study sample has to be type data frame.")
    ## If seqn in dataset, remove seqn to later bind it to the dataframe
    seqn_in_data <- FALSE
    if ("seqn" %in% names(study.sample)) {
        seqn <- study.sample$seqn # Get seqn as object
        seqn[is.na(seqn)] <- 999 # seqn = 999 should not be NA
        study.sample$seqn <- NULL # Remove from study.sample
        seqn_in_data <- TRUE
    }
    ## Prepare study data using the data dictionary
    study.sample[] <- lapply(names(study.sample), function(n) {
        vdd <- data_dictionary[[n]] # Get variable specific data dictionary and assign that to vdd
        data <- study.sample[, n]
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
    ## If seqn is in study data, add seqn again
    if (seqn_in_data) study.sample <- data.frame(study.sample, seqn = seqn)
    return(study.sample)
}

