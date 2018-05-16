#' Add time between injury and arrival function
#'
#' Calculates the time between injury and arrival based on doi, toi, doar and toar
#' @param study_data The study data frame. No default.
#' @export
add.time.between.injury.and.arrival <- function(
                                                study_data,
                                                data_dictionary
                                                )
{
    ## Define required variables
    datetime_list <- list(doi_toi = c("doi", "toi"),
                          doar_toar = c("doar", "toar"))
    required_variables <- unlist(datetime_list)
    ## Error handling
    if (!is.data.frame(study_data)) stop("Study data has to be a data frame")
    if (!all(required_variables %in% colnames(study_data))) stop ("All required variables are not in study data")
    ## Modify date and time formats in data dictionary
    data_dictionary[required_variables] <- lapply(data_dictionary[required_variables], function(x) {
        x$f <- gsub("\"", "", x$f)
        return(x)
    })
    ## Reformat date and time data
    datetime_data <- lapply(datetime_list, function(dt) {
        date <- study_data[, dt[1]] # Get current date data
        time <- study_data[, dt[2]] # And time data
        date_format <- data_dictionary[[dt[1]]]$f # Get date format
        time_format <- data_dictionary[[dt[2]]]$f # And time format
        data <- as.POSIXct(strptime(paste(date, time), format = paste(date_format, time_format), tz = "Asia/Kolkata")) # Format as datetime objects
        return(data)
    })
    ## Calculate delay and put back in study data
    study_data$delay <- with(datetime_data, as.numeric(difftime(doar_toar, doi_toi, units = "mins")))
    ## Drop date and time variables from study data
    study_data <- study_data[, !(colnames(study_data) %in% required_variables)]
    return(study_data)
}
