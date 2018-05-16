#' Change outcome function
#'
#' This function changes patient outcome data if certain conditions are met. The outcome variable is set to dead if patients were dead on discharge after 30 days or if dead after 24 hours. Moreover, s30d is set to alive if coded alive and admitted to other hospital.
#' @param study_data The study data as a data frame. No default
#' @export
set.to.outcome <- function(
                           study_data
                           )
{
    ## Test that study data if a data frame, stop if not
    if (!is.data.frame(study_data)) stop("study_data is not a data frame")
    ## Set s30d to dead if we know that patients were dead on discharge or at 24
    ## hours
    study_data[study_data$hd == 1 & !is.na(study_data$hd) | study_data$s24h == 1 & !is.na(study_data$s24h), "s30d"] <- "Yes"
    ## Finally remove s24h and hd from the dataset
    study_data <- study_data[, -grep("s24h|hd", colnames(study_data))]
    return (study_data)
}
