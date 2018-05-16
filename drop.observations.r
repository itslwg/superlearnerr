#' Drop observations function
#'
#' This function takes as only input the study data data frame and drops
#' (removes) observations collected before all centres started collecting triage
#' category data and observations collected later then one month prior to
#' creating the study dataset
#' @param study_data The study data as a data frame. No default
#' @param before_date Patients enrolled before this date are excluded. Defaults to "2016-07-28"
#' @param after_date Patients enrolled after this data are excluded. Defaults to "2018-04-07"
#' @param test Logical. If TRUE, then all observations are kept and before_date and after_date are ignored. Defaults to FALSE
#' @export
drop.observations <- function(
                              study_data,
                              before_date = "2016-07-28",
                              after_date = "2018-04-07",
                              test = FALSE
                              )
{
    ## Test that study data is a data frame, stop if not
    if (!is.data.frame(study_data)) stop("study_data is not a data frame")
    ## Continue if test is FALSE else return study_data as provided
    if (!test) {
        ## Make date of arrival a date vector instead of character
        study_data$doar <- as.Date(study_data$doar)
        ## Drop observations collected before all centres started collecting triage
        ## category data
        study_data <- study_data[study_data$doar >= before_date, ]
        ## Drop observations included later than one month prior to creating this dataset
        study_data <- study_data[study_data$doar <= after_date, ]
    } 
    return (study_data)
}
