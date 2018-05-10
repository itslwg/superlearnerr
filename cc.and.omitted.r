#' Number of patients omitted and complete case dataset function
#'
#' This function counts the number of patients with missing data. Then, the patients with missing data are omitted from the study data to create a complete case dataset.
#' @param study_data The study data as a data frame. No default
#' @export
cc.and.omitted <- function (
                            study_data
                            )
{
    ## order the dataset according to data of arrival and s30d
    mdf <- study_data[order(-study_data$s30d, study_data$doar), ]
    ## identify complete cases
    cc <- mdf[complete.cases(mdf), ]
    ## create omitted object
    omitted <- nrow(mdf) - nrow(cc)

    return(list(study_data = cc,
                omitted = omitted))
}
