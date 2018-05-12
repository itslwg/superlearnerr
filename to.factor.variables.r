#' Make variables to factor function
#'
#' This function transforms variables into factors.
#' @param study_data The study data as a data frame. No default
#' @param variables_to_factor Character vector of variables. Defaults to c("egcs", "mgcs", "vgcs", "avpu", "cmoi")
#' @export
to.factor.variables <- function(
                                study_data,
                                variables_to_factor = c("egcs",
                                                        "mgcs",
                                                        "vgcs",
                                                        "avpu",
                                                        "cmoi")
                                )
{
    ## Collapse mechanism of injury
    study_data <- collapse.moi(study_data)
    ## Test that study data if a data frame, stop if not
    if (!is.data.frame(study_data)) stop("study_data is not a data frame")
    ## Make sure all components are in the dataset
    if (!all(variables_to_factor %in% colnames(study_data))) stop("not all components are in the dataset")
    ## Make variables as factors
    study_data[, variables_to_factor] <- lapply(study_data[, variables_to_factor], as.factor)
    return(study_data)
}
