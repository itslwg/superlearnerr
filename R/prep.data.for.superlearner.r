#' Prepare data function
#'
#' This function prepares and generates training and review sets by splitting the dataset in half according to date of arrival. Factor variables are removed.
#' @param study_data The study data as a data frame. No default
#' @param outcome The outcome variable as a string. Default: 's30d'.
#' @param factors_to_remove Character vector describing factor variables to remove from datasets. Default: c('mgcs','egcs','vgcs','avpu','cmoi')
#' @export
prep.data.for.superlearner <- function(
                                       study_data,
                                       outcome = 's30dYes',
                                       test = FALSE
                                       )
{
    dates <- function(
                      time_variable
                      )
    {
            df <- study_data[order(study_data[,time_variable]),]
            ## Find midpoint of dates
            dates <- df[,time_variable]
            mid <- dates[1] + floor(tail(dates, n = 1) - dates[1])/2
            ## Create set for training the model
            train <- df[dates < mid, ]
            ## Create set for review of model performance
            review <- df[dates >= mid, ]

            return(list(train = train,
                        review = review))
    }
    if (test == TRUE) sets <- dates("seqn")
    if (test == FALSE) sets <- dates("doar")
    ## Set sets
    x_sets <- list(x_train = sets$train,
                   x_review = sets$review)
    ## Dfs without outcome
    x_wo_outcome <- lapply(x_sets,
                           function(x) x[, !(names(x) %in% outcome)])
    ## Dfs without triage category, outcome and factor variables
    x_wo_tc_outcome <- lapply(x_sets,
                              function(x) x[, !(names(x) %in% c(grep("tc", names(x)),
                                                                outcome))])
    ## Extract outcome variables for training and review set
    y_training_and_review <- lapply(x_sets, function(x) x[, outcome])
    names(y_training_and_review) <- c('y_train', 'y_review')

    return (list(sets_wo_tc = x_wo_tc_outcome,
                 sets_w_tc = x_wo_outcome,
                 outcome = y_training_and_review))
}
