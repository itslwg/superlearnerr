#' Prepare data function
#'
#' This function prepares and generates training and review sets by splitting the dataset in half according to date of arrival. Factor variables are removed.
#' @param study_data The study data as a data frame. No default
#' @param outcome The outcome variable as a string. Default: 's30d'.
#' @param factors_to_remove Character vector describing factor variables to remove from datasets. Default: c('mgcs','egcs','vgcs','avpu','cmoi')
#' @export
prep.data.for.superlearner <- function(
                                       study_data,
                                       outcome = 's30d',
                                       factors_to_remove = c('mgcs',
                                                             'egcs',
                                                             'vgcs',
                                                             'avpu',
                                                             'cmoi')
                                       )
{
    ## Order dataframe by date
    df <- study_data[order(study_data$doar),]
    ## Find midpoint of dates
    dates <- df$doar
    mid <- dates[1] + floor(tail(dates, n = 1) - dates[1])/2
    ## Create set for training the model
    train <- df[df$doar < mid, ]
    ## Create set for review of model performance
    review <- df[df$doar >= mid, ]
    ## Set sets
    x_sets <- list(x_train = train,
                   x_review = review)
    ## Dfs without outcome
    x_wo_outcome <- lapply(x_sets,
                           function(x) x[, !(names(x) %in% outcome)])
    ## Dfs without triage category, outcome and factor variables
    x_wo_tc_outcome_factors <- lapply(x_sets,
                              function(x) x[, !(names(x) %in% c('tc',
                                                                factors_to_remove,
                                                                outcome))])
    ## Extract outcome variables for training and review set
    y_training_and_review <- lapply(x_sets, function(x) x[, outcome])
    names(y_training_and_review) <- c('y_train', 'y_review')

    return (list(sets_wo_tc = x_wo_tc_outcome_factors,
                 sets_w_tc = x_wo_outcome,
                 outcome = y_training_and_review))
}
