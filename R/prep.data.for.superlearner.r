#' Prepare data function
#'
#' This function prepares and generates training and review sets by splitting the dataset in half according to a time variable . Factor variables are removed.
#' @param study_data The study data as a data frame. No default
#' @param outcome The outcome variable as a string. Default: 's30d'.
#' @param test Logical. If TRUE, seqn is used to split data in training and review set. Defaults to FALSE.
#' @export
prep.data.for.superlearner <- function(
                                       study_data,
                                       outcome = 's30d',
                                       test = FALSE
                                       )
{
    dates <- function(
                      study_data,
                      time_variable
                      )
    {
        df <- study_data[order(study_data[, time_variable]),]
        ## Find midpoint of dates
        dates <- df[, time_variable]
        mid <- dates[1] + floor(tail(dates, n = 1) - dates[1])/2
        ## Create set for training the model
        train <- df[dates < mid, ]
        ## Create set for review of model performance
        review <- df[dates >= mid, ]

        return(list(x_train = train,
                    x_review = review))
    }
    if (test == TRUE) sets <- dates(study_data, time_variable = "seqn")
    if (test == FALSE) sets <- dates(study_data, time_variable = "doar")
    ## Extract tc from review set
    tc <- as.numeric(sets$x_review$tc)
    y_training_and_review <- lapply(sets,
                                    function(x)
                                        as.numeric(x[, outcome]));names(y_training_and_review) <- c("y_train", "y_review")
    ## Remove tc and outcome from sets
    x_sets <- lapply(sets,
                     function (the_set) the_set[, !(names(the_set) %in% c(outcome,
                                                                          "tc",
                                                                          "seqn"))])
    ## Extract outcome variables for training and review set
    return (list(sets = x_sets,
                 tc = as.numeric(tc),
                 y_train = y_training_and_review$y_train,
                 y_review = y_training_and_review$y_review))
}
