#' Prepare data function
#'
#' This function prepares and generates training and review sets by splitting the dataset in half according to a time variable . Factor variables are removed.
#' @param study_data The study data as a data frame. No default
#' @param outcome The outcome variable as a string. Default: "s30d".
#' @param test Logical. If TRUE, seqn is used to split data in training and review set. Defaults to FALSE.
#' @export
prep.data.for.superlearner <- function(
                                       study_data,
                                       outcome = "s30d",
                                       test = FALSE
                                       )
{
    split_and_set <- function(
                              study_data,
                              time_variable,
                              outcome
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
        ## Use dates function to split dataset by seqn or doar depending on the
        ## main function argument test
        sets <- list(x_train = train,
                     x_review = review)
        ## Extract tc from review set
        tc <- as.numeric(sets$x_review$tc)
        ## Extract outcome from training and review set
        y_training_and_review <- lapply(sets,
                                        function(x)
                                            as.numeric(x[, outcome]))
        names(y_training_and_review) <- c("y_train", "y_review")
        ## Remove tc, outcome and time_variable from sets
        x_sets <- lapply(sets,
                         function (the_set) the_set[, !(names(the_set) %in% c(outcome,
                                                                              "tc",
                                                                              time_variable))])
        ## Do median imputation on training and review set separately
        x_sets <- lapply(x_sets, do.median.imputation)

        return (list(sets = x_sets,
                     tc = as.numeric(tc),
                     y_train = y_training_and_review$y_train,
                     y_review = y_training_and_review$y_review,
                     y_name = outcome,
                     y_levels = levels(study_data[, outcome])))
    }
    ## Split and set
    time_variable <- "doar" # Define time variable
    if (test) time_variable <- "seqn" # Change if test is TRUE
    return_list <- split_and_set(study_data,
                                 outcome = outcome,
                                 time_variable = time_variable)
    class(return_list) <- c(class(return_list), "prepped") # Add prepped to return list class
    return(return_list)
}
