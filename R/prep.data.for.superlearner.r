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
        df <- study_data[order(study_data[,time_variable]),]
        ## Get 3/4 of patients
        top_split <- floor(nrow(df)*0.75)
        ## Create set for training
        train <- df[1:top_split,]
        ## Create set for review
        review <- df[!(row.names(df) %in% row.names(train)),]
        ## Use dates function to split dataset by seqn or doar depending on the
        ## main function argument test
        sets <- list(x_train = train,
                     x_review = review)
        ## Extract tc from review set
        tc <- sets$x_review$tc
        ## Change levels of outcome factor from "No" and "Yes", to 0 and 1. Then
        ## outcome is extracted from the sets
        y_training_and_review <- lapply(sets,
                                        function(the_set)
                                        {
                                            levels(the_set[[outcome]]) <- c(0,1)
                                            the_set[[outcome]] <- as.numeric(as.character(the_set[[outcome]]))
                                            return(the_set[[outcome]])
                                        })
        names(y_training_and_review) <- c("y_train", "y_review")
        ## Remove tc, outcome and time_variable from sets
        x_sets <- lapply(sets,
                         function (the_set) the_set[, !(names(the_set) %in% c(outcome,
                                                                              "tc",
                                                                              time_variable))])
        ## Do median imputation on training and review set separately
        x_sets <- lapply(x_sets, do.median.imputation)
        return (list(sets = x_sets,
                     tc = tc,
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
