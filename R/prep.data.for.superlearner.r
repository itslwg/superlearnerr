#' Prepare data function
#'
#' This function prepares and generates training and review sets by splitting the dataset in half according to a time variable . Factor variables are removed.
#' @param study_data The study data as a data frame. No default
#' @param outcome The outcome variable as a string. Default: "s30d".
#' @export
prep.data.for.superlearner <- function(
                                       study_data,
                                       outcome = "s30d"
                                       )
{
    split.dataset <- function(
                              dataset,
                              time_variable,
                              outcome
                              )
    {
        df <- dataset[order(dataset[,time_variable]),]
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
        return(sets)
    }
    set.sets <- function(
                        sets,
                        time_variable,
                        outcome
                        )
    {
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
    ## Split 
    time_variable <- "seqn" 
    datasets <- lapply(levels(as.factor(study_data$centre)), function(level) study_data[study_data$centre == level, ])
    sets_list <- lapply(datasets, function(dataset) {
        dataset$centre <- NULL
        split.dataset(dataset,
                      outcome = outcome,
                      time_variable = time_variable)
    })
    sets <- list()
    sets$x_train <- do.call(rbind, lapply(sets_list, function(set) set$x_train))
    sets$x_review <- do.call(rbind, lapply(sets_list, function(set) set$x_review))
    ## Set
    return_list <- set.sets(sets, time_variable, outcome)
    class(return_list) <- c(class(return_list), "prepped") # Add prepped to return list class
    return(return_list)
}
