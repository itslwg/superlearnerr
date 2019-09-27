#' Partition Train And Predict
#'
#' Partitions the study sample, fits the model and makes predictions with SuperLearner.
#' @param study.sample Data frame. The study.sample. No default.
#' @param outcome.variable.name Character vector of length 1. The name of the outcome variable of interest. Defaults to "s30d".
#' @param n.partitions Numeric vector of length 1. The number of partitions to create with PartitionSample. Accepted values are 2 or 3. If 2, a train and test set is created. If 3, train, validation, and test sets are created - the models is fitted on the training set, optimal breaks is gridsearched on the validation set, and the model is tested on the test set. Defaults to 2. 
#' @param models.names Character vector. The model names to stack in SuperLearner. Defaults to c("SL.gam", "SL.randomForest", "SL.nnet, SL.xgboost", "SL.svm")
#' @param save.to.results Logical. If TRUE SuperLearner predictions, outcome and tc in each partition is saved to the Results list. Defaults to TRUE.
#' @param verbose Logical. If TRUE the modelling process is printed to console. Defaults to FALSE.
#' @export
PartitionTrainAndPredict <- function(study.sample,
                                     outcome.variable.name = "s30d",
                                     model.names = c("SL.randomForest"),
                                     n.partitions = 2,
                                     save.to.results = TRUE,
                                     verbose = FALSE){
    ## Error handling
    if (!is.data.frame(study.sample))
        stop ("data must be of type data frame")
    if (!bengaltiger::IsLength1(outcome.variable.name) | !is.character(outcome.variable.name))
        stop ("outome.variable.name must be of a character vector of length 1")     
    ## Partition the sample, and return the separate partitions, the corresponding outcome
    ## for both sets and tc in both sets
    n.partitions = 3
    partitions.outcome.and.tc <- PartitionSample(study.sample = study.sample,
                                                 outcome.variable.name = outcome.variable.name,
                                                 n.partitions = n.partitions)
    if (verbose)
        message("Fitting SuperLearner...")
    ## Fit the model to the training data
    fitted.sl <- with(partitions.outcome.and.tc, SuperLearner::SuperLearner(Y = train$y, X = train$x,
                                                                            family = binomial(),
                                                                            SL.library = model.names,
                                                                            method = "method.AUC",
                                                                            verbose = FALSE))
    ## Extract training sets
    training.and.validation.sublists <- partitions.outcome.and.tc[-grep("test", names(partitions.outcome.and.tc))]
    con.list.labels <- paste0("con.model.", names(training.and.validation.sublists))
    ## Make predictions on the validation set
    predictions <- lapply(setNames(training.and.validation.sublists, nm = con.list.labels),
                          function (partition.list) predict(object = fitted.sl,
                                                            newdata = partition.list$x,
                                                            onlySL = TRUE)$pred)
    label <- ifelse(n.partitions == 2, "train", "validation")
    if (verbose)
        message(paste("\nFinding Optimal breaks for continuous probabilities on the", label, "set..."))
    ## Gridsearch the optimal cut-points for the predicted probabilities on
    ## the appropriate set
    optimal.breaks <- GridsearchBreaks(predictions = predictions[grepl(label, con.list.labels)][[1]],
                                       outcome.vector = partitions.outcome.and.tc[[label]]$y)
    full.training.list <- list(y = unlist(lapply(training.and.validation.sublists, "[[", "y")),
                               x = do.call(rbind, lapply(training.and.validation.sublists, "[[", "x")))
    if (n.partitions == 3)
        ## Train the model once again. Now, on both the training and validation sets
        fitted.sl <- SuperLearner::SuperLearner(Y = full.training.list$y,
                                                X = full.training.list$x,
                                                family = binomial(),
                                                SL.library = model.names,
                                                method = "method.AUC",
                                                verbose = FALSE)
    ## Make predictions on the test set
    predictions$con.model.test <- predict(object = fitted.sl,
                                          newdata = partitions.outcome.and.tc$test$x,
                                          onlySL = TRUE)$pred
    ## Bin predictions made on the test set using the optimal cut-points
    cut.list.labels <- paste0("cut.model.", names(partitions.outcome.and.tc))
    binned.predictions <- lapply(setNames(predictions, nm = cut.list.labels),
                                 function (preds) as.numeric(cut(x = preds, breaks = c(-Inf, optimal.breaks, Inf),
                                                                 labels = c("Green", "Yellow", "Orange", "Red"),
                                                                 include.lowest = TRUE)))
    ## Adds suffixes to
    NewLabelsAndNumeric <- function(label) {
        new.labels <- paste0(label, ".", names(partitions.outcome.and.tc))
        new.list <- lapply(setNames(partitions.outcome.and.tc, nm = new.labels),
                           function (partition.list) as.numeric(partition.list[[label]]))
        return (new.list)
    }
    return.object <- c(predictions,
                       binned.predictions,
                       NewLabelsAndNumeric("y"),
                       NewLabelsAndNumeric("tc"))
    ## Save the predictions, outcome and clinicians tc in each partition to the results list
    if (save.to.results)
        bengaltiger::SaveToResults(output.object = return.object, object.name = "predictions.outcome.and.tc")
    return (return.object)
}
