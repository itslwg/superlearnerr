#' Partition Train And Predict
#'
#' Partitions the study sample, fits the model and makes predictions with SuperLearner.
#' @param study.sample Data frame. The study.sample. No default.
#' @param outcome.variable.name Character vector of length 1. The name of the outcome variable of interest. Defaults to "s30d".
#' @param save.to.results Logical. If TRUE SuperLearner predictions, outcome and tc in each partition is saved to the Results list. Defaults to TRUE.
#' @param models.names Character vector. The model names to stack in SuperLearner. Defaults to c("SL.gam", "SL.randomForest", "SL.nnet, SL.xgboost", "SL.svm") 
#' @export
PartitionTrainAndPredict <- function(study.sample, outcome.variable.name = "s30d",
                                     save.to.results = TRUE, model.names = c("SL.randomForest")){
    ## Error handling
    if (!is.data.frame(study.sample))
        stop ("data must be of type data frame")
    ## Partition the sample, and return the separate partitions, the corresponding outcome
    ## for both sets and tc in both sets
    partitions.outcome.and.tc <- PartitionSample(study.sample = study.sample,
                                                 outcome.variable.name = outcome.variable.name,
                                                 p = 0.75, groups = 2, list = FALSE)
    message("Fitting SuperLearner...")
    ## Fit the model to the training data
    fitted.sl <- with(partitions.outcome.and.tc, SuperLearner::SuperLearner(Y = train$y, X = train$x,
                                                                            family = binomial(),
                                                                            SL.library = model.names,
                                                                            method = "method.AUC",
                                                                            verbose = FALSE))
    con.list.labels <- paste0("con.model.", names(partitions.outcome.and.tc))
    ## Make predictions on both the train and test partitions
    predictions <- lapply(setNames(partitions.outcome.and.tc, nm = con.list.labels),
                          function (partition.list) predict(object = fitted.sl,
                                                            newdata = partition.list$x,
                                                            onlySL = TRUE)$pred)
    message("Finding Optimal breaks for continuous probabilities...")
    ## Gridsearch the optimal cut-points for the predicted probabilities on the training set 
    optimal.breaks <- GridsearchBreaks(predictions = predictions$con.model.train,
                                       outcome.vector = partitions.outcome.and.tc$train$y)
    ## Bin predictions made on the test set
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
