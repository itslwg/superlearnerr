#' Partition Train And Predict
#'
#' Partitions the study sample, fits the model and makes predictions with SuperLearner.
#' @param study.sample Data frame. The study.sample. No default.
#' @export
PartitionTrainAndPredict <- function(study.sample){
    ## Error handling
    if (!is.data.frame(study.sample))
        stop ("data must be of type data frame")
    ## Partition the sample, and return the separate partitions, the corresponding outcome
    ## for both sets and tc in both sets
    partitions.outcome.and.tc <- PartitionSample(study.sample = d,
                                                 outcome.variable.name = outcome.variable.name,
                                                 p = 0.75, groups = 2, list = FALSE)
    message("Fitting SuperLearner...")
    ## Fit the model to the training data
    fitted.sl <- with(partitions.outcome.and.tc, SuperLearner::SuperLearner(Y = train$y, X = train$x,
                                                                            family = binomial(),
                                                                            SL.library = model.names,
                                                                            method = "method.AUC", verbose = FALSE))
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
                                 function (preds)
                                     as.numeric(cut(x = preds, breaks = c(-Inf, optimal.breaks, Inf),
                                                    labels = c("Green", "Yellow", "Orange", "Red"),
                                                    include.lowest = TRUE)))
    NewLabels <- function(prefix) paste0(prefix, names(partitions.outcome.and.tc))
    return.object <- c(predictions, binned.predictions,
                       lapply(setNames(partitions.outcome.and.tc, nm = NewLabels("tc.")),
                              with, expr = as.numeric(tc)),
                       lapply(setNames(partitions.outcome.and.tc, nm = NewLabels("outcome.")),
                              with, expr = y))

    return (return.object)
}
