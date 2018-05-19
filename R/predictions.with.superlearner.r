#' Generate predictions with SuperLearner
#'
#' This function trains SuperLearner on the training set and makes predictions on the review set. Then, predictions are divided by quantiles into four colour-coded groups. The groups are green, yellow, orange, and red. They respectively include ranges from the 0% quantile to 25% quantile, 25% to 50%, 50% to 75%, and 75% to 100% of the continous predictions. (To be changed)
#' @param prepped_data. Data as prepared by prep.data.for.superlearner as list. No default.
#' @param models Models to use in ensemble algorithm. Default: SL.mean and SL.glmnet.
#' @param save_breaks Logical. If TRUE, save optimal breaks to results. Defaults to FALSE.
#' @param save_to_results Logical. If TRUE stuff is saved to results in parent environment. Defaults to FALSE.
#' @export
predictions.with.superlearner <- function(
                                          prepped_data,
                                          models = c('SL.glmnet',
                                                     'SL.glm',
                                                     'SL.randomForest',
                                                     'SL.xgboost'),
                                          save_breaks = FALSE,
                                          save_to_results = FALSE
                                          )
{
    ## Train algorithm with training set
    train_algo <- with(prepped_data, SuperLearner(Y = y_train,
                                                  X = x_train,
                                                  family = binomial(),
                                                  SL.library = models))
    ## Predict with algorithm on training set
    sets <- list(train = prepped_data$x_train, test = prepped_data$x_review)
    continuous_predictions <- lapply(sets, function(sample) {
        predict(train_algo,
                sample,
                onlySL = T)$pred
    })
    ## Subset continous predictions for categorisation
    train_pred <- continuous_predictions$train
    ## Do a grid search to find optimal cutpoints
    breaks <- gridsearch.breaks(predictions = train_pred, outcomes = prepped_data$y_train)
    ## Save breaks
    if (save_breaks) results$optimal_breaks <<- breaks
    ## Use those to categorise predictions
    labels <- c('Green', 'Yellow', 'Orange', 'Red') # define labels
    categorical_predictions <- lapply(continuous_predictions, function(predictions) {
        cut(predictions,
            breaks = c(-Inf, breaks, Inf),
            labels = labels,
            include.lowest = TRUE)
    })
    ## Return data with predictions
    pred_data <- list(pred_con = continuous_predictions$test,
                      pred_cat = categorical_predictions$test,
                      tc = prepped_data$tc,
                      outcome = prepped_data$y_review)
    ## Save to results
    if (save_to_results) {
        ## Generate list of predictions
        predictions <- list(continuous = continuous_predictions$train,
                            categorical = categorical_predictions$train)
        ## Get prediction objects
        prediction_objects <- lapply(predictions, function(prediction) {
            ROCR::prediction(as.numeric(prediction), prepped_data$y_train)
        })
        ## Calculate aucs
        aucs <- lapply(prediction_objects, function(prediction_object) {
            ROCR::performance(prediction_object, "auc")@y.values[[1]]
        })
        ## Get roc objects
        rocs <- lapply(prediction_objects, function(prediction_object) {
            ROCR::performance(prediction_object, "tpr", "fpr")
        })
        ## Save relevant data to results
        results$continuous_superlearner_auc_point_estimate_train <<- aucs$continuous # Superlearner point estimate in training data
        results$categorical_superlearner_auc_point_estimate_train <<- aucs$categorical # Superlearner point estimate in training data
        results$continuous_superlearner_roc_curve_data_train <<- rocs$continuous # Will be used to create a roc curve
        results$categorical_superlearner_roc_curve_data_train <<- rocs$categorical # Will be used to create a roc curve
        results$superlearner_calibration_plot_data_train <<- list(predictions = continuous_predictions$train, outcomes = prepped_data$y_train) # Will be used to create a calibration plot
    }
    return (pred_data)
}
