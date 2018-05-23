#' Generate predictions with SuperLearner
#'
#' This function trains SuperLearner on the training set and makes predictions on the review set. Then, predictions are divided by quantiles into four colour-coded groups. The groups are green, yellow, orange, and red. They respectively include ranges from the 0% quantile to 25% quantile, 25% to 50%, 50% to 75%, and 75% to 100% of the continous predictions. (To be changed)
#' @param prepped_sample Data as prepared by prep.data.for.superlearner as list. No default.
#' @param models Models to use in ensemble algorithm. Default: SL.mean and SL.glmnet.
#' @param save_breaks Logical. If TRUE, save optimal breaks to results. Defaults to FALSE.
#' @param save_all_predictions Logical. If TRUE all predictions are saved in pred_data. Defaults to FALSE.
#' @param sample Logical. If TRUE the grid search will only search a random sample of possible cutpoint combinations, not all. Defaults to TRUE.
#' @param gridsearch_parallel Logical. If TRUE the gridsearch is performed in parallel. Defaults to FALSE.
#' @param n_cores Integer. The number of cores to run any parallel computing on. Default to NULL.
#' @param save_superlearner Logical. If TRUE the superlearner object will be saved to disk. Defaults to FALSE.
#' @param verbose Logical. If TRUE information to help gauge progress is printed. Defaults to TRUE.
#' @export
predictions.with.superlearner <- function(
                                          prepped_sample,
                                          models = c('SL.glmnet',
                                                     'SL.glm',
                                                     'SL.randomForest',
                                                     'SL.xgboost',
                                                     'SL.gam'),
                                          save_breaks = FALSE,
                                          save_all_predictions = FALSE,
                                          sample = TRUE,
                                          gridsearch_parallel = FALSE,
                                          n_cores = NULL,
                                          save_superlearner = FALSE,
                                          verbose = TRUE
                                          )
{
    ## Train algorithm with training set
    if (verbose) message("Training SuperLearner")
    train_algo <- with(prepped_sample, SuperLearner(Y = y_train,
                                                  X = x_train,
                                                  family = binomial(),
                                                  SL.library = models))
    if (save_superlearner) saveRDS(train_algo, "superlearner.rds")
    if (verbose) message("SuperLearner trained")
    ## Predict with algorithm on training set
    sets <- list(train = prepped_sample$x_train, test = prepped_sample$x_review)
    continuous_predictions <- lapply(sets, function(sample) {
        predict(train_algo,
                sample,
                onlySL = T)$pred
    })
    ## Subset continous predictions for categorisation
    train_pred <- continuous_predictions$train
    ## Do a grid search to find optimal cutpoints
    if (verbose) message("Finding optimal cutpoints")
    breaks <- gridsearch.breaks(predictions = train_pred,
                                outcomes = prepped_sample$y_train,
                                sample = sample,
                                parallel = gridsearch_parallel,
                                n_cores = n_cores)
    if (verbose) message("Optimal cutpoints identified")
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
    pred_data <- list(pred_con_test = continuous_predictions$test,
                      pred_cat_test = categorical_predictions$test,
                      tc = prepped_sample$tc,
                      outcome_test = prepped_sample$y_review)
    ## Save all predictions
    if (save_all_predictions) {
        pred_data$pred_con_train <- continuous_predictions$train
        pred_data$pred_cat_train <- categorical_predictions$train
        pred_data$outcome_train <- prepped_sample$y_train   
    }
    if (verbose) message("Returning prediction data \n")
    return (pred_data)
}
