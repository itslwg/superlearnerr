#' Generate predictions with SuperLearner
#'
#' This function trains SuperLearner on the training set and makes predictions on the review set. Then, predictions are divided by quantiles into four colour-coded groups. The groups are green, yellow, orange, and red. They respectively include ranges from the 0% quantile to 25% quantile, 25% to 50%, 50% to 75%, and 75% to 100% of the continous predictions.
#' @param prepped_data. The prepared data for SuperLearner predictions as dataframe No default
#' @param models Models to include in SuperLearner. Default: SL.mean and SL.glmnet.
#' @export
predictions.with.superlearner <- function(
                                          prepped_data,
                                          models = c('SL.glmnet')
                                          )
{
    ## Train algorithm with training set
    train_algo <- SuperLearner(Y = prepped_data$y_train,
                               X = prepped_data$x_train,
                               family = binomial(),
                               SL.library = models)
    ## Predict with algorithm on review set
    predictions <- predict(train_algo,
                           prepped_data$x_review,
                           onlySL = T)
    ## Subset continous predictions for categorisation
    pred <- predictions$pred
    ## Get quantiles of predictions
    quantiles <- quantile(pred, probs = c(0.25, 0.50, 0.75))
    ## Use those to categorise predictions
    labels <- c('green', 'yellow', 'orange', 'red') # define labels
    pred_cat <- cut(pred,
                    breaks = c(0, quantiles, 1),
                    labels = labels,
                    include.lowest = TRUE)
    ## Return data with predictions and study data
    pred_data <- list(pred_con = pred,
                      pred_cat = pred_cat,
                      tc = prepped_data$tc,
                      outcome = prepped_data$y_review)

    return (pred_data)
}
