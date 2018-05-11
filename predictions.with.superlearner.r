#' Generate predictions with SuperLearner
#'
#' This function trains SuperLearner on the training set and makes predictions on the review set. Then, predictions are divided by quantiles into four colour-coded groups. The groups are green, yellow, orange, and red. They respectively include ranges from the 0% quantile to 25% quantile, 25% to 50%, 50% to 75%, and 75% to 100% of the continous predictions.
#' @param study_sample. The study_sample as dataframe No default
#' @param outcome The outcome variable as a string. Default: 's30d'.
#' @param models Models to include in SuperLearner. Default: SL.mean and SL.glmnet.
#' @export
predictions.with.superlearner <- function(
                                          study_sample,
                                          models = c('SL.mean', 'SL.glmnet')
                                          )
{
    ## Retrive training and review data as well as outcome for training and review
    data <- prep.data.for.superlearner(study_sample)
    ## Train algorithm with training set
    train_algo <- SuperLearner(Y = data$outcome$y_train,
                               X = data$sets_wo_tc$x_train,
                               family = binomial(),
                               SL.library = models)
    ## Predict with algorithm on review set
    predictions <- predict(train_algo,
                           data$sets_wo_tc$x_review,
                           onlySL = T)
    ## Subset continous predictions for categorisation
    pred <- predictions$pred
    ## Get quantiles of predictions
    quantiles <- quantile(predictions$pred, probs = c(0.25, 0.50, 0.75))
    ## Use those to categorise predictions
    labels <- c('green', 'yellow', 'orange', 'red') # define labels
    pred_cat <- cut(pred,
                    breaks = c(0, quantiles, 1),
                    labels = labels,
                    include.lowest = TRUE)
    ## Return data with predictions and study data
    pred_data <- list(clinicians_predictions = data$sets_w_tc$x_review$tc,
                      pred_con = pred,
                      pred_cat = pred_cat,
                      outcome_review = data$outcome$y_review)

    return (list(preds = pred_data,
                 data = data))
}
