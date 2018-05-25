#' Make learner weight, risk, and AUROCC table function
#'
#' This function generates a risk, learner weight and learner-AUROCC table.
#' @param study_sample The study_sample as list. No default.
#' @param superlearner_object_path The path to the SuperLearner object as generated from the SuperLearner::SuperLearner() method. Default: "./superlearner.rds"
#' @export
coefficients.table <- function(
                               study_sample,
                               superlearner_object_path = "./superlearner.rds"
                               )
{
    ## Courtesy of
    removeWords <- function(str, stopwords) {
        x <- unlist(strsplit(str, " "))
        paste(x[!x %in% stopwords], collapse = " ")
    }
    ## Load SuperLearner object
    superlearner_object <- readRDS(superlearner_object_path)
    ## Error handling
    if (!is.list(superlearner_object)) stop("Superlearner object is not a list.")
    ## Set learner names
    learner_names <- unlist(lapply(superlearner_object$SL.library$library$predAlgorithm,
                                   function(name) strsplit(name, ".", fixed = TRUE)))
    learner_names <- learner_names[!(learner_names %in% "SL")]
    ## Get predictons on training set from each model
    preds <- superlearner_object$library.predict
    l_of_predictions <- list()
    ## Data frame columns to list
    for (i in colnames(preds)){
        l_of_predictions[[i]] <- preds[, i]
    }
    ## Calculate AUC of SuperLearner learners
    auroccs <- lapply(l_of_predictions,
                      function (model){
                          pred <- ROCR::prediction(as.numeric(model),
                                                   study_sample[["outcome_train"]])
                          perf <- ROCR::performance(pred, measure = "auc", x.measure = "cutoff")@y.values
                          return(perf)
                      })
    ## Set table
    t_coeff_risk <- data.frame(Learner = learner_names,
                               Weight = superlearner_object$coef,
                               Risk = superlearner_object$cvRisk,
                               AUROCC = unlist(auroccs)
                               )
    ## Round columns
    t_coeff_risk[,-1] <- round(t_coeff_risk[,-1],
                               digits = 3)
    print(xtable(t_coeff_risk,
                 include.rownames = FALSE))
}

