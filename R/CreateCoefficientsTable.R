#' Make learner weight, risk, and AUROCC table function
#'
#' This function generates a risk, learner weight and learner-AUROCC table.
#' @param prepped.sample The prepped.sample list. No default.
#' @param superlearner.object.path The path to the SuperLearner object as generated from the SuperLearner::SuperLearner() method. Default: "./superlearner.rds"
#' @export
CreateCoefficientsTable <- function(prepped.sample, table.name = "coeff.risk"
                                    superlearner.object.path = "./SuperLearner.rds",
                                    caption = "10 fold cross validated risk and area under
                                               the receiver operating curve characteristics
                                               (AUROCC) in the training sample, weight, and
                                               AUROCC in the test sample for SuperLearner and
                                               each included learner",
                                    pretty.colnames = c("AUROCC (95\\% CI)",
                                                        "Model-Clinicians AUROCC difference (95\\% CI)",
                                                        paste0("Model-Model",
                                                               kableExtra::footnote_marker_symbol(1, "latex"),
                                                               "AUROCC difference (95\\% CI)")),
                                    pretty.rownames = c("SuperLearner", "GLMnet", "GLM",
                                                        "Random Forest", "XGboost", "GAM"),
                                    footnote = "The risk is equal to 1 - cross validated AUROCC.
                                                The weight is the weight given to each learner in
                                                the final SuperLearner. A weight of 0 means that the
                                                learner was not included. Abbreviations: GAM Generalized
                                                Additive Model, GLM Generalized Linear Model, XGboost
                                                Extreme Gradient Boosting Machine.",
                                    return.table = FALSE, save.to.disk = TRUE, file.format = "pdf", ...) {
    ## Load SuperLearner object
    superlearner.object <- readRDS(superlearner.object.path)
    ## Error handling
    if (!is.list(superlearner.object))
        stop("Superlearner object is not a list.")
    ## Set learner names
    learner.names <- unlist(lapply(superlearner.object$SL.library$library$predAlgorithm,
                                   function(name) strsplit(name, ".", fixed = TRUE)))
    learner.names <- learner.names[!(learner.names %in% "SL")]
    ## Get predictons on training set from each model
    preds <- do.call(cbind, predict(superlearner.object, newdata = prepped.sample$x.review))
    colnames(preds) <- pretty.names
    ## Initiate list and save preds columns, i.e. model predictions, to list
    l.of.predictions <- lapply(setNames(nm = colnames(preds)), function(nm) preds[, nm])
    ## Calculate AUC of learners
    auroccs <- lapply(l.of.predictions, function(predictions) EvaluateWithRocr(predictions, prepped.sample$y.test))
    ## Set table
    t.coeff.risk <- data.frame(Learner = pretty.names,
                               cvRisk = c("Not applicable", superlearner.object$cvRisk),
                               cvAUROCC = 1 - c("Not applicable", superlearner.object$cvRisk),
                               Weight = c("Not applicable", superlearner.object$coef),
                               AUROCC = unlist(auroccs),
                               stringsAsFactors = FALSE)
    ## Round columns
    t.coeff.risk[] <- lapply(t.coeff.risk, function(x) if(!is.character(x)) sprintf("%.3f", x) else x)
    colnames(t.coeff.risk) <- c("Learner", "Cross validated risk", "Cross validated AUROCC", "Weight", "AUROCC in test sample")
    MakeTable(t.coeff.risk,
              table.name = table.name,
              save.to.disk = save.to.disk,
              file.format = file.format,
              return.table = return.table
              footnote = footnote, ...)
    if (return.table)
        return (t.coeff.risk)
}

#' RemoveWords
#'
#' Remove
#' @param str Character vector of length 1. The string in which words are to be removed. No default.
#' @param stopwords Character vector of length 1 or list of character vectors. The words to remove in str. No default.
RemoveWords <- function(str, stopwords) {
    ## User Mikkos answer at
    ## https://stackoverflow.com/questions/35790652/removing-words-featured-in-character-vector-from-string
    x <- unlist(strsplit(str, " "))
    paste(x[!x %in% stopwords], collapse = " ")
}
