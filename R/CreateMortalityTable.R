#' CreateMortalityTable
#'
#' Table reporting number (%) of patients that died in each triage category.
#' @param predictions.outcome.and.tc List. The predictions, outcome and clinicians triage in each partition. No default.
#' @param digits Numeric vector of length 1. Number of digits to use in table. Defaults to 2.
#' @param scores.to.invert Characer vector. Model scores to invert. Defaults to NULL.
#' @export
CreateMortalityTable <- function(predictions.outcome.and.tc,
                                 digits = 2, model.labels = NULL,
                                 outcome.label = "s30d", pretty.names = NULL,
                                 scores.to.invert = NULL, save.to.disk = FALSE) {
    if (!is.null(scores.to.invert)) {
        Inversion <- function(score) {
            score.as.factor <- as.factor(score)
            new.score <- forcats::fct_rev(score.as.factor)
            return (as.numeric(new.score))
        }
        for (score.label in scores.to.invert)
            predictions.outcome.and.tc[[score.label]] <- Inversion(predictions.outcome.and.tc[[score.label]])
    }
    if (is.null(model.labels))
        model.labels <- names(predictions.outcome.and.tc)[!(names(predictions.outcome.and.tc) %in% "s30d")]
    ## Error handling
    tbl <- as.data.frame(do.call(cbind, lapply(model.labels, function(model.label) {
        freq.tbl <- table(predictions.outcome.and.tc[[model.label]],
                          predictions.outcome.and.tc[[outcome.label]])
        prop <- paste0("(", round(prop.table(as.matrix(freq.tbl), 1) * 100, digits)[, 2], ")")
        mortality.tbl <- paste(freq.tbl[, 2], prop)
        return (mortality.tbl)
    })))
    rownames(tbl) <- c("Green", "Yellow", "Orange", "Red")
    if (!is.null(pretty.names)) {
        colnames(tbl) <- pretty.names   
    } else {
        colnames(tbl) <- model.labels
    }
    if (save.to.disk)
        MakeTable(as.data.frame(tbl), "mortality.table")
    return (tbl)
}
