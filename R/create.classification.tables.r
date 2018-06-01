#' Create classification tables function
#'
#' Function to create classification tables
#' @param study_sample The study sample object. No default.
#' @export
create.classification.tables <- function(
                                         study_sample
                                         )
{
    ## Error handling
    if (!is.list(study_sample)) stop("Study sample has to be a list")
    ## Make list to hold tables
    table_list <- list()
    ## Make simple table function
    make.simple.table <- function(outcome, priorities, caption, label)
    {
        simple_table <- cbind(table(outcome, priorities), table(outcome))
        colnames <- colnames(simple_table)
        colnames[5] <- "Overall"
        simple_table <- matrix(paste0(simple_table, " (", round(prop.table(simple_table, margin = 2) * 100), ")"), nrow = 2, dimnames = list(NULL, paste0(colnames, " (\\%)")))
        simple_table <- cbind(c("No", "Yes"), simple_table)
        colnames(simple_table)[1] <- "All cause 30-day mortality"
        simple_table <- print.xtable(xtable(simple_table,
                                           caption = paste0("\\bf ", caption),
                                           label = label),
                                     table.placement = "!ht",
                                     include.rownames = FALSE,
                                     sanitize.text.function = function(x) x,
                                     print.results = FALSE,
                                     caption.placement = "top")
        return(simple_table)
    }
    ## Reclassification table function
    make.reclassification.table <- function(superlearner_priorities, clinicians_priorities, caption, label)
    {
        reclass_table <- table(clinicians_priorities, superlearner_priorities)
        mat <- as.matrix(reclass_table)
        reclass <- sapply(1:nrow(mat), function(i) round((1 - mat[i, i]/sum(mat[i, ])) * 100))
        reclass_up <- sapply(1:nrow(mat), function(i) {
            if(i < ncol(mat)) round(sum(mat[i, (i + 1):ncol(mat)]) / sum(mat[i, ]) * 100)
            else "NA"
        })
        reclass_down <- sapply(1:nrow(mat), function(i) {
            if(i > 1) round(sum(mat[i, 1:(i - 1)]) / sum(mat[i, ]) * 100)
            else "NA"
        })
        reclass_table <- cbind(reclass_table, reclass, reclass_up, reclass_down)
        reclass_table <- cbind(rownames(reclass_table), reclass_table)
        rownames(reclass_table) <- NULL
        reclass_table[reclass_table == NaN | reclass_table == "NA"] <- ""
        reclass_xtable <- xtable(reclass_table,
                                 caption = paste0("\\bf ", caption),,
                                 label = label)
        addtorow <- list()
        addtorow$pos <- list(0, 0)
        addtorow$command <- c("& \\multicolumn{4}{c}{SuperLearner} \\\\\n",
                              "Clinicians & Green & Yellow & Orange & Red & Rec. \\% & Rec. up \\% & Rec. down \\% \\\\\n")
        reclass_xtable <- print.xtable(reclass_xtable,
                                       add.to.row = addtorow,
                                       include.rownames = FALSE,
                                       include.colnames = FALSE,
                                       print.results = FALSE,
                                       caption.placement = "top",
                                       table.placement = "!ht")
        star_caption <- "Reclassification (Rec.) figures refer to \\% of patients reclassified by the SuperLearner compared to clinicians. Rec. up and Rec. down indicates \\% of patients reclassified to a higher or lower priority level respectively."
        reclass_xtable <- add.star.caption(reclass_xtable, star_caption)
        return(reclass_xtable)
    }
    ## Settings for classification tables
    settings_classification_tables <- list(superlearner_train_table = list(outcome = study_sample$outcome_train,
                                                                          priorities = study_sample$pred_cat_train,
                                                                          caption = paste0("Priority levels assigned by the binned SuperLearner prediction in the training sample (n = ", length(study_sample$outcome_train), ")"),
                                                                          label = "tab:superlearner_priorities_train"),
                                          superlearner_test_table = list(outcome = study_sample$outcome_test,
                                                                         priorities = study_sample$pred_cat_test,
                                                                         caption = paste0("Priority levels assigned by the binned SuperLearner prediction in the test sample (n = ", length(study_sample$outcome_test), ")"),
                                                                         label = "tab:superlearner_priorities_test"),
                                          clinicians_test_table = list(outcome = study_sample$outcome_test,
                                                                       priorities = study_sample$tc,
                                                                       caption = paste0("Priority levels assigned by clinicians in the test sample (n = ", length(study_sample$outcome_test), ")"),
                                                                       label = "tab:clinicians_priorities_test"))
    ## Settings for reclassification tables
    settings_reclassification_tables <- list(reclass_all = list(superlearner_priorities = study_sample$pred_cat_test,
                                                                clinicians_priorities = study_sample$tc,
                                                                caption = paste0("Priority levels assigned by SuperLearner and clinicians in complete test sample (n = ", length(study_sample$tc), ")"),
                                                                label = "tab:reclass_all"),
                                             reclass_events = list(superlearner_priorities = with(study_sample, pred_cat_test[outcome_test == 1]),
                                                                   clinicians_priorities = with(study_sample, tc[outcome_test == 1]),
                                                                   caption = paste0("Priority levels assigned by SuperLearner and clinicians in test sample events (n = ", length(with(study_sample, tc[outcome_test == 1])), ")"),
                                                                   label = "tab:reclass_events"),
                                             reclass_nonevents = list(superlearner_priorities = with(study_sample, pred_cat_test[outcome_test == 0]),
                                                                      clinicians_priorities = with(study_sample, tc[outcome_test == 0]),
                                                                      caption = paste0("Priority levels assigned by SuperLearner and clinicians in test sample non-events (n = ", length(with(study_sample, tc[outcome_test == 0])), ")"),
                                                                      label = "tab:reclass_nonevents"))
    ## Make classification tables
    class_tables <- lapply(settings_classification_tables, function(setting) {
        with(setting, make.simple.table(outcome, priorities, caption, label))
    })
    ## Make reclassification tables
    reclass_tables <- lapply(settings_reclassification_tables, function(setting) {
        with(setting, make.reclassification.table(superlearner_priorities,
                                                  clinicians_priorities,
                                                  caption,
                                                  label))
    })
    ## Put them in the same list
    table_list <- c(class_tables, reclass_tables)
    return(table_list)
}
