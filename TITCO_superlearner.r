## * Make study
#' Make study function
#'
#' This is the main study function and runs the entire study
#' @param data_path Path to data set. Should be a character vector of length 1. Defaults to c("../data/mdf.csv")
#' @export

make.study <- function(
                       data_path = "mdf.csv"
                       )
{
    ## Load all required packages
    load.required.packages()
    data_path = "mdf.csv"
    ## Import study data
    study_data <- read.csv(data_path)
    ## Drop obsevations collected before all centres started collecting triage
    ## category data and observations later than one month prior to creating
    ## this dataset
    study_data <- drop.observations(study_data)
    ## Keep only variables relevant to this study
    study_data <- keep.relevant.variables(study_data)
    ## Define 999 as missing
    study_data[study_data == 999] <- NA
    ## Create omitted object, i.e. number of patients with missing
    ## information, and df with patients with complete information
    study_data <- cc.and.omitted(study_data)
    ## Make Glasgow coma scale components factors
    study_data <- make.gcs.components.factors(study_data$study_data)
    ## Make AVPU factor
    study_data$avpu <- as.factor(study_data$avpu)
    ## Add collapsed mechanism of injury data
    study_data <- add.collapsed.moi(study_data)
    ## Set patients to dead if dead at discharge or at 24 hours
    ## and alive if coded alive and admitted to other hospital
    study_data <- set.to.outcome(study_data)
    ## Prep data for model predictions, make predicitons and review AUROCC
    analysis <- model.model.and.model.clinicians(study_data)
    return (analysis)
}

## * Load required packages
#' Load required packages function
#' @export
load.required.packages <- function()
{
    ## Vector of names of required packages
    packages <- c("SuperLearner",
                  "ggplot2",
                  "nricens",
                  "PredictABEL",
                  "dplyr",
                  "boot")
    ## Require those packages using a loop
    for (p in packages) require(p, character.only = TRUE)
}

## * Drop observations
#' Drop observations function
#'
#' This functions takes as only input the study data data frame and drops
#' (removes) observations collected before all centres started collecting triage
#' category data and observations collected later then one month prior to
#' creating the study dataset
#' @param study_data The study data as a data frame. No default
#' @export
drop.observations <- function(
                              study_data
                              )
{
    ## Test that study data is a data frame, stop if not
    if (!is.data.frame(study_data)) stop("study_data is not a data frame")
    ## Make date of arrival a date vector instead of character
    study_data$doar <- as.Date(study_data$doar)
    ## Drop observations collected before all centres started collecting triage
    ## category data
    study_data <- study_data[study_data$doar >= "2016-07-28", ]
    ## Drop observations included later than one month prior to creating this dataset
    study_data <- study_data[study_data$doar < "2017-05-22", ]
    return(study_data)
}

## * Keep relevant variables
#' Keep relevant variables
#'
#' This function takes as only input the study data data frame and keeps only
#' those variables that are relevant to the study
#' @param study_data The study data as a data frame. No default
#' @param variables_to_keep Character vector of variables to keep. Defaults to c("doar", "age", "sex", "sbp", "rr", "hr", "egcs", "mgcs", "vgcs", "avpu", "nsi", "tc", "hd", "moi", "s24h", "s30d")
#' @param variables_to_drop Character vector of variables to drop. Defaults to NULL
#' @export
keep.relevant.variables <- function(
                                    study_data,
                                    variables_to_keep = c("doar",
                                                          "age",
                                                          "sex",
                                                          "sbp",
                                                          "rr",
                                                          "hr",
                                                          "egcs",
                                                          "mgcs",
                                                          "vgcs",
                                                          "avpu",
                                                          "nsi",
                                                          "tc",
                                                          "hd",
                                                          "moi",
                                                          "s24h",
                                                          "s30d"),
                                    variables_to_drop = NULL
                                    )
{
    ## Test that study data if a data frame, stop if not
    if (!is.data.frame(study_data)) stop("study_data is not a data frame")
    ## Test that either variables_to_keep or variables_to_drop, but not both, is
    ## NULL
    cond1 <- !(is.null(variables_to_keep) | is.null(variables_to_drop))
    cond2 <- is.null(variables_to_keep) & is.null(variables_to_drop)
    if (cond1 | cond2) stop("one of variables_to_keep and variables_to_drop has to be NULL, but not both")
    ## Get names of all variables currently in study_data
    all_vars <- colnames(study_data)
    ## If variables_to_keep is NULL redefine it using all_vars and variables_to_drop
    if (is.null(variables_to_keep)) variables_to_keep <- all_vars[!(all_vars %in% variables_to_drop)]
    ## Keep only relevant variables
    study_data <- study_data[variables_to_keep]
    return(study_data)
}

## * Make GCS components factors
#' Make GCS components factors function
#'
#' This function transform GCS components into factors
#' @param study_data The study data as a data frame. No default
#' @param gcs_components_names Character vector of GCS components. Defaults to c("egcs", "mgcs", "vgcs")
#' @export
make.gcs.components.factors <- function(
                                        study_data,
                                        gcs_components_names = c("egcs", "mgcs", "vgcs")
                                        )
{
    ## Test that study data if a data frame, stop if not
    if (!is.data.frame(study_data)) stop("study_data is not a data frame")
    ## Make sure all components are in the dataset
    if (!all(gcs_components_names %in% colnames(study_data))) stop("not all components are in the dataset")
    ## Make components factors
    study_data[, gcs_components_names] <- lapply(study_data[, gcs_components_names], as.factor)
    ## Make dummy variables from gcs components; and set
    study_data <- cbind(study_data,
                        model.matrix( ~ egcs + mgcs + vgcs,
                                     data = study_data)[,-1])

                                        #test
    #apa <- model.matrix( ~ egcs + mgcs + vgcs,
    #                         data = study_data)[,-1]
    #apa1 <- study_data
    return(study_data)
}

## * Add collapsed mechanism of injury
#' Add collapsed mechanism of injury function
#'
#' Takes the main data as input and generates a new mechanism of injury variable
#' with overall levels instead of codes
#' @param study_data The study data as a data frame. No default
#' @export
add.collapsed.moi <- function(
                              study_data
                              )
{
    ## Test that study data if a data frame, stop if not
    if (!is.data.frame(study_data)) stop("study_data is not a data frame")
    ## Transport accidents are V01-V99, falls are W00-W19, burns are X00-X19, other
    ## external causes of accidental injury are W20-W99 and X20-X59, intentional
    ## self-harm is X60-X84, assault is X85-Y09, events of undetermined intent is
    ## Y10-Y34, and legal intervention is Y35-Y36
    c00_09 <- paste0(0, 0:9) # make vector of codes 00-09
    ## List of lists, in which each sub-list holds two vectors. The first vector is
    ## called codes and has the codes used to define a certain mechanism and the
    ## second is called level and has the name of that mechanism
    moi_list <- list(ta = list(codes = paste0("V", c(c00_09, 10:99)),
                               level = "Transport accident"),
                     falls = list(codes = paste0("W", c(c00_09, 10:19)),
                                  level = "Fall"),
                     burns = list(codes = paste0("X", c(c00_09, 10:19)),
                                  level = "Burn"),
                     oeca = list(codes = c(paste0("W", 20:99), paste0("X", 20:59)),
                                 level = "Other external cause of accidental injury"),
                     ish = list(codes = paste0("X", 60:84),
                                level = "Intentional self harm"),
                     assault = list(codes = c(paste0("X", 85:99), paste0("Y", c00_09)),
                                    level = "Assault"),
                     ui = list(codes = paste0("Y", 10:34),
                               level = "Event of undetermined intent"),
                     li = list(codes = paste0("Y", 35:36),
                               level  = "Legal intervention"))
    new_moi <- as.character(study_data$moi) # make a new character vector of moi
    ## Replace codes with levels in the new copy
    for (m in moi_list)
        new_moi[grep(paste0(m$codes, collapse = "|"), study_data$moi)] <- m$level
    ## Replace old moi with new in the study data
    study_data <- data.frame(study_data, cmoi = as.factor(new_moi))
    return (study_data)
}

## * Set patients to dead according to couple of parameters
set.to.outcome <- function(
                           study_data
                           )
{
    ## Test that study data if a data frame, stop if not
    if (!is.data.frame(study_data)) stop("study_data is not a data frame")
    ## set s30d to dead if we know that patients were dead on discharge or at 24
    ## hours
    study_data[study_data$hd == 1 & !is.na(study_data $hd) | study_data$s24h == 1 & !is.na(study_data$s24h), "s30d"] <- 1
    ## set s30d to 0 if 2, i.e. to alive if coded alive and admitted to other hospital
    study_data[study_data$s30d == 2 & !is.na(study_data$s30d), "s30d"] <- 0

    return (study_data)
}

## * Create omitted object and complete.case object

cc.and.omitted <- function (
                            study_data
                            )
{
    ## order the dataset according to data of arrival and s30d
    mdf <- study_data[order(-study_data$s30d, study_data$doar), ]
    ## identify complete cases
    cc <- mdf[complete.cases(mdf), ]
    ## create omitted object
    omitted <- nrow(mdf) - nrow(cc)

    return(list(study_data = cc,
                omitted = omitted))
}

## * Set seed for reproducabilility
set.seed(123)

## * Prepare data for superlearner
prep_data_for_superlearner <- function(
                                       mothertree,
                                       outcome = 's30d',
                                       gcs_components_names = c('egcs', 'mgcs', 'vgcs')
                                       )
{

    ## Order dataframe by date
    df <- mothertree[order(mothertree$doar),]
    ## Find midpoint of dates
    dates <- df$doar
    mid <- dates[1] + floor(tail(dates, n = 1) - dates[1])/2
    ## Create set for training the model
    train <- df[df$doar < mid, ]
    ## Create set for review of model performance
    review <- df[df$doar >= mid, ]
    ## Set sets
    x_sets <- list(x_train = train,
                   x_review = review)
    ## Dfs without outcome
    x_wo_outcome <- lapply(x_sets,
                           function(x) x[, !(names(x) %in% outcome)])
    ## Dfs without triage category,outcome and original gcs components
    x_wo_tc_gcs_outcome <- lapply(x_sets,
                                  function(x) x[, !(names(x) %in% c('tc',
                                                                    gcs_components_names,
                                                                    outcome))])
    ## Extract outcome variables for training and review set
    y_training_and_review <- lapply(x_sets, function(x) x[, outcome])
    names(y_training_and_review) <- c('y_train', 'y_review')

    return (list(sets_wo_tc = x_wo_tc_gcs_outcome,
                 sets_w_tc = x_wo_outcome,
                 outcome = y_training_and_review))

}

## * Predictions with SuperLearner

predictions_with_superlearner <- function(mothertree,
                                          outcome = 's30d',
                                          all = FALSE)
{

    ## Retrive training and review data as well as outcome for training and review
    data <- prep_data_for_superlearner(mothertree, outcome)
    ## Train algorithm with training set
    train_algo <- SuperLearner(Y = data$outcome$y_train,
                               X = data$sets_wo_tc$x_train,
                               family = binomial(),
                               SL.library = c('SL.mean', 'SL.glmnet'))
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
    ## Return different data depending on analysis; to alleviate analysis
    alleviated_data <- list(clinicians_predictions = data$sets_w_tc$x_review$tc,
                            pred_con = pred,
                            pred_cat = pred_cat,
                            outcome_review = data$outcome$y_review)
    if (all = TRUE){ ##kan bli fel
        return (list(alleviated_data,
                     data))
    }else {
        return (alleviated_data)
    }
}

## * Model review (AUROCC, Calibration, Reclassification)
## ** AUROCC

review_aurocc <- function(
                          mothertree,
                          outcome = 's30d'
                          )
{

    ## Get predictions from Superlearner and various data
    predictions_and_data <- predictions_with_superlearner(mothertree,
                                                          outcome)
    ## Set up prediction object for SuperLearner and clinicians; ROCR package
    pred_rocr <- list(SuperLearner = ROCR::prediction(as.numeric(predictions_and_data$pred_cat),
                                                      predictions_and_data$outcome_review),
                      clin = ROCR::prediction(predictions_and_data$clinicians_predictions,
                                              predictions_and_data$outcome_review))
    ## Calculate the Area Under the Receiver Operating Charecteristics Curve
    AUROCC <- lapply(pred_rocr,
                     function(model) ROCR::performance(model,
                                                       measure = 'auc',
                                                       x.measure =  'cutoff')@y.values[[1]])
    return (AUROCC)

}

## ** Reclassification

review_reclassification <- function(
                                    mothertree,
                                    outcome = 's30d'
                                    )
{

    ## Get predictions from Superlearner and various data
    predictions_and_data <- predictions_with_superlearner(mothertree,
                                                          outcome,
                                                          all = TRUE)
    ## Compute reclassification of SuperLearner model and clinicians
    reclassification <- nribin(event = predictions_and_data$data$outcome$y_review,
                               p.std = predictions_and_data$data$sets_w_tc$x_review$tc,
                               p.new = predictions_and_data$predictions,
                               cut = c(2,3,4))
    return (reclassification)

}

## * Significance testing (P-value) and Confidence intervals (95 %)
## ** Confidence intervals

confidence_intervals <- function(
                                 mothertree,
                                 outcome = 's30d',
                                 bs_samples = 3
                                 )
{

    ## Get point estimates of AUROCC
    analysis <- review_aurocc(mothertree, outcome)
    ## Bootstrap n bootstrap samples
    simulated_dfs <- lapply(1:bs_samples,
                            function(i) mothertree[sample(1:nrow(mothertree),
                                                          replace = TRUE),])
    ## Train, predict and aurocc on every sample
    train_predict_aurocc <- lapply(simulated_dfs,
                                   function (df) review_aurocc(df, outcome))
    ## Matrixify samples
    matrixify <- sapply(train_predict_aurocc, unlist)
    ## Calculate Deltastar
    deltastar <- apply(matrixify, 2, function(col) col - unlist(analysis))
    ## Get 2.5 and 97.5 percentiles
    quantiles <- apply(deltastar, 1, function (row) quantile(row, c(.025, 0.975)))
    ## Generate confidence intervals
    confidence_intervals <- cbind(apply(quantiles,
                                        1,
                                        function(row) unlist(analysis) - row),
                                  point_estimate = unlist(analysis))

    return(confidence_intervals)

}

## ** Significance testing

## Create function for boot for p-value on difference of point estimates
diff <- function(
                 d1,
                 i)
{

    d = d1;
    d$x <- d$x[i];  # randomly re-assign groups
    ## Subset d1 for model and clinicians
    model_1_df <- d[d$x %in% 'model_1', ]
    model_2_df <- d[d$x %in% 'model_2', ]
    ## List for looping
    dfs <- list(model_1 = model_1_df,
                model_2 = model_2_df)
    ## Calculate AUROOC for each df
    AUROCCS <- lapply(dfs, function(df) ROCR::performance(ROCR::prediction(df$Variable,
                                                                           labels = df$s30d),
                                                          'auc')@y.values)
    ## Calculate difference of estimates
    Diff <- AUROCCS$model_1[[1]] - AUROCCS$model_2[[1]]

    Diff

}

#pred_con, pred_cat, clinicians_predictions
significance.testing <- function(mothertree,
                                 outcome = 's30d',
                                 which_predictions,
                                 bs_samples = 3)
{

    ## Get data for significance testing
    data <- predictions_with_superlearner(mothertree,
                                          outcome)
    ## Create dataframe for significance testing
    df <- data.frame(x = c(rep('model_1',
                               each = length(data[[which_predictions[1]]])),
                           rep('model_2',
                               each = length(data[[which_predictions[2]]]))),
                     Variable = c(data[[which_predictions[1]]],
                                  data[[which_predictions[2]]]),
                     s30d = as.factor(rep(data$outcome_review, 2)))
    ## Bootstrap diff on n bootstrap samples
    boot_strapped <- boot(data = df,
                          statistic = diff,
                          R = bs_samples)
    ##Generate p-value
    p_value<- mean(abs(boot_strapped$t) > abs(boot_strapped$t0))

    return (p_value)

}

model.model.and.model.clinicians <- function(
                                             mothertree,
                                             outcome
                                             )
{
    ## Generate p-value on difference of AUROCC of SuperLearner and clinicians
    p1 <- significance.testing(mothertree,
                               which_predictions = c('pred_cat',
                                                     'clinicians_predictions'))
    ## Generate p-value on difference of AUROCC of SuperLearner's continous
    ## predictions and categorised predictions
    p2 <- significance.testing(mothertree,
                               which_predictions = c('pred_con',
                                                     'pred_cat'))
    return (c(p1,p2))
}

## * COMPLETE ANALYSIS

complete_analysis <- function (
                               mothertree,
                               outcome = 's30d',
                               bs_samples = 3
                               )
{

    ## Discrimination
    AUROCC <- confidence_intervals(mothertree, outcome, bs_samples)
    ## Reclassification
    reclassification <- review_reclassification(mothertree, outcome)
    ## P-value on difference of discrimination
    h_testing <- significance_testing(mothertree, outcome, bs_samples)
    ## List statistics
    statistics <- list(AUROCC = AUROCC,
                       Reclassification = reclassification,
                       P_value = h_testing)
    ## Save rdata to file
    saveRDS(statistics, file = sprintf('complete_analysis_%s.Rdata', bs_samples))
    ## Return list with statistics
    return (statistics)

}



## * test space

df.original <-data.frame(eggs = c('foo', 'foo', 'bar', 'bar', 'bar', 'foo'),
                         ham = c(1,2,3,4, 'apa', 'korv'),
                         apa = factor(c(1,2,3,4,5,6)))
t1 = data.frame(model.matrix( ~ ham + apa, data =df.original))[,-1]
t2 = cbind(df.original, t1); t2[, c('eggs', 'apa')] <- NULL
t2
t2 = model.matrix( ~., data =df.original)[,-1]

l <- list("a", "b", "c")

l2 <- list('2', '3', '4', '4')

l3 <- list(l, l2)
str(l3)
