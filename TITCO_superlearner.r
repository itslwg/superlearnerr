## * Make study
#' Make study function
#'
#' This is the main study function and runs the entire study.
#' @param data_path Path to data set. Should be a character vector of length 1. Defaults to c("../data/mdf.csv")
#' @param bs_samples The number of bootstrap samples to be generated as int. Defaults to 10
#' @export
make.study <- function(
                       data_path =  c("./extdata/sample.csv"),
                       bs_samples = 10
                       )
{
    ##Set seed for reproducability
    set.seed(123)
    ## Load all required packages
    load.required.packages()
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
    ## Define 99 in gcs components as missing
    gcs_components_names <- c('mgcs',
                              'egcs',
                              'vgcs')
    study_data[, gcs_components_names][study_data[, gcs_components_names] == 99] <- NA
    ## Create omitted object, i.e. number of patients with missing
    ## information, and df with patients with complete information
    study_data <- cc.and.omitted(study_data)$study_data
    ## Transform GCS, MOI, and AVPU into factor variables
    study_data <- to.factor.variables(study_data)
    ## Transform GCS, MOI, and AVPU into dummy variables,
    ## but keep original variables for table1
    study_data <- cbind(study_data,
                        model.matrix( ~.,
                                     data = study_data)[, -1])
    ## Set patients to dead if dead at discharge or at 24 hours
    ## and alive if coded alive and admitted to other hospital
    study_data <- set.to.outcome(study_data)
    ## Train model on training set and predict on review set. Then,
    ## estimate 95 % CIs around difference of AUROCCs of categorised
    ## SuperLearner (seefunction predictions.with.superlearner() for
    ## categorisationprocedure) and clinicians triage.
    conf <- generate.confidence.intervals(study_data, bs_samples)
    ## Conduct reclassification analysis and generate nribin-object
    reclass <- model.review.reclassification(study_data)
    pvalues <- generate.pvalue(study_data, bs_samples)
    ## Return list of model AUROCCs and their respective confidence
    ## intervals, nribin object and p-values.
    statistics <- list(CIs = conf,
                       Reclassification = reclass,
                       Pvalues = pvalues)
    ## Compile manuscript
    compile.manuscript("superlearner_vs_clinicians_manuscript.rtex")
    return (statistics)
}

## * Load required packages
#' Load required packages function
#' @export
load.required.packages <- function()
{
    ## Vector of names of required packages
    packages <- c("SuperLearner",
                  "nricens",
                  "dplyr",
                  "boot",
                  "knitr")
    ## Require those packages using a loop
    for (p in packages) require(p, character.only = TRUE)
}

## * Drop observations
#' Drop observations function
#'
#' This function takes as only input the study data data frame and drops
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

## * Transform GCS, AVPU and MOI into factors
## ** Collapse mechanism of injury variable
#' Collapse mechanism of injury function
#'
#' Takes the main data as input and generates a new mechanism of injury variable with overall levels instead of codes.
#' @param study_data The study data as a data frame. No default
#' @export
collapse.moi <- function(
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
    ## Replace old moi with new, factor moi in dataset
    study_data <- data.frame(study_data, cmoi = as.factor(new_moi))
    study_data$moi <- NULL

    return (study_data)
}

## ** Transform some variables into factors (Formulering)
#' Make variables to factor function
#'
#' This function transform variables into factors.
#' @param study_data The study data as a data frame. No default
#' @param variables_to_factor Character vector of variables. Defaults to c("egcs", "mgcs", "vgcs", "avpu", "cmoi")
#' @export
to.factor.variables <- function(
                                study_data,
                                variables_to_factor = c("egcs",
                                                        "mgcs",
                                                        "vgcs",
                                                        "avpu",
                                                        "cmoi")
                                )
{
    ## Collapse mechanism of injury
    study_data <- collapse.moi(study_data)
    ## Test that study data if a data frame, stop if not
    if (!is.data.frame(study_data)) stop("study_data is not a data frame")
    ## Make sure all components are in the dataset
    if (!all(variables_to_factor %in% colnames(study_data))) stop("not all components are in the dataset")
    ## Make variables as factors
    study_data[, variables_to_factor] <- lapply(study_data[, variables_to_factor], as.factor)

    return(study_data)
}

## * Set outcome variable
#' Change outcome function
#'
#' This function changes patient outcome data if certain conditions are met. The outcome variable is set to dead if patients were dead on discharge after 30 days or if dead after 24 hours. Moreover, s30d is set to alive if coded alive and admitted to other hospital.
#' @param study_data The study data as a data frame. No default
#' @export
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

## * Count omitted patients and create complete case dataset
#' Number of patients omitted and complete case dataset function
#'
#' This function counts the number of patients with missing data. Then, the patients with missing data are omitted from the study data to create a complete case dataset.
#' @param study_data The study data as a data frame. No default
#' @export
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

## * Prepare data for superlearner
#' Prepare data function
#'
#' This function prepares and generates training and review sets by splitting the dataset in half according to date of arrival. Factor variables are removed.
#' @param study_data The study data as a data frame. No default
#' @param outcome The outcome variable as a string. Default: 's30d'.
#' @param factors_to_remove Character vector describing factor variables to remove from datasets. Default: c('mgcs','egcs','vgcs','avpu','cmoi')
#' @export
prep.data.for.superlearner <- function(
                                       study_data,
                                       outcome = 's30d',
                                       factors_to_remove = c('mgcs',
                                                             'egcs',
                                                             'vgcs',
                                                             'avpu',
                                                             'cmoi')
                                       )
{
    ## Order dataframe by date
    df <- study_data[order(study_data$doar),]
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
    ## Dfs without triage category, outcome and factor variables
    x_wo_tc_outcome_factors <- lapply(x_sets,
                              function(x) x[, !(names(x) %in% c('tc',
                                                                factors_to_remove,
                                                                outcome))])
    ## Extract outcome variables for training and review set
    y_training_and_review <- lapply(x_sets, function(x) x[, outcome])
    names(y_training_and_review) <- c('y_train', 'y_review')

    return (list(sets_wo_tc = x_wo_tc_outcome_factors,
                 sets_w_tc = x_wo_outcome,
                 outcome = y_training_and_review))
}

## * Predictions with SuperLearner
#' Generate predictions with SuperLearner
#'
#' This function trains SuperLearner on the training set. Then, predictions are divided byquantiles into four colour-coded groups. The groups are green, yellow, orange, and red. They respectively include ranges from the 0% quantile to 25% quantile, 25% to 50%, 50% to 75%, and 75% to 100% of the continous predictions. Lastly, SuperLearner uses groups to make predictions on the review set.
#' @param study_data The study data as a data frame. No default
#' @param outcome The outcome variable as a string. Default: 's30d'.
#' @param models Models to include in SuperLearner. Default: SL.mean and SL.glmnet.
#' @param all Boolean value to determine whether data should be included in return(). Default: FALSE, i.e. not to include the dataset.
predictions.with.superlearner <- function(
                                          study_data,
                                          models = c('SL.mean', 'SL.glmnet'),
                                          all = FALSE
                                          )
{
    ## Retrive training and review data as well as outcome for training and review
    data <- prep.data.for.superlearner(study_data)
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
    ## Return different data depending on analysis; to alleviate analysis
    alleviated_data <- list(clinicians_predictions = data$sets_w_tc$x_review$tc,
                            pred_con = pred,
                            pred_cat = pred_cat,
                            outcome_review = data$outcome$y_review)
    if (all == TRUE){
        return (list(preds = alleviated_data,
                     data = data))
    }else {
        return (alleviated_data)
    }
}

## * Model review (AUROCC, Reclassification)
## ** AUROCC
#' Area Under Receiver Operating Characteristics Curve (AUROCC) function
#'
#' This function calculates the AUROCC of specified models.
#' @param study_data The study data as a data frame. No default
#' @param models Character vector describing which models to calculate AUROCCs on. Default: c('pred_cat', 'clinicians_predictions')
#' @export
 model.review.AUROCC <- function(
                                study_data,
                                models = c('pred_con',
                                           'pred_cat',
                                           'clinicians_predictions')
                                )
{
    ## Get predictions from Superlearner and review as well as training dataset
    predictions_and_data <- predictions.with.superlearner(study_data)
    ## Setup prediction obejects for ROCR
    pred_rocr <- lapply(models,
                        function(model) ROCR::prediction(
                                                  as.numeric(
                                                      predictions_and_data[[model]]),
                                                  predictions_and_data$outcome_review))
    ## Set names for models
    names(pred_rocr) <- models
    ## Calculate the Area Under the Receiver Operating Charecteristics Curve
    AUROCC <- lapply(pred_rocr,
                     function(model) ROCR::performance(model,
                                                       measure = 'auc',
                                                       x.measure =  'cutoff')@y.values[[1]])

    return (AUROCC)
}

## ** Reclassification
#' Generate reclassification tables and Net Reclassification Index (NRI)
#'
#' This function generates an nribin object for cut SuperLearner and clinicians, i.e. cross tabulations of categorisation of the two, description of net proportions patient movements upwards and downwards in categories, and NRI.
#' @param study_data The study data as a data frame. No default
#' @export
model.review.reclassification <- function(
                                          study_data
                                          )
{
    ## Get predictions from Superlearner and various data
    predictions_and_data <- predictions.with.superlearner(study_data,
                                                          all = TRUE)
    ## Compute reclassification of SuperLearner model and clinicians
    reclassification <- nricens::nribin(event = predictions_and_data$data$outcome$y_review,
                                        p.std = predictions_and_data$data$sets_w_tc$x_review$tc,
                                        p.new = as.numeric(predictions_and_data$preds$pred_cat),
                                        cut = c(2,3,4),
                                        niter = 0)

    return (reclassification)
}

## * Significance testing (P-value) and Confidence intervals (95 %)
## ** Significance testing
## *** Statistic function for boot
#' 'Statistic' function to use with the "boot"-package
#'
#' This function is used as 'Statistic' in the generate.pvalue function.
#' @param d1 The study data as data frame. No default.
#' @param i The indice variable which defines the bootstrap sample.
#' @export
diff <- function(
                 d1,
                 i
                 )
{
    d = d1;
    d$x <- d$x[i];  # randomly re-assign groups
    ## Subset d1 for models
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
## *** P-value function
#' P-value generating function
#'
#' This function generates p-value of difference in AUROCC using a permutation test.
#' @param study_data The study data as a data frame. No default
#' @param which_models Character vector describing models to compare in AUROCCs. Default: c('pred_cat', 'clinicians_predictions').
#' @param bs_samples The number of bootstrap samples to be generated. Specified in main.study()
#' @export
significance.testing <- function(
                                 study_data,
                                 which_models = c('pred_cat',
                                                  'clinicians_predictions'),
                                 bs_samples
                                 )
{
    ## Get data for significance testing
    data <- predictions.with.superlearner(study_data)
    ## Create dataframe for significance testing
    df <- data.frame(x = c(rep('model_1',
                               each = length(data[[which_models[1]]])),
                           rep('model_2',
                               each = length(data[[which_models[2]]]))),
                     Variable = c(data[[which_models[1]]],
                                  data[[which_models[2]]]),
                     s30d = as.factor(rep(data$outcome_review, 2)))
    ## Bootstrap diff on bs_samples bootstrap samples
    boot_strapped <- boot::boot(data = df,
                                statistic = diff,
                                R = bs_samples)
    ##Generate p-value
    p_value <- mean(abs(boot_strapped$t) > abs(boot_strapped$t0))

    return (p_value)
}
## *** Generate p-values between model_cat and model_con, as well as between SuperLearner and clinicians
#' Generate p-values function
#'
#' This function generate p-values of difference of AUROCC between categorised SuperLearner predictions and clinicians triage category, as well as between continous and categorised SuperLearner predictions - the latter to evaluate the chosen cutoffs for the cut predicitons.
#' @param study_data The study data as a data frame. No default
#' @param bs_samples The number of bootstrap samples to be generated. Specified in main.study()
#' @export
generate.pvalue <- function(
                            study_data,
                            bs_samples
                            )
{
    ## Generate p-value on difference of AUROCC of SuperLearner and clinicians
    p1 <- significance.testing(study_data,
                               which_models = c('pred_cat',
                                                'clinicians_predictions'),
                               bs_samples)
    ## Generate p-value on difference of AUROCC of SuperLearner's continous
    ## predictions and categorised predictions
    p2 <- significance.testing(study_data,
                               which_models = c('pred_con',
                                                'pred_cat'),
                               bs_samples)
    return (c(p1,p2))
}

## ** Estimate confidence intervals
#' Confidence interval function
#'
#' This function generates confidence intervals around AUROCC of Superlearner and clinicians using empirical bootstrapping.
#' @param study_data The study data as a data frame. No default
#' @param bs_samples The number of bootstrap samples to be generated as int. Specified in main.study()
#' @export
generate.confidence.intervals <- function(
                                          study_data,
                                          bs_samples
                                          )
{

    ## Get point estimates of AUROCC
    analysis <- model.review.AUROCC(study_data)
    ## Calculate difference of AUROCCs of categorised SuperLearner predictions
    ## and clinicians, as well as between categorised and continous
    ## SuperLearner predictions
    diff <- list(analysis$pred_cat - analysis$clinicians_predictions,
                 analysis$pred_cat - analysis$pred_con)
    ## Bootstrap n bootstrap samples
    simulated_dfs <- lapply(1:bs_samples,
                            function(i) study_data[sample(1:nrow(study_data),
                                                          replace = TRUE),])
    ## Train, predict and aurocc on every sample
    train_predict_aurocc <- lapply(simulated_dfs,
                                   function (df) model.review.AUROCC(df))
    ## Matrixify samples
    matrixify <- sapply(train_predict_aurocc, unlist)
    ## Calculate difference between AUROCCs in every sample
    diff_samples <- list(matrixify['pred_cat',] - matrixify['clinicians_predictions',],
                         matrixify['pred_cat',] - matrixify['pred_con',])
    ## Calculate deltastar
    deltastar <- mapply('-', diff_samples, diff, SIMPLIFY = FALSE)
    ## Get 2.5% and 97.5% percentiles from difference of samples
    quantiles <- lapply(deltastar,
                        function (vec)quantile(vec,
                                               c(.025, 0.975)))
    ## Generate confidence intervals
    confidence_intervals <- mapply('-',
                                   diff,
                                   quantiles,
                                   SIMPLIFY = FALSE)

    return(confidence_intervals)
}

#' Compile manuscript function
#'
#' This function compiles the Superlearner vs clinicians manuscript
#' @param manuscript_file_name The file name of the manuscript as a character vector of length 1. No default.
#' @param compiler The compiler to use when compiling the rtex manuscript into a pdf. Defaults to pdflatex.
#' @export
compile.manuscript <- function(
                               manuscript_file_name,
                               compiler = "pdflatex"
                               )
{
    ## Compile manuscript using knit2pdf
    knit2pdf(manuscript_file_name, compiler = compiler)
}
