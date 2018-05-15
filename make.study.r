#' Make study function
#'
#' This is the main study function and runs the entire study.
#' @param data_path Path to data set. Should be a character vector of length 1. Defaults to c("../data/mdf.csv")
#' @param bs_samples The number of bootstrap samples to be generated as int. Defaults to 10
#' @export
make.study <- function(
                       data_path =  c("./extdata/sample.csv"),
                       bs_samples = 3
                       )
{
    ## Set seed for reproducability
    set.seed(123)
    ## Source all functions (remove when turned into package)
    files <- list.files(".", pattern = ".r$", full.names = TRUE)
    files <- files[!(files %in% "./make.study.r")]
    for (f in files) source(f)
    ## Load all required packages (remove when turned into package)
    load.required.packages()
    ## Import study data
    study_data <- read.csv(data_path, stringsAsFactors = FALSE)
    ## Drop obsevations collected before all centres started collecting triage
    ## category data and observations later than one month prior to creating
    ## this dataset
    study_data <- drop.observations(study_data, test = TRUE)
    ## Get data dictionary
    data_dictionary <- get.data.dictionary(test = TRUE)
    ## Keep only variables relevant to this study
    study_data <- keep.relevant.variables(study_data,
                                          variables_to_keep = names(data_dictionary),
                                          test = TRUE)
    ## Define 999 as missing
    study_data[study_data == 999] <- NA
    ## Prepare study data using the data dictionary
    study_data <- prepare.study.data(study_data, data_dictionary)
    ## Set patients to dead if dead at discharge or at 24 hours
    ## and alive if coded alive and admitted to other hospital
    study_data <- set.to.outcome(study_data)
    ## Make age numeric
    study_data$age <- as.numeric(study_data$age)
    ## Collapse mechanism of injury
    study_data <- collapse.moi(study_data)
    ## Apply exclusion criteria, i.e. drop observations with missing outcome
    ## data and save exclusions to results list
    results <- list() # List to hold results
    study_data <- apply.exclusion.criteria(study_data)
    ## Create missing indicator variables and save table of number of missing
    ## values per variable
    study_data <- add.missing.indicator.variables(study_data)
    ## Do median imputation
    ## study_data <- do.median.imputation(study_data)
    ## Create table of sample characteristics
    results$table_of_sample_characteristics <- create.table.of.sample.characteristics(study_data, data_dictionary)
    ## Transform GCS, MOI, and AVPU into dummy variables,
    ## but keep original variables for table1
    study_data <-  as.data.frame(model.matrix( ~.,
                                              data = study_data)[, -1])
    ## Prepare data for SuperLearner predictions
    prepped_data <- prep.data.for.superlearner(study_data, test = TRUE)
    ## Train and review SuperLearner on study sample
    study_sample <- predictions.with.superlearner(prepped_data)
    ## Bootstrap samples
    samples <- generate.bootstrap.samples(study_data,
                                          3)
    ## Prepare samples
    prepped_samples <- prep.bssamples(samples)
    ## Train and review SuperLearner on boostrap samples
    samples <- train.predict.bssamples(samples = prepped_samples)
    ## Create list of analysis to conduct
    funcList <- list(list(func = 'model.review.AUROCC',
                          model_or_pe = c('pred_cat',
                                          'clinicians_predictions')),
                     list(func = 'model.review.reclassification',
                          model_or_pe = c('NRI+',
                                          'NRI-')))
    ## Generate confidence intervals around point estimates from funcList
    CIs <- lapply(funcList,
                  function(i) generate.confidence.intervals(study_sample,
                                                            func = get(i$func),
                                                            model_or_pointestimate = i$model_or_pe,
                                                            samples = samples))
    ## Set names of cis
    names(CIs) <- c('AUROCC',
                    'reclassification')
    ## Compile manuscript
    compile.manuscript("superlearner_vs_clinicians_manuscript.rtex")

    return (CIs)
}

