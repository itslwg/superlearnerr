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
    study_data <- drop.observations(study_data, test = TRUE)
    ## Keep only variables relevant to this study
    study_data <- keep.relevant.variables(study_data)
    ## Define 999 as missing
    study_data[study_data == 999] <- NA
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
    ## Train and review SuperLearner on study sample
    study_sample <- predictions.with.superlearner(study_data)
    ## Bootstrap samples
    samples <- generate.bootstrap.samples(study_data,
                                          bs_samples)
    ## Train and review SuperLearner on boostrap samples
    samples <- train.predict.bssamples(samples = samples)
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
