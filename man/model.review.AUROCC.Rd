% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.review.AUROCC.r
\name{model.review.AUROCC}
\alias{model.review.AUROCC}
\title{Area Under Receiver Operating Characteristics Curve (AUROCC) function}
\usage{
model.review.AUROCC(study_sample, which_preds = c("pred_cat", "tc"),
  outcome_name)
}
\arguments{
\item{study_sample}{The study sample list. No default.}

\item{which_preds}{Character vector with list name of predictions. Default: c('pred_cat', 'tc')}

\item{outcome_name}{The name of the outcome variable. No default.}
}
\description{
This function calculates AUROCC of predictions of the specified models.
}
