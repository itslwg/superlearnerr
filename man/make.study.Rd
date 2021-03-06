% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make.study.r
\name{make.study}
\alias{make.study}
\title{Make study function}
\usage{
make.study(data_path = c("./extdata/sample.csv"), bs_samples = 1000)
}
\arguments{
\item{data_path}{Path to data set. Should be a character vector of length 1. Defaults to c("../data/mdf.csv")}

\item{bs_samples}{The number of bootstrap samples to be generated as int. Defaults to 10}
}
\description{
This is the main study function and runs the entire study.
}
