% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Tests.R
\name{T_test}
\alias{T_test}
\title{Do t-test}
\usage{
T_test(v1, v2, paired = FALSE, log = FALSE)
}
\arguments{
\item{v1}{a (non-empty) numeric vector of data values.}

\item{v2}{an optional (non-empty) numeric vector of data values.}

\item{paired}{a logical indicating whether you want a paired t-test.}

\item{log}{whether data have log2 transformation.}
}
\value{
Return a log2 fold change value and t-test p value
}
\description{
Do t-test
}
\note{
Only for three and more repeat experiment.
}
