% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers-data.R
\name{calc_error_relative_to_nontargets}
\alias{calc_error_relative_to_nontargets}
\title{Calculate response error relative to non-target values}
\usage{
calc_error_relative_to_nontargets(data, response, nt_features)
}
\arguments{
\item{data}{A \code{data.frame} object where each row is a single observation}

\item{response}{Character. The name of the column in \code{data} which contains
the response}

\item{nt_features}{Character vector. The names of the columns in \code{data} which
contain the values of the non-targets}
}
\value{
A \code{data.frame} with n*m rows, where n is the number of rows of \code{data}
and m is the number of non-target variables. It preserves all other columns
of \code{data}, except for the non-target locations, and adds a column \code{y_nt},
which contains the transformed response error relative to the non-targets
}
\description{
Given a vector of responses, and the values of non-targets, this
function computes the error relative to each of the non-targets.
}
\keyword{transform}
