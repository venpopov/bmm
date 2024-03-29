% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers-model.R
\name{stancode.bmmformula}
\alias{stancode.bmmformula}
\title{Generate Stan code for bmm models}
\usage{
\method{stancode}{bmmformula}(object, data, model, prior = NULL, ...)
}
\arguments{
\item{object}{A \code{bmmformula} object}

\item{data}{An object of class data.frame, containing data of all variables
used in the model. The names of the variables must match the variable names
passed to the \code{bmmodel} object for required argurments.}

\item{model}{A description of the model to be fitted. This is a call to a
\code{bmmodel} such as \code{mixture3p()} function. Every model function has a
number of required arguments which need to be specified within the function
call. Call \code{\link[=supported_models]{supported_models()}} to see the list of supported models and
their required arguments}

\item{prior}{One or more \code{brmsprior} objects created by \code{\link[brms:set_prior]{brms::set_prior()}}
or related functions and combined using the c method or the + operator. See
also \code{\link[=default_prior]{default_prior()}} for more help. Not necessary for the default model
fitting, but you can provide prior constraints to model parameters}

\item{...}{Further arguments passed to \code{\link[brms:stancode]{brms::stancode()}}. See the
description of \code{\link[brms:stancode]{brms::stancode()}} for more details}
}
\value{
A character string containing the fully commented Stan code to fit a
bmm model.
}
\description{
Given the \code{model}, the \code{data} and the \code{formula} for the model,
this function will return the combined stan code generated by \code{bmm} and
\code{brms}
}
\examples{
scode1 <- stancode(bmf(c ~ 1, kappa ~ 1),
                   data = oberauer_lin_2017,
                   model = sdm(resp_error = "dev_rad"))
cat(scode1)
}
\seealso{
\code{\link[=supported_models]{supported_models()}}, \code{\link[brms:stancode]{brms::stancode()}}
}
\keyword{extract_info}
