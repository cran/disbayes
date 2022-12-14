% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.R
\name{tidy_obsdat}
\alias{tidy_obsdat}
\title{Extract observed data from a disbayes model fit}
\usage{
tidy_obsdat(x)
}
\arguments{
\item{x}{Fitted \code{\link{disbayes}} model}
}
\value{
A data frame with columns \code{num} and \code{denom} giving the incidence, prevalence and mortality (and remission if used) numerators and denominators used in the model fit.  The column \code{var} indicates which of incidence, prevalence etc. the numbers refer to.   The column \code{prob} is derived as \code{num} divided by \code{denom}.  Columns \code{lower} and \code{upper} define credible intervals for the "data-based" point estimate \code{prob}, obtained from the Beta posterior assuming a Beta(0.5, 0.5) prior.

This "data-based" point estimate can be compared with estimates from the model using the functions \code{\link{plotfit_data_disbayes}} and \code{\link{plotfit_disbayes}}.
}
\description{
Extract observed data from a disbayes model fit
}
