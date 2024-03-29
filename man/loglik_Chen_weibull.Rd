% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loglik_Chen_weibull.R
\name{loglik_Chen_weibull}
\alias{loglik_Chen_weibull}
\title{Log-likelihood function for Weibull distribution with cured population}
\usage{
loglik_Chen_weibull(par, df)
}
\arguments{
\item{par}{a vector with three elements, where the first element denotes the logistic of the proportion
of the cured population, and the second element and the third element denote
the log of the shape and log of the scale parameter of the Weibull distribution.}

\item{df}{The subject-level event data, including \code{time}
and \code{event}.}
}
\value{
The negative value of the log-likelihood function given parameter
\code{par} and the dataset \code{df}
}
\description{
Provide log-likelihood function for Weibull distribution with cured population
}
\references{
\itemize{
\item Chen, Tai-Tsang. "Predicting analysis times in randomized clinical trials with cancer immunotherapy."
BMC medical research methodology 16.1 (2016): 1-10.
}
}
