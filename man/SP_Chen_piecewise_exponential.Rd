% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SP_Chen_piecewise_exponential.R
\name{SP_Chen_piecewise_exponential}
\alias{SP_Chen_piecewise_exponential}
\title{Survival probability function of the piecewise exponential distribution with cured population}
\usage{
SP_Chen_piecewise_exponential(par, t, piecewiseSurvivalTime)
}
\arguments{
\item{par}{a vector with m+1 elements, where the first element denotes the logistic of the proportion
of the cured population, and the rest element denotes the log of the hazard rate in intervals}

\item{t}{a vector with non-negative elements.}

\item{piecewiseSurvivalTime}{A vector that specifies the time
intervals for the piecewise exponential survival distribution.
Must start with 0, e.g., c(0, 60) breaks the time axis into 2
event intervals: [0, 60) and [60, Inf). By default, it is set to 0.}
}
\value{
Survival probability of the piecewise-exponential distribution with cured population given parameters \code{par} at times \code{t}.
}
\description{
Calculate survival probability of the piecewise exponential distribution with cured population
}
\references{
\itemize{
\item Chen, Tai-Tsang. "Predicting analysis times in randomized clinical trials with cancer immunotherapy."
BMC medical research methodology 16.1 (2016): 1-10.
}
}
