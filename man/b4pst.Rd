% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/b4pst.R
\name{b4pst}
\alias{b4pst}
\title{Function to calculate survival time and censor variables before and post a time lag (delay treatment effect time)}
\usage{
b4pst(os, osc, lag)
}
\arguments{
\item{os}{a vector to denote the observed times}

\item{osc}{a vector to denote censor variables}

\item{lag}{a scalar to denote the time lag}
}
\value{
A list including the following variables:
\itemize{
\item b4os: overall survival time before the time lag
\item b4osc: censor variable before the time lag
\item pstos: overall survival time post the time lag
\item pstosc: censor variable post the time log lag
}
}
\description{
calculate survival time and censor variables before and post a time lag.
}
\examples{
n <- 500
event <- runif(n,1, 5)
osc<-1*(event<=4)
os <- pmin(event,4)
b4pst(os,osc,3.5)

}
