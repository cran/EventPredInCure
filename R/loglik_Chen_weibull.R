#' @title Log-likelihood function for Weibull distribution with cured population
#' @description Provide log-likelihood function for Weibull distribution with cured population
#'
#' @param df The subject-level event data, including \code{time}
#'   and \code{event}.
#' @param par a vector with three elements, where the first element denotes the logistic of the proportion
#'  of the cured population, and the second element and the third element denote
#'  the log of the shape and log of the scale parameter of the Weibull distribution.
#'
#' @return
#' The negative value of the log-likelihood function given parameter
#' \code{par} and the dataset \code{df}
#'
#'
#' @references 
#' \itemize{
#' \item Chen, Tai-Tsang. "Predicting analysis times in randomized clinical trials with cancer immunotherapy." 
#' BMC medical research methodology 16.1 (2016): 1-10.
#' }
#' 
#'
#' @export
#'
#'
#'
loglik_Chen_weibull<-function(par,df){
  p=exp(par[1])/(1+exp(par[1]))
  a=exp(par[2])
  sigma=exp(par[3])
  delta=df$event
  t=df$time

  f0=a/sigma*(t/sigma)^(a-1)*exp(-(t/sigma)^a)
  S0=exp(-(t/sigma)^a)


  part1=delta*(log(1-p)+log(f0))
  part2=(1-delta)*log(p+(1-p)*S0)
  neg_loglik=-sum(part1+part2)
  return(neg_loglik)
}
