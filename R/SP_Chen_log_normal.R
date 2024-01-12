#' @title Survival probability function of the log-normal distribution with cured population
#' @description Calculate survival probability of the log-normal distribution with cured population
#'
#' @param par a vector with three elements, where the first element denotes the logistic of the proportion
#'  of the cured population, and the second element and the third element denote
#'  the mean and the log of standard deviation parameter of the log-normal distribution.
#' @param t a vector with non-negative elements.
#' @return
#' Survival probability of the log-normal distribution with cured population given parameters \code{par} at times \code{t}.
#'
#' @references \itemize{
#' \item Chen, Tai-Tsang. "Predicting analysis times in randomized clinical trials with cancer immunotherapy." 
#' BMC medical research methodology 16.1 (2016): 1-10.
#' }
#' @references \itemize{
#' \item Chen, Tai-Tsang. "Predicting analysis times in randomized clinical trials with cancer immunotherapy." 
#' BMC medical research methodology 16.1 (2016): 1-10.
#' }
#' 
#' @export
#'
#'
#'
SP_Chen_log_normal<-function(par,t){
  p=exp(par[1])/(1+exp(par[1]))
  mu=par[2]
  sigma=exp(par[3])
  S0=1-stats::pnorm(log(t),mean=mu,sd=sigma)
  SP=p+(1-p)*S0
  return(SP)
}
