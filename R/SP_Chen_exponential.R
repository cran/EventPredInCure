#' @title Survival probability function of the exponential distribution with cured population
#' @description Calculate survival probability of the exponential distribution with cured population
#'
#' @param par a vector with two elements, where the first element denotes the logistic of the proportion
#'  of the cured population, and the second element denotes the log of the hazard rate.
#'
#' @param t a vector with non-negative elements.
#' @return
#' Survival probability of the exponential distribution with cured population given parameters \code{par} at times \code{t}.
#'
#' @references 
#' \itemize{
#' \item Chen, Tai-Tsang. "Predicting analysis times in randomized clinical trials with cancer immunotherapy." 
#' BMC medical research methodology 16.1 (2016): 1-10.
#' }
#' @export
#'
#'
#'
SP_Chen_exponential<-function(par,t){
  p=exp(par[1])/(1+exp(par[1]))
  sigma=exp(-par[2])
  S0=exp(-(t/sigma))
  SP=p+(1-p)*S0
  return(SP)
}
