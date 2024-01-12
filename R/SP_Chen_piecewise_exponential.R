#' @title Survival probability function of the piecewise exponential distribution with cured population
#' @description Calculate survival probability of the piecewise exponential distribution with cured population
#'
#' @param par a vector with m+1 elements, where the first element denotes the logistic of the proportion
#'  of the cured population, and the rest element denotes the log of the hazard rate in intervals
#' @param t a vector with non-negative elements.
#'
#' @param piecewiseSurvivalTime A vector that specifies the time
#'   intervals for the piecewise exponential survival distribution.
#'   Must start with 0, e.g., c(0, 60) breaks the time axis into 2
#'   event intervals: [0, 60) and [60, Inf). By default, it is set to 0.
#'
#' @return
#' Survival probability of the piecewise-exponential distribution with cured population given parameters \code{par} at times \code{t}.
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
SP_Chen_piecewise_exponential<-function(par,t,piecewiseSurvivalTime){

  u = piecewiseSurvivalTime[piecewiseSurvivalTime < max(t)]
  ucut = c(u, max(t))
  J = length(u)
  n=length(t)

  par0=rep(NA,J+2)

  par0[1]=exp(par[1])/(1+exp(par[1]))
  par0[2:(J+1)]=exp(par[2:(J+1)])
  par0[J+2]=exp(par[J+1])

  time_interval_length=diff(ucut)

  sum_interval=c(0,cumsum((par0[2:(J+1)])*time_interval_length))
  interval_index=rep(NA,n)

  for(i in 1:n){
    interval_index[i]=findInterval(t[i],ucut)
  }

  p=par0[1]
  S0=exp(-sum_interval[interval_index]-(t-ucut[interval_index])*par0[interval_index+1])

  SP=p+(1-p)*S0


  return(SP)
}



