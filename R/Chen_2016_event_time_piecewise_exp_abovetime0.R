#' @title Function to generate event time with piecewise exponential distribution for ongoing subject in the existence of cured population
#'
#' @description generate event time under the delay-treatment effect and cured population setting
#' @param u a scalar with between 0 and 1, which is the conditional survival probability at the event time.
#' @param p the proportion of cured population in the control arm
#' @param time0 the observed ongoing survival time
#' @param piecewiseSurvivalTime A vector that specifies the time
#'   intervals for the piecewise exponential survival distribution.
#'   Must start with 0, e.g., c(0, 60) breaks the time axis into 2
#'   event intervals: [0, 60) and [60, Inf). By default, it is set to 0.
#' @param piecewisehazard A vector that specifies the hazard rate in
#'   intervals for the piecewise exponential survival distribution.
#' @return the event time
#'
#' @references 
#' \itemize{
#' \item Chen, Tai-Tsang. "Predicting analysis times in randomized clinical trials with cancer immunotherapy." 
#' BMC medical research methodology 16.1 (2016): 1-10.
#' }
#' 
#' @export
#'
Chen_2016_event_time_piecewise_exp_abovetime0<-function(u,p,time0=0,piecewiseSurvivalTime=0,piecewisehazard){


  if (piecewiseSurvivalTime[1] != 0) {
    stop("piecewiseSurvivalTime must start with 0");
  }
  if (length(piecewiseSurvivalTime) > 1 &
      any(diff(piecewiseSurvivalTime) <= 0)) {
    stop("piecewiseSurvivalTime should be increasing")
  }

  if (length(piecewiseSurvivalTime)!=length(piecewisehazard)) {
    stop("the lengths of piecewiseSurvivalTime and piecewisescale should be the same")
  }


    lambda = piecewisehazard
    J = length(lambda) # number of intervals
    ut = piecewiseSurvivalTime # left end points of the intervals



    # exp(partial sums of lambda*interval_width)
    if (J > 1) {
      psum = c(0, cumsum(lambda[1:(J-1)] * diff(ut)))
    } else {
      psum = 1
    }

    j0 = findInterval(time0, ut)
    st0 = p+(1-p)*exp(-(psum[j0] + lambda[j0]*(time0 - ut[j0])))
    phi0=p/st0
    if(u<=phi0){
      t=10^10

    }else{
      utemp=u*st0
      u0=-log((utemp-p)/(1-p))
      j1 = findInterval(u0, psum)
      t=ut[j1]+(u0-psum[j1])/lambda[j1]
    }

  return(t)


}



