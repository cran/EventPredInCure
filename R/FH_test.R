#' @title Fleming-Harrington weighted log-rank tests
#' @description Calculating the Fleming-Harrington weighted log-rank tests
#'
#'
#' @param survival Time to event or censoring.
#' @param delta Event indicators.
#' @param trt Treatment assignment indicator with 1 denoting the treated group, and 0 denoting the placebo group.
#' @param rho First power parameter for the Fleming-Harrington weight which weighs on the early departures: \eqn{S(t^-)^\rho(1-S(t^-))^\gamma}.
#' @param gamma Second power parameter for the Fleming-Harrington weight which weighs on the late departures: \eqn{S(t^-)^\rho(1-S(t^-))^\gamma}.
#' @param test a character denotes the test type, include "Superiority","Futility","Two-sided"
#' @return A list 3 different components
#'   \item{O1}{Observed number of weighted events (with a multiplication of corresponding weights) in the treatment arm.}
#'   \item{E1 }{Expected number of weighted events (with a multiplication of corresponding weights) in the treatment arm.}
#'   \item{Z}{Weighted log-rank test statistic.}
#'   \item{pvalue}{Weighted log-rank test statistic pvalue}
#'   
#' 
#' @examples 
#' n <- 500
#' event <- runif(n,1, 5)
#' osc<-1*(event<=4)
#' os <- pmin(event,4)
#' trt<-c(rep(0,n/2),rep(1,n/2))
#' FH_test(os,osc,trt,rho=1,gamma=0)
#' 
#' @export
#'


FH_test<- function(survival, delta, trt, rho, gamma,test=c('Futility')){
  ord <- order(survival)
  survival <- survival[ord]
  delta <- delta[ord]
  trt <- trt[ord]
  n <- length(delta)
  if(n != length(survival)) stop("Unequal lengths of survival and delta")
  #delete the last delta=1 observation to avoid the situation of S=0
  if(delta[n] == 1) {
    survival = survival[ - n]
    delta = delta[ - n]
    trt = trt[ - n]
    n = n - 1
  }

  survival = survival
  Surv = c(1, cumprod(1 - delta / (n:1))[ - n])
  Surv.exact = cumprod(1 - delta / (n:1))
  delta = delta
  trt = trt
  Y <- n:1
  P1 <- rev(cumsum(rev(trt))) / Y
  P0 <- 1 - P1
  weight= Surv^rho * (1 - Surv)^gamma
  O1 = trt*delta
  E1 = P1 * delta
  Cov= P1 * P0 * delta
  O1 <- sum(weight * O1)
  E1 <- sum(weight * E1)
  V <- sum(weight^2 * Cov)
  Z<-(O1 - E1) / sqrt(V)
  pvalue<-stats::pnorm(abs(Z),lower.tail = FALSE)*2
  pvalue_greater<-stats::pnorm(Z,lower.tail = FALSE)
  pvalue_less<-stats::pnorm(Z,lower.tail = TRUE)
  if(test=='Futility'){
    return(list(O1=O1,E1=E1,Z=Z,pvalue=pvalue_greater))
  }else if(test=='Superiority'){
    return(list(O1=O1,E1=E1,Z=Z,pvalue=pvalue_less))
  }else if(test=="Two-sided"){
    return(list(O1=O1,E1=E1,Z=Z,pvalue=pvalue))
  }
}

