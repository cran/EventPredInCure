#' @title Function to calculate survival time and censor variables before and post a time lag (delay treatment effect time)
#' @description calculate survival time and censor variables before and post a time lag.
#'
#' @param os a vector to denote the observed times
#' @param osc a vector to denote censor variables
#' @param lag a scalar to denote the time lag 
#' @return A list including the following variables:
#' \itemize{
#'   \item b4os: overall survival time before the time lag
#'   \item b4osc: censor variable before the time lag
#'   \item pstos: overall survival time post the time lag
#'   \item pstosc: censor variable post the time log lag
#' }
#' 
#' 
#' @examples
#' n <- 500
#' event <- runif(n,1, 5)
#' osc<-1*(event<=4)
#' os <- pmin(event,4)
#' b4pst(os,osc,3.5)
#'
#' @export
#'
b4pst<-function(os, osc, lag){
  tmp1<-cbind(os, lag)
  tmp2<-cbind(os-lag,0)
  b4os<-apply(tmp1,1,min)
  pstos<-apply(tmp2,1,max)
  pstos[pstos==0]<-1	## censored at the start point day 1 ##

  b4osc<-as.numeric(os<=lag)
  pstosc<-osc
  pstosc[os<=lag]<-0

  return(list(b4os=b4os, b4osc=b4osc, pstos=pstos, pstosc=pstosc))
}
