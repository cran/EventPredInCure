#' @title Function to generate event time based on Chen 2016 method for ongoing subject
#'
#' @description generate event time under the delay-treatment effect and cured population setting given T>time0
#' @param u a scalar with between 0 and 1
#' @param distribution the distribution for the control arm, valid values of inputs include: exponential, weibull, log-normal, log-logistic
#' @param p the proportion of cured population in the control arm
#' @param time0 the observed ongoing survival time
#' @param a the shape parameter in the Weibull or the log-logistic distribution
#' @param b the scale parameter in the exponential, Weibull or the log-logistic distribution
#' @param mu the mean in the log-normal distribution
#' @param sd the standard deviation in the log-normal distribution
#' @return the event time
#'
#' @references \itemize{
#' \item Chen, Tai-Tsang. "Predicting analysis times in randomized clinical trials with cancer immunotherapy." 
#' BMC medical research methodology 16.1 (2016): 1-10.
#' }
#' 
#' @export
#'
Chen_2016_event_time_abovetime0<-function(u,distribution,p,time0,a, b,mu,sd){
  distribution=tolower(distribution)

  if((distribution!="weibull")&(distribution!="log-normal")&(distribution!="log-logistic")&(distribution!="exponential")){
    stop("The input of distribution is invalid. The valid input of distribution can be chosen from the following distributions: 'exponential','weibull', 'log-normal', and 'Log-logistic'.")
  }
  if(!is.numeric(u)|length(u)!=1|u<0|u>1){
    stop('The input of u is invalid. The u should be a scalar between 0 and 1.')
  }


  if(!is.numeric(p)|length(p)!=1|p<0|p>1){
    stop('The input of p is invalid. The valid value of p should be a scalar between 0 and 1.')
  }
  if(!is.numeric(time0)|length(time0)!=1|time0<0){
    stop('The input of time0 is invalid. The valid value of time0 should be a postive scalar.')
  }
  if((distribution=="weibull")|(distribution=="log-logistic")){
    if(!is.numeric(a)|length(a)!=1|a<=0){
      stop('The input of a is invalid. The shape paramtere a should be a positive scalar.')
    }
    if(!is.numeric(b)|length(b)!=1|b<=0){
      stop('The input of b is invalid. The scale paramtere b should be a positive scalar.')
    }
  }
  if(distribution=="log-normal"){
    if(!is.numeric(mu)|length(mu)!=1){
      stop('The input of a is invalid. The shape paramtere a should be a numerical scalar.')
    }
    if(!is.numeric(sd)|length(sd)!=1|sd<=0){
      stop('The input of sd is invalid. The scale paramtere sd should be a positive scalar.')
    }
  }


  if(distribution=="exponential"){
    if(!is.numeric(b)|length(b)!=1|b<=0){
      stop('The input of b is invalid. The scale paramtere b should be a positive scalar.')
    }
  }

  if(distribution=='exponential'){
    st0=p+(1-p)*exp(-(time0/b))
    phi0=p/st0
    if(u>phi0){
      u0=u*st0
      t=(-log((u0-p)/(1-p)))*b
    }
    if(u<=phi0){
      t=10^10
    }

  }




  if(distribution=='weibull'){
    st0=p+(1-p)*exp(-(time0/b)^a)
    phi0=p/st0
    if(u>phi0){
      u0=u*st0
      t=(-log((u0-p)/(1-p)))^(1/a)*b
    }
    if(u<=phi0){
      t=10^10
    }

  }

  if(distribution=='log-normal'){
    st0=p+(1-p)*(1-stats::pnorm((log(time0)-mu)/sd))
    phi0=p/st0
    if(u>phi0){
      u0=u*st0
      t=exp(stats::qnorm(1-(u0-p)/(1-p))*sd+mu)
    }
    if(u<=phi0){
      t=10^10
    }
  }

  if(distribution=='log-logistic'){
    st0=p+(1-p)*(1/(1+(time0/b)^a))
    phi0=p/st0


    if(u>phi0){
      u0=u*st0
      t=b*((1-u0)/(u0-p))^(1/a)
    }
    if(u<=phi0){
      t=10^10
    }

  }
  return(t)


}

