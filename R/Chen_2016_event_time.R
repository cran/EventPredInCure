#' @title Function to generate event time in the existence of cured population
#'
#' @description generate event time under the delay-treatment effect and cured population setting
#' @param u a scalar with between 0 and 1
#' @param hr hazard ratio if this subject in the corresponding arm vs control arm
#' @param distribution the distribution for the control arm, valid values of inputs include: exponential, weibull, log-normal, log-logistic
#' @param p the proportion of cured population in the control arm
#' @param lag delayed treatment effect time after treatment initialization, when the hr between treatment arm and control =1 until lag time
#' @param a the shape parameter in the Weibull or the log-logistic distribution
#' @param b the scale parameter in the exponential, Weibull or the log-logistic distribution
#' @param mu the mean in the log-normal distribution
#' @param sd the standard deviation in the log-normal distribution
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
Chen_2016_event_time<-function(u,hr,distribution,p,lag,a, b,mu,sd){
  distribution=tolower(distribution)

  if((distribution!="weibull")&(distribution!="log-normal")&(distribution!="log-logistic")&(distribution!="exponential")){
    stop("The input of distribution is invalid. The valid input of distribution can be chosen from the following distributions: 'exponential','weibull', 'log-normal', and 'Log-logistic'.")
  }
  if(!is.numeric(u)|length(u)!=1|u<0|u>1){
    stop('The input of u is invalid. The u should be a scalar between 0 and 1.')
  }

  if(!is.numeric(hr)|length(hr)!=1|hr<=0){
    stop('The input of hr is invalid. The hr should be a positive scalar.')
  }

  if(!is.numeric(p)|length(p)!=1|p<0|p>1){
    stop('The input of p is invalid. The valid value of p should be a scalar between 0 and 1.')
  }
  if(!is.numeric(lag)|length(lag)!=1|lag<0){
    stop('The input of lag is invalid. The valid value of lag should be a a non-negative scalar.')
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
    st0=p+(1-p)*exp(-(lag/b))
    phi0=p/st0
    if(u>=st0){
      t=(-log((u-p)/(1-p)))*b
    }
    if((u/st0)^(1/hr)<=phi0){
      t=10^10
    }

    if(u< st0&(u/st0)^(1/hr)>phi0){
      t=(-log(((u/st0)^(1/hr)-phi0)/(1-phi0))+(lag/b))*b
    }
  }




  if(distribution=='weibull'){
    st0=p+(1-p)*exp(-(lag/b)^a)
    phi0=p/st0
    if(u>=st0){
      t=(-log((u-p)/(1-p)))^(1/a)*b
    }
    if((u/st0)^(1/hr)<=phi0){
      t=10^10
    }

    if(u< st0&(u/st0)^(1/hr)>phi0){
      t=(-log(((u/st0)^(1/hr)-phi0)/(1-phi0))+(lag/b)^a)^(1/a)*b
    }
  }

  if(distribution=='log-normal'){
    st0=p+(1-p)*(1-stats::pnorm((log(lag)-mu)/sd))
    phi0=p/st0
    if(u>=st0){
      t=exp(stats::qnorm(1-(u-p)/(1-p))*sd+mu)
    }
    if((u/st0)^(1/hr)<=phi0){
      t=10^10
    }

    if(u< st0&(u/st0)^(1/hr)>phi0){
      t=exp(stats::qnorm(1-((u/st0)^(1/hr)-phi0)/(1-phi0)*(1-stats::pnorm((log(lag)-mu)/sd)))*sd+mu)
    }
  }

  if(distribution=='log-logistic'){
    st0=p+(1-p)*(1/(1+(lag/b)^a))
    phi0=p/st0
    if(u>=st0){
      t=b*((1-u)/(u-p))^(1/a)
    }
    if((u/st0)^(1/hr)<=phi0){
      t=10^10
    }

    if(u< st0&(u/st0)^(1/hr)>phi0){
      t=b*((1+(lag/b)^a)*(((u/st0)^(1/hr)-phi0)/(1-phi0))^(-1)-1)^(1/a)
    }
  }
  return(t)


}

###
#Chen_2016_event_time(0.6,hr=1,distribution='weibull',p=0.4,lag=3,a=5,b=5)

