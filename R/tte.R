#' @title Calculating log-rank test p-value, median time from each arm, hazard ratio between two arms, number of subjects and events in time-to-event outcomes.
#' @description Calculate log-rank test p-value, median time from each arm, HR between arms, number of subjects and events
#'
#' @param os Time to event variable
#' @param osc Time to event censoring variable
#' @param grp Treatment assignment indicator with 1 denoting the treated group, and 0 denoting the placebo group.
#' @param type either "all" or "logrank" or "hr"
#' @param test a character denotes the test type, include "Superiority","Futility","Two-sided"
#' @details
#' The control arm
#'
#' @return If type='all', return a list that includes the log-rank p-value, median time from each arm, hazard ratio between arms,
#' and 95% confidence interval of hazard ratio, number of subjects and events; and events
#' If type='logrank', return a list that includes the log-rank p-value, number of subjects and events;
#' If type='hr', return a list that includes hazard ratio between arms, and 95% confidence interval of hazard ratio
#' 
#'
#' @examples 
#' n <- 500
#' event <- runif(n,1, 5)
#' osc<-1*(event<=4)
#' os <- pmin(event,4)
#' trt<-c(rep(0,n/2),rep(1,n/2))
#' tte(os,osc,trt,type='all')
#'
#' @export
#'
tte<-function(os, osc, grp, type,test='Futility'){
  if(type=="logrank"){
    fit<-survival::survdiff(survival::Surv(os, osc)~ grp, rho=0)
    n_c<-fit$n[1]
    n_a<-fit$n[2]
    evt_c<-fit$obs[1]
    evt_a<-fit$obs[2]

    ### FH Test ###
    fitFH<-FH_test(os,osc, grp,rho=0,gamma=1,test=test)
    pFH<-fitFH$pvalue
    ###log-rank test pvalue
    p<-FH_test(os,osc, grp,rho=0,gamma=0,test=test)$pvalue


    return(list(p=p,n_c=n_c, n_a=n_a, evt_c=evt_c, evt_a=evt_a, pFH=pFH))
  }

  if(type=="all"){
    ### log-rank test ###
    fit<-survival::survdiff(survival::Surv(os, osc)~ grp, rho=0)
    n_c<-as.numeric(fit$n[1])
    n_a<-as.numeric(fit$n[2])
    evt_c<-as.numeric(fit$obs[1])
    evt_a<-as.numeric(fit$obs[2])

    ### median ###
    fit2<-survival::survfit(survival::Surv(os, osc)~ grp )
    osm<-smed(fit2)

    ### HR ###
    fit3<- survival::coxph(survival::Surv(os,osc)~grp)
    hr<-exp(fit3$coef)
    hrlow<-exp(fit3$coef-stats::qnorm(0.975)*sqrt(fit3$var))
    hrup<-exp(fit3$coef+stats::qnorm(0.975)*sqrt(fit3$var))

    ### OS rate ###
    os12rate_a<-fit2[2]$surv[fit2[2]$time==max(fit2[2]$time[fit2[2]$time<=365.25])]
    os12rate_c<-fit2[1]$surv[fit2[1]$time==max(fit2[1]$time[fit2[1]$time<=365.25])]
    os24rate_a<-fit2[2]$surv[fit2[2]$time==max(fit2[2]$time[fit2[2]$time<=730.5])]
    os24rate_c<-fit2[1]$surv[fit2[1]$time==max(fit2[1]$time[fit2[1]$time<=730.5])]
    os48rate_a<-fit2[2]$surv[fit2[2]$time==max(fit2[2]$time[fit2[2]$time<=1461])]
    os48rate_c<-fit2[1]$surv[fit2[1]$time==max(fit2[1]$time[fit2[1]$time<=1461])]

    ### FH Test ###
    fitFH<-FH_test(os,osc, grp,rho=0,gamma=1,test=test)
    pFH<-fitFH$pvalue
    ###log-rank test pvalue
    p<-FH_test(os,osc, grp,rho=0,gamma=0,test=test)$pvalue

    return(list(p=p,n_c=n_c, n_a=n_a, evt_c=evt_c, evt_a=evt_a,
                med_c=osm[1,'median'], med_a=osm[2,'median'], hr=hr, hrlow=hrlow, hrup=hrup,
                os12rate_a=os12rate_a,os12rate_c=os12rate_c,os24rate_a=os24rate_a,
                os24rate_c=os24rate_c, os48rate_a=os48rate_a, os48rate_c=os48rate_c,
                pFH=pFH))
  }


  if(type=="hr"){
    ### HR ###
    fit3<- survival::coxph(survival::Surv(os,osc)~grp)
    hr<-exp(fit3$coef)
    hrlow<-exp(fit3$coef-stats::qnorm(0.975)*sqrt(fit3$var))
    hrup<-exp(fit3$coef+stats::qnorm(0.975)*sqrt(fit3$var))

    return(list(hr=hr, hrlow=hrlow, hrup=hrup))
  }

  if(type=="osrate"){
    ### OS rate ###
    os12rate_a<-fit2[2]$surv[fit2[2]$time==max(fit2[2]$time[fit2[2]$time<=365.25])]
    os12rate_c<-fit2[1]$surv[fit2[1]$time==max(fit2[1]$time[fit2[1]$time<=365.25])]
    os24rate_a<-fit2[2]$surv[fit2[2]$time==max(fit2[2]$time[fit2[2]$time<=730.5])]
    os24rate_c<-fit2[1]$surv[fit2[1]$time==max(fit2[1]$time[fit2[1]$time<=730.5])]
    os48rate_a<-fit2[2]$surv[fit2[2]$time==max(fit2[2]$time[fit2[2]$time<=1461])]
    os48rate_c<-fit2[1]$surv[fit2[1]$time==max(fit2[1]$time[fit2[1]$time<=1461])]

    return(list(
      os12rate_a=os12rate_a,os12rate_c=os12rate_c,os24rate_a=os24rate_a,
      os24rate_c=os24rate_c, os48rate_a=os48rate_a, os48rate_c=os48rate_c))

  }

}

