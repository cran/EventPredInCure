#' @title Function to provide summary and test statistics based on simulation.
#' @description Provides summary and test statistics based on simulation.
#' @param pilevel the confidence interval, the default is 0.95.
#' @param nyears the year after data cutoff or follow-up.
#' @param enroll_fit an object generated from \code{fitEnrollment}.
#' @param dropout_fit an object generated from \code{fitDropout}.
#' @param enroll_prior The prior of enrollment model parameters.
#' @param event_prior_h0 The prior of event model parameters under null hypothesis
#' @param event_prior_ha The prior of event model parameters under alternative hypothesis
#' @param dropout_prior The prior of dropout model parameters.
#' @param target_n The target number of subjects to enroll in the study.
#' @param target_IA_d number of events needed for interim analysis
#' @param target_d number of events needed for primary analysis
#' @param ialpha interim analysis alpha nominal value (only one interim allowed)
#' @param falpha primary analysis alpha nominal value
#' @param lag a scalar to denote time (days). Hazard ratio before and after this time would be calculated.
#' @param by_fitted_enroll A Boolean variable to control whether or not to
#'   predict enrollment time with fitted model. By default, it is set to \code{FALSE}.
#' @param by_fitted_dropout A Boolean variable to control whether or not to
#'   predict dropout time with fitted model. By default, it is set to \code{FALSE}.
#' @param treatment_label The treatment labels for treatments in a
#'   randomization block for design stage prediction.
#' @param ngroups The number of treatment groups for enrollment prediction
#'   at the design stage. By default, it is set to 2.
#'   It is replaced with the actual number of
#'   treatment groups in the observed data if \code{df} is not \code{NULL}.
#' @param alloc The treatment allocation in a randomization block.
#'   By default, it is set to \code{NULL}, which yields equal allocation
#'   among the treatment groups.
#' @param nreps The number of replications for simulation. By default,
#'   it is set to 500.
#'@param IA_included A Boolean variable to control whether or not to
#'   include one interim analysis. By default, it is set to \code{FALSE}.
#' @param test a character denotes the test type, includes "Superiority","Futility","Two-sided"
#'
#' @param test_IA a character denotes the test type in interim analysis, includes "Efficacy","Futility",or "Efficacy and Futility"
#' @param Futility_boundary a positive number denotes the boundary of the Futility in the scale of hazard ratio
#' @param seed.num The number of the random seed. The default is NULL.
#' 
#' @returns  A list with following components
#' \itemize{
#'   \item iteration0 - the number of simulations that achieved target number of events in interim analysis and primary analysis under null hypothesis.
#'   \item iteration1 - the number of simulations that achieved target number of events in interim analysis and primary analysis under alternative hypothesis.
#'   \item simu_summary - the summary table of number of simulations that achieved or do not achieve target number of events in each analysis under null hypothesis and alternative hypothesis.
#'   \item power - the alpha and powers from log-rank test and Fleming&Harrington test
#'   \item samplesize - the counts per arm and total, includes the number of patients,
#'   events at interim analysis and primary analysis, events at interim analysis before delay,
#'    events at primary analysis before delay
#'    \item hzratio - average hazard ratio at primary analysis and interim analysis
#' \item hzrc - Frequencies of hazard ratios in specific zones
#' \item hzratio2 - HR before and after delay
#' \item median - Medians of survival time at interim analysis and primary analysis
#' \item osrate - survival rate at milestones (1, 2 and 4 years)
#' \item duration - Study duration for enrollment, interim analysis and primary analysis
#' \item duration1 - Durations for interim analysis and primary analysis with 95% CI
#' \item textH0 - the summary text of number of simulations that achieved or do not achieve target number of events in primary analysis under null hypothesis.
#' \item textHA - the summary text of number of simulations that achieved or do not achieve target number of events in primary analysis under alternative hypothesis.
#' }
#'
#'
#' @examples
#' \donttest{
#' fit1 <- list(model = "piecewise uniform",
#'              theta = -0.58, 
#'              vtheta=0, accrualTime =0)
#' fit2<-list()
#' fit2[[1]] <- list(model = "weibull with cured population and delayed treatment", 
#'                   theta = c(-2.2,0,6.5,0,1), 
#'                   vtheta = matrix(0,5,5))
#' fit2[[2]] <- list(model = "weibull with cured population and delayed treatment", 
#'                  theta = c(-2.2,0,6.5,46,0.65), 
#'                  vtheta = matrix(0,5,5))
#' fit3 <-list()
#' fit3[[1]] <- list(model = "weibull with cured population and delayed treatment", 
#'                   theta = c(-2.2,0,6.5,0,1), 
#'                   vtheta = matrix(0,5,5))
#' fit3[[2]] <- list(model = "weibull with cured population and delayed treatment", 
#'                   theta = c(-2.2,0,6.5,0,1),
#'                   vtheta = matrix(0,5,5))
#' fit4 <-list()
#' 
#' fit4[[1]] <- list(model = "exponential", 
#'                    theta =log(0.0003), 
#'                    vtheta=0)
#' fit4[[2]] <- list(model = "exponential", 
#'                    theta =log(0.0003), 
#'
#'                    vtheta=0)
#' test1<-test_procedure(pilevel=0.9,nyears=4,enroll_fit=fit1,
#'                       dropout_fit=fit4,enroll_prior=fit1,event_prior_h0=fit3,
#'                       event_prior_ha=fit2,dropout_prior=NULL,
#'                       target_n=200,target_IA_d=40,target_d=60,
#'                       ialpha=0.016,falpha=0.0450,
#'                       lag=46,by_fitted_enroll=FALSE,
#'                       by_fitted_dropout=FALSE,treatment_label=c('a','b'),
#'                       ngroups=2,alloc=c(1,1),nreps=100, IA_included=TRUE)
#'}                       
#' @export
#'
test_procedure<-function(pilevel=0.9,
                         nyears=4,
                         enroll_fit=enroll_fit,
                         dropout_fit=dropout_fit,
                         enroll_prior=NULL,
                         event_prior_h0=NULL,
                         event_prior_ha=NULL,
                         dropout_prior=NULL,
                         target_n,
                         target_IA_d,target_d,
                         ialpha=0.025,
                         falpha,
                         lag,by_fitted_enroll=FALSE, by_fitted_dropout=FALSE,
                         treatment_label,
                         ngroups=2,
                         alloc=NULL,
                         nreps=500, IA_included,
                         test='Superiority',test_IA='Superiority',Futility_boundary=1,seed.num=NULL){

  w=alloc/sum(alloc)

  ####check specific parameters in the test####
  lag0=lag
  lag=lag 
  if(IA_included){
    if (!is.na(target_IA_d)) erify::check_n(target_IA_d)
  }else{
    target_IA_d=target_d
    ialpha=falpha
  }

  if (!is.na(target_d)) erify::check_n(target_d)


  if(!is.numeric(falpha)|length(falpha)!=1|falpha<0|falpha>1){
    stop('The input of falpha is invalid. The falpha should be a scalar between 0 and 1.')
  }


  if(ngroups!=2){
    stop('The test part only support the test between two arms')
  }
  if(by_fitted_enroll&&is.null(enroll_fit)){
    stop('No fitted enrollment model.')
  }
  if(by_fitted_dropout&&is.null(dropout_fit)){
    stop('No fitted dropout model.')
  }

  if (!is.na(target_n)) erify::check_n(target_n)
  if (!is.na(target_d)) erify::check_n(target_d)
  if (is.na(target_n) && is.na(target_d))
    stop("At least one of target_n and target_d must be specified.")
  if (!is.na(target_n) && !is.na(target_d) && target_d > target_n)
    stop("target_d cannot exceed target_n.")

  ###check input model parameters
  if (!is.null(enroll_prior)) {
    erify::check_class(enroll_prior, "list")
    erify::check_content(tolower(enroll_prior$model), c(
      "poisson", "time-decay", "piecewise poisson", "piecewise uniform"))

    model = tolower(enroll_prior$model)
    p = length(enroll_prior$theta)
    vtheta = enroll_prior$vtheta

    if ((p > 1 && (!is.matrix(vtheta) || nrow(vtheta) != p ||
                   ncol(vtheta) != p)) ||
        (p == 1 && length(c(vtheta)) != 1)) {
      stop(paste("Dimensions of vtheta must be compatible with the length",
                 "of theta in enroll_prior"))
    }

    if ((model == "poisson" && p != 1) ||
        (model == "time-decay" && p != 2) ||
        (model == "piecewise poisson" &&
         p != length(enroll_prior$accrualTime))||
        (model == "piecewise uniform" &&
         p != length(enroll_prior$accrualTime))) {
      stop(paste("Length of theta must be compatible with model",
                 "in enroll_prior"))
    }

    if (model == "piecewise poisson"||model =="piecewise uniform") {
      if (enroll_prior$accrualTime[1] != 0) {
        stop("accrualTime must start with 0 in enroll_prior")
      }
      if (length(enroll_prior$accrualTime) > 1 &&
          any(diff(enroll_prior$accrualTime) <= 0)) {
        stop("accrualTime should be increasing in enroll_prior")
      }
    }
  }









  # check event model prior under H0
  if (!is.null(event_prior_h0)) {
    erify::check_class(event_prior_h0, "list")


    if (length(event_prior_h0) != ngroups) {
      stop("event_prior_h0 must be a list with one element per treatment.")
    }


    # convert to a list with one element per treatment
    if ("model" %in% names(event_prior_h0)) {
      event_prior_h02 <- list()
      event_prior_h02[[1]] <- event_prior_h0
    } else {
      event_prior_h02 <- event_prior_h0
    }

    for (j in 1:length(event_prior_h02)) {
      erify::check_content(tolower(event_prior_h02[[j]]$model),
                           c("exponential", "weibull", "log-logistic",
                             "log-normal", "piecewise exponential",
                             "weibull with cured population","exponential with cured population","log-normal with cured population",
                             "log-logistic with cured population","piecewise exponential with cured population",
                             "exponential with cured population and delayed treatment","weibull with cured population and delayed treatment",
                             "log-normal with cured population and delayed treatment","log-logistic with cured population and delayed treatment"))

      model = tolower(event_prior_h02[[j]]$model)
      p = length(event_prior_h02[[j]]$theta)
      vtheta = event_prior_h02[[j]]$vtheta

      if ((p > 1 && (!is.matrix(vtheta) || nrow(vtheta) != p ||
                     ncol(vtheta) != p)) ||
          (p == 1 && length(c(vtheta)) != 1)) {
        stop(paste("Dimensions of vtheta must be compatible with",
                   "the length of theta in event_prior_h0"))
      }

      if ((model == "exponential" && p != 1) ||
          (model == "weibull" && p != 2) ||
          (model == "log-logistic" && p != 2) ||
          (model == "log-normal" && p != 2) ||
          (model == "piecewise exponential" &&
           p != length(event_prior_h02[[j]]$piecewiseSurvivalTime))||
          (model == "exponential with cured population" && p != 2) ||
          (model == "weibull with cured population" && p != 3) ||
          (model == "log-logistic with cured population" && p != 3) ||
          (model == "log-normal with cured population" && p != 3) ||
          (model == "exponential with cured population and delayed treatment" && p != 4) ||
          (model == "weibull with cured population and delayed treatment" && p != 5) ||
          (model == "log-logistic with cured population and delayed treatment" && p != 5) ||
          (model == "log-normal with cured population and delayed treatment" && p != 5) ||
          (model == "piecewise exponential with cured population" &&
           p != (length(event_prior_h02[[j]]$piecewiseSurvivalTime)+1))
      ) {
        stop(paste("Length of theta must be compatible with model",
                   "in event_prior_h0"))
      }

      if (model == "piecewise exponential"||model == "piecewise exponential with cured population") {
        if (event_prior_h02[[j]]$piecewiseSurvivalTime[1] != 0) {
          stop(paste("piecewiseSurvivalTime must start with 0",
                     "in event_prior_h0"))
        }
        if (length(event_prior_h02[[j]]$piecewiseSurvivalTime) > 1 &&
            any(diff(event_prior_h02[[j]]$piecewiseSurvivalTime) <= 0)) {
          stop(paste("piecewiseSurvivalTime should be increasing",
                     "in event_prior_h0"))
        }
      }
    }
  }


  # check event model prior under HA
  if (!is.null(event_prior_ha)) {
    erify::check_class(event_prior_ha, "list")


    if (length(event_prior_ha) != ngroups) {
      stop("event_prior_ha must be a list with one element per treatment.")
    }


    # convert to a list with one element per treatment
    if ("model" %in% names(event_prior_ha)) {
      event_prior_ha2 <- list()
      event_prior_ha2[[1]] <- event_prior_ha
    } else {
      event_prior_ha2 <- event_prior_ha
    }

    for (j in 1:length(event_prior_ha2)) {
      erify::check_content(tolower(event_prior_ha2[[j]]$model),
                           c("exponential", "weibull", "log-logistic",
                             "log-normal", "piecewise exponential",
                             "weibull with cured population","exponential with cured population","log-normal with cured population",
                             "log-logistic with cured population","piecewise exponential with cured population",
                             "exponential with cured population and delayed treatment","weibull with cured population and delayed treatment",
                             "log-normal with cured population and delayed treatment","log-logistic with cured population and delayed treatment"))

      model = tolower(event_prior_ha2[[j]]$model)
      p = length(event_prior_ha2[[j]]$theta)
      vtheta = event_prior_ha2[[j]]$vtheta

      if ((p > 1 && (!is.matrix(vtheta) || nrow(vtheta) != p ||
                     ncol(vtheta) != p)) ||
          (p == 1 && length(c(vtheta)) != 1)) {
        stop(paste("Dimensions of vtheta must be compatible with",
                   "the length of theta in event_prior_ha"))
      }

      if ((model == "exponential" && p != 1) ||
          (model == "weibull" && p != 2) ||
          (model == "log-logistic" && p != 2) ||
          (model == "log-normal" && p != 2) ||
          (model == "piecewise exponential" &&
           p != length(event_prior_ha2[[j]]$piecewiseSurvivalTime))||
          (model == "exponential with cured population" && p != 2) ||
          (model == "weibull with cured population" && p != 3) ||
          (model == "log-logistic with cured population" && p != 3) ||
          (model == "log-normal with cured population" && p != 3) ||
          (model == "exponential with cured population and delayed treatment" && p != 4) ||
          (model == "weibull with cured population and delayed treatment" && p != 5) ||
          (model == "log-logistic with cured population and delayed treatment" && p != 5) ||
          (model == "log-normal with cured population and delayed treatment" && p != 5) ||
          (model == "piecewise exponential with cured population" &&
           p != (length(event_prior_ha2[[j]]$piecewiseSurvivalTime)+1))
      ) {
        stop(paste("Length of theta must be compatible with model",
                   "in event_prior_ha"))
      }

      if (model == "piecewise exponential"||model == "piecewise exponential with cured population") {
        if (event_prior_ha2[[j]]$piecewiseSurvivalTime[1] != 0) {
          stop(paste("piecewiseSurvivalTime must start with 0",
                     "in event_prior_ha"))
        }
        if (length(event_prior_ha2[[j]]$piecewiseSurvivalTime) > 1 &&
            any(diff(event_prior_ha2[[j]]$piecewiseSurvivalTime) <= 0)) {
          stop(paste("piecewiseSurvivalTime should be increasing",
                     "in event_prior_ha"))
        }
      }
    }
  }





  # check dropout model prior
  if (!is.null(dropout_prior)) {
    erify::check_class(dropout_prior, "list")


    if (length(dropout_prior) != ngroups) {
      stop("dropout_prior must be a list with one element per treatment.")
    }


    # convert to a list with one element per treatment
    if ("model" %in% names(dropout_prior)) {
      dropout_prior2 <- list()
      dropout_prior2[[1]] <- dropout_prior
    } else {
      dropout_prior2 <- dropout_prior
    }

    for (j in 1:length(dropout_prior2)) {
      erify::check_content(tolower(dropout_prior2[[j]]$model),
                           c("exponential", "weibull", "log-logistic",
                             "log-normal", "piecewise exponential"))

      model = tolower(dropout_prior2[[j]]$model)
      p = length(dropout_prior2[[j]]$theta)
      vtheta = dropout_prior2[[j]]$vtheta

      if ((p > 1 && (!is.matrix(vtheta) || nrow(vtheta) != p ||
                     ncol(vtheta) != p)) ||
          (p == 1 && length(c(vtheta)) != 1)) {
        stop(paste("Dimensions of vtheta must be compatible with",
                   "the length of theta in dropout_prior"))
      }

      if ((model == "exponential" && p != 1) ||
          (model == "weibull" && p != 2) ||
          (model == "log-logistic" && p != 2) ||
          (model == "log-normal" && p != 2) ||
          (model == "piecewise exponential" &&
           p != length(dropout_prior2[[j]]$piecewiseDropoutTime))) {
        stop(paste("Length of theta must be compatible with model",
                   "in event_prior"))
      }

      if (model == "piecewise exponential") {
        if (dropout_prior2[[j]]$piecewiseDropoutTime[1] != 0) {
          stop(paste("piecewiseDropoutTime must start with 0",
                     "in dropout_prior"))
        }
        if (length(dropout_prior2[[j]]$piecewiseDropoutTime) > 1 &&
            any(diff(dropout_prior2[[j]]$piecewiseDropoutTime) <= 0)) {
          stop(paste("piecewiseDropoutTime should be increasing",
                     "in dropout_prior"))
        }
      }



      if (!is.null(event_prior_ha) && "w" %in% names(event_prior_ha2[[j]])) {
        if (event_prior_ha2[[j]]$w != dropout_prior2[[j]]$w) {
          stop("w must be equal between event prior and dropout prior.")
        }
      }
    }
  }



  ####prediction part####



  if (!is.na(target_n)) erify::check_n(target_n)
  if (is.na(target_n) && is.na(target_d))
    stop("At least one of target_n and target_d must be specified.")
  if (!is.na(target_n) && !is.na(target_d) && target_d > target_n)
    stop("target_d cannot exceed target_n.")






  if(by_fitted_enroll&&by_fitted_dropout){
    dropout_fit1<-list()
    for(i in 1:ngroups){
      dropout_fit1[[i]]<- append(dropout_fit, list(w=w[i]))
    }
    pred_all_H0<-getPrediction(df = NULL,
                                    to_predict = "enrollment and event",
                                    target_n=target_n ,
                                    target_d=target_d,
                                    enroll_prior = enroll_fit,
                                    event_prior = event_prior_h0,
                                    dropout_prior =dropout_fit1,
                                    pilevel=pilevel,
                                    nyears=nyears,
                                    nreps=nreps,
                                    showEnrollment=TRUE, showEvent=TRUE,
                                    showDropout=FALSE, showOngoing=FALSE,
                                    by_treatment=TRUE,
                                    ngroups=ngroups,
                                    alloc=alloc,
                                    treatment_label=treatment_label,
                                    criterion="both",seed.num=seed.num)

    pred_all_HA<-getPrediction(df = NULL,
                                    to_predict = "enrollment and event",
                                    target_n=target_n ,
                                    target_d=target_d,
                                    enroll_prior = enroll_fit,
                                    event_prior = event_prior_ha,
                                    dropout_prior =dropout_fit1,
                                    pilevel=pilevel,
                                    nyears=nyears,
                                    nreps=nreps,
                                    showEnrollment=TRUE, showEvent=TRUE,
                                    showDropout=FALSE, showOngoing=FALSE,
                                    by_treatment=TRUE,
                                    ngroups=ngroups,
                                    alloc=alloc,
                                    treatment_label=treatment_label,
                                    criterion="both",seed.num=seed.num)

  } else if(by_fitted_enroll&&(!by_fitted_dropout)){
    pred_all_H0<-getPrediction(df = NULL,
                                    to_predict = "enrollment and event",
                                    target_n=target_n ,
                                    target_d=target_d,
                                    enroll_prior = enroll_fit,
                                    event_prior = event_prior_h0,
                                    dropout_prior =dropout_prior,
                                    pilevel=pilevel,
                                    nyears=nyears,
                                    nreps=nreps,
                                    showEnrollment=TRUE, showEvent=TRUE,
                                    showDropout=FALSE, showOngoing=FALSE,
                                    by_treatment=TRUE,
                                    ngroups=ngroups,
                                    alloc=alloc,
                                    treatment_label=treatment_label,
                                    criterion="both",seed.num=seed.num)


    pred_all_HA<-getPrediction(df = NULL,
                                    to_predict = "enrollment and event",
                                    target_n=target_n ,
                                    target_d=target_d,
                                    enroll_prior = enroll_fit,
                                    event_prior = event_prior_ha,
                                    dropout_prior =dropout_prior,
                                    pilevel=pilevel,
                                    nyears=nyears,
                                    nreps=nreps,
                                    showEnrollment=TRUE, showEvent=TRUE,
                                    showDropout=FALSE, showOngoing=FALSE,
                                    by_treatment=TRUE,
                                    ngroups=ngroups,
                                    alloc=alloc,
                                    treatment_label=treatment_label,
                                    criterion="both",seed.num=seed.num)
  } else if((!by_fitted_enroll)&&by_fitted_dropout){

    dropout_fit1<-list()
    for(i in 1:ngroups){
      dropout_fit1[[i]]<- append(dropout_fit, list(w=w[i]))
    }

    pred_all_H0<-getPrediction(df = NULL,
                                    to_predict = "enrollment and event",
                                    target_n=target_n ,
                                    target_d=target_d,
                                    enroll_prior = enroll_prior,
                                    event_prior = event_prior_h0,
                                    dropout_prior =dropout_fit1,
                                    pilevel=pilevel,
                                    nyears=nyears,
                                    nreps=nreps,
                                    showEnrollment=TRUE, showEvent=TRUE,
                                    showDropout=FALSE, showOngoing=FALSE,
                                    by_treatment=TRUE,
                                    ngroups=ngroups,
                                    alloc=alloc,
                                    treatment_label=treatment_label,
                                    criterion="both",seed.num=seed.num)

    pred_all_HA<-getPrediction(df = NULL,
                                    to_predict = "enrollment and event",
                                    target_n=target_n ,
                                    target_d=target_d,
                                    enroll_prior = enroll_prior,
                                    event_prior = event_prior_ha,
                                    dropout_prior =dropout_fit1,
                                    pilevel=pilevel,
                                    nyears=nyears,
                                    nreps=nreps,
                                    showEnrollment=TRUE, showEvent=TRUE,
                                    showDropout=FALSE, showOngoing=FALSE,
                                    by_treatment=TRUE,
                                    ngroups=ngroups,
                                    alloc=alloc,
                                    treatment_label=treatment_label,
                                    criterion="both",seed.num=seed.num)

  } else if((!by_fitted_enroll)&&(!by_fitted_dropout)){

    pred_all_H0<-getPrediction(df = NULL,
                                    to_predict = "enrollment and event",
                                    target_n=target_n ,
                                    target_d=target_d,
                                    enroll_prior = enroll_prior,
                                    event_prior = event_prior_h0,
                                    dropout_prior =dropout_prior,
                                    pilevel=pilevel,
                                    nyears=nyears,
                                    nreps=nreps,
                                    showEnrollment=TRUE, showEvent=TRUE,
                                    showDropout=FALSE, showOngoing=FALSE,
                                    by_treatment=TRUE,
                                    ngroups=ngroups,
                                    alloc=alloc,
                                    treatment_label=treatment_label,
                                    criterion="both",seed.num=seed.num)
    pred_all_HA<-getPrediction(df = NULL,
                                    to_predict = "enrollment and event",
                                    target_n=target_n ,
                                    target_d=target_d,
                                    enroll_prior = enroll_prior,
                                    event_prior = event_prior_ha,
                                    dropout_prior =dropout_prior,
                                    pilevel=pilevel,
                                    nyears=nyears,
                                    nreps=nreps,
                                    showEnrollment=TRUE, showEvent=TRUE,
                                    showDropout=FALSE, showOngoing=FALSE,
                                    by_treatment=TRUE,
                                    ngroups=ngroups,
                                    alloc=alloc,
                                    treatment_label=treatment_label,
                                    criterion="both",seed.num=seed.num)

  }





  if(IA_included){

   
    ###elements to save simulation results
    iterFH<-0
    iterFH_h0<-0
    os_pw<-rep(0,nreps)
    os_pFH<-rep(0,nreps)
    p1<-p2<-pFH1<-pFH2<-0
    p1f<-p1f_FH<-0

    p1_FH_h0<-0
    p1f_FH_h0<-0
    p2_FH_h0<-0
    cross_IA<-0
    cross_IA_FH<-0
    cross_IA_h0<-0
    cross_IA_FH_h0<-0
    p1_h0<-p2_h0<-0
    p1f_h0<-p2f_h0<-0
    hrle8<-hrgt8le9<-hrgt9<-0
    nA<-nC<-os_evta<-os_evtc<-rep(0,nreps)
    os_hr<-os_mediana<-os_medianc<-rep(0,nreps)

    b4_os_hr<-pst_os_hr<-b4_os_hrup<-b4_os_hrlow<-pst_os_hrup<-pst_os_hrlow<-rep(0,nreps)
    b4_eventsai<-b4_eventsci<-rep(0,nreps)

    os_pwh0<-os_pwh0i<-rep(0,nreps)

    b4_eventsa<-b4_eventsc<-rep(0,nreps)
    b4lag_eventsa<-b4lag_eventsc<-rep(0,nreps)

    os_itmediana<-os_itmedianc<-rep(0,nreps)

    os_duration<-interim_timing<-os_itpw<-os_itpFH<-os_ithr<-os_ithr0<-os_itpFH_h0<-os_pFHh0<-os_itevta<-os_itevtc<-rep(0,nreps)

    maxHR<-maxHR1<-maxHR2<-rep(0,nreps)

    pst_osi<-pst_osic<-b4_oscensori<-rep(0,target_n)
    pst_oscensori<-rep(0,target_n)

    os12_a<-os12_c<-os24_a<-os24_c<-os48_a<-os48_c<-rep(0,nreps)


    accrualtime=rep(NA,nreps)


    i1_IA=0 ##number of simulations achieve target number of event in IA under HA
    i1=0 ##number of simulations achieve target number of events in both IA and FA under HA
    e1=rep(0,nreps) ##indicator of whether this simulation achieve the target number of events in both IA and FA under HA
    for(i in 1:nreps){
      #####
      a0<-pred_all_HA$event_pred$newEvents
      a1<-a0[a0$draw==i,]
      accrualtime[i]=max(a1$arrivalTime)
      grph1<-1*(a1$treatment==2)
      event_num=sum(a1$event)
      if(target_IA_d<=event_num){
        i1_IA=i1_IA+1
      }
      if(target_IA_d<=event_num&&target_d<=event_num){
        i1=i1+1
        e1[i]=1
      }
    
  
      
      if(e1[i]==1){
        IA_time_cut<-sort(a1$totalTime[a1$event==1])[target_IA_d]
        FA_time_cut<-sort(a1$totalTime[a1$event==1])[target_d]
        data_IA<-a1[a1$arrivalTime<=IA_time_cut,]
        data_FA<-a1[a1$arrivalTime<=FA_time_cut,]
        IA_censor<-1*(data_IA$event&data_IA$totalTime<=IA_time_cut)
        FA_censor<-1*(data_FA$event&data_FA$totalTime<=FA_time_cut)
        IA_surv_time<-pmin(data_IA$totalTime,IA_time_cut)-data_IA$arrivalTime
        FA_surv_time<-pmin(data_FA$totalTime,FA_time_cut)-data_FA$arrivalTime
        grph1f<-grph1[a1$arrivalTime<=FA_time_cut]
        grph1i<-grph1[a1$arrivalTime<=IA_time_cut]
        
        out<-b4pst(os=FA_surv_time,osc=FA_censor,lag=lag)
        b4os<-out$b4os
        b4osc<-out$b4osc
        pstos<-out$pstos
        pstosc<-out$pstosc
        
        out<-b4pst(os=IA_surv_time,osc=IA_censor,lag=lag)
        b4osi<-out$b4os
        b4osci<-out$b4osc
        pstosi<-out$pstos
        pstosci<-out$pstosc
        
        
        ### calculate HR before and after lag to verify assumptions ###
        ### including HR and its 95% confidence interval                         ###
        
        b4out<-tte(b4os, b4osc, grph1f,"hr",test=test)
        b4_os_hr[i]<-b4out$hr
        b4_os_hrlow[i]<-b4out$hrlow
        b4_os_hrup[i]<-b4out$hrup
        
        pstout<-tte(pstos, pstosc, grph1f,"hr",test=test)
        pst_os_hr[i]<-pstout$hr
        pst_os_hrlow[i]<-pstout$hrlow
        pst_os_hrup[i]<-pstout$hrup
        
        
        
        
        ### calculate HR before and after lag to verify assumptions ###
        ### including HR and its 95% confidence interval                         ###
        
        b4out<-tte(b4os, b4osc, grph1f,"hr",test=test)
        b4_os_hr[i]<-b4out$hr
        b4_os_hrlow[i]<-b4out$hrlow
        b4_os_hrup[i]<-b4out$hrup
        
        pstout<-tte(pstos, pstosc, grph1f,"hr",test=test)
        pst_os_hr[i]<-pstout$hr
        pst_os_hrlow[i]<-pstout$hrlow
        pst_os_hrup[i]<-pstout$hrup
        
        
        
        
        b4_eventsa[i]<-sum(b4osc[grph1f==1])
        b4_eventsc[i]<-sum(b4osc[grph1f==0])
        
        
        
        
        ###primary OS log rank test and COX model under Ha ####
        
        tteout<-tte(FA_surv_time, FA_censor, grph1f,"all",test=test)
        os_pw[i]<-tteout$p
        os_mediana[i]<-tteout$med_a
        os_medianc[i]<-tteout$med_c
        os_evta[i]<-tteout$evt_a
        os_evtc[i]<-tteout$evt_c
        nA[i]<-tteout$n_a
        nC[i]<-tteout$n_c
        os_hr[i]<-tteout$hr
        
        os12_a[i]<-tteout$os12rate_a
        os12_c[i]<-tteout$os12rate_c
        os24_a[i]<-tteout$os24rate_a
        os24_c[i]<-tteout$os24rate_c
        os48_a[i]<-tteout$os48rate_a
        os48_c[i]<-tteout$os48rate_c
        
        os_pFH[i]<-tteout$pFH
        
        
        
        
        ## calulating number of events before lag for interim analysis by arm ##
        
        b4_eventsai[i]<-sum(IA_surv_time[grph1i==1 & IA_censor==1]<=(lag))
        b4_eventsci[i]<-sum(IA_surv_time[grph1i==0 & IA_censor==1]<=(lag))
        
        ### Interim OS log rank test and COX model under Ha####
        
        
        tteouti<-tte(IA_surv_time, IA_censor, grph1i,"all",test='Superiority')
        os_itpw[i]<-tteouti$p
        os_itmediana[i]<-tteouti$med_a
        os_itmedianc[i]<-tteouti$med_c
        os_itevta[i]<-tteouti$evt_a
        os_itevtc[i]<-tteouti$evt_c
        os_ithr[i]<-tteouti$hr
        os_itpFH[i]<-tteouti$pFH
        
        ### count number of HRs in ranges of HR<=0.8, 0.8<HR<=0.9, HR>0.9 for hrle8 hrgt8le9 hrgt9
        
        if(os_hr[i]<=0.8){
          hrle8<-hrle8+1}  # counting how many HRs <= 0.8 at PA
        
        if(os_hr[i]>0.8 & os_hr[i]<=0.9){
          hrgt8le9<-hrgt8le9+1}  # counting how many HRs <= 0.8 at PA
        
        if(os_hr[i]>0.9){
          hrgt9<-hrgt9+1}  # counting how many HRs <= 0.8 at PA
        
        
        
        
        ## need to figure out the study duration  ##
        os_duration[i]<- FA_time_cut
        interim_timing[i]<-IA_time_cut
        
        ### section for power calculation under Ha ###
        stop_IA_ind=0
        if(test_IA=='Efficacy'||test_IA=='Efficacy and Futility'){
          if (os_itpw[i]<ialpha){
            p1<-p1+1  # counting how many superiority trials under Ha at interim
            stop_IA_ind=1
          }
        }
        if(test_IA=='Futility'||test_IA=='Efficacy and Futility'){
          if(os_ithr[i]>Futility_boundary){
            p1f<-p1f+1 # counting how many Futility trials under Ha at interim
            stop_IA_ind=1
          }
        }
        if(test_IA=='Efficacy and Futility'){
          if(os_ithr[i]>Futility_boundary && os_itpw[i]<ialpha){
            cross_IA=1
          }
        }
        
        
        if (stop_IA_ind==0 & os_pw[i]<falpha ){
          p2<-p2+1  # counting how many superiority trials under Ha at PA
        }
        
        stop_IA_ind_FH=0
        
        if (!is.na(os_itpFH[i])) {
          iterFH<-iterFH+1
          
          if(test_IA=='Efficacy'||test_IA=='Efficacy and Futility'){
            if (os_itpFH[i]<ialpha){
              
              pFH1<-pFH1+1  # counting how many superiority trials under Ha at interim
              stop_IA_ind_FH=1
            }
          }
          
          if(test_IA=='Futility'||test_IA=='Efficacy and Futility'){
            if(os_ithr[i]>Futility_boundary){
              p1f_FH<-p1f_FH+1 # counting how many Futility trials under Ha at interim
              stop_IA_ind_FH=1
            }
          }
          
          if(test_IA=='Efficacy and Futility'){
            if(os_ithr[i]>Futility_boundary && os_itpFH[i]<ialpha){
              cross_IA_FH=1
            }
          }
          
          
          
          if (stop_IA_ind_FH==0 & os_pFH[i]<falpha ){
            
            pFH2<-pFH2+1  # counting how many superiority trials under Ha at PA
          }
        }
        

        
        
        
        
      }
      
      


    }
    
    
    totalpw<-p1+p2        ## total power
    IA_p1f<-p1f           ##Futility under IA
    totalpwFH<-pFH1+pFH2  ## total power for FH Test





    ###under H0####
    i0_IA=0 ##number of simulations achieve target number of event in IA under H0
    i0=0 ##number of simulations achieve target number of events in both IA and FA under H0
    e0=rep(0,nreps) ##indicator of whether this simulation achieve the target number of events in both IA and FA under H0
    for(i in 1:nreps){
      
      a0<-pred_all_H0$event_pred$newEvents
      a1<-a0[a0$draw==i,]

      grph1<-1*(a1$treatment==2)
      event_num=sum(a1$event)
      
      if(target_IA_d<=event_num){
        i0_IA=i0_IA+1
      }
      if(target_IA_d<=event_num&&target_d<=event_num){
        i0=i0+1
        e0[i]=1
      }
      

      
     if(e0[i]==1){
       IA_time_cut<-sort(a1$totalTime[a1$event==1])[target_IA_d]
       FA_time_cut<-sort(a1$totalTime[a1$event==1])[target_d]
       data_IA<-a1[a1$arrivalTime<=IA_time_cut,]
       data_FA<-a1[a1$arrivalTime<=FA_time_cut,]
       IA_censor<-1*(data_IA$event&data_IA$totalTime<=IA_time_cut)
       FA_censor<-1*(data_FA$event&data_FA$totalTime<=FA_time_cut)
       IA_surv_time<-pmin(data_IA$totalTime,IA_time_cut)-data_IA$arrivalTime
       FA_surv_time<-pmin(data_FA$totalTime,FA_time_cut)-data_FA$arrivalTime
       grph1f<-grph1[a1$arrivalTime<=FA_time_cut]
       grph1i<-grph1[a1$arrivalTime<=IA_time_cut]
       
       
       
       
       ###PA OS log rank test and COX model under H0 ####
       tteout0<-tte(FA_surv_time,FA_censor, grph1f,"all",test=test)
       os_pwh0[i]<-tteout0$p
       os_pFHh0[i]<-tteout0$pFH
       
       ### Interim OS log rank test and COX model under H0####
       tteouti0<-tte(IA_surv_time,IA_censor, grph1i,"all",test=test)
       os_pwh0i[i]<-tteouti0$p
       os_ithr0[i]<-tteouti0$hr
       os_itpFH_h0[i]<-tteouti0$pFH
       
       ### section for alpha calulation under H0 ###
       
       stop_IA_ind_h0=0
       if(test_IA=='Efficacy'||test_IA=='Efficacy and Futility'){
         if (os_pwh0i[i]<ialpha){
           p1_h0<-p1_h0+1  # counting how many superiority trials under Ha at interim
           stop_IA_ind_h0=1
         }
       }
       if(test_IA=='Futility'||test_IA=='Efficacy and Futility'){
         if(os_ithr0[i]>Futility_boundary){
           p1f_h0<-p1f_h0+1 # counting how many Futility trials under Ha at interim
           stop_IA_ind_h0=1
         }
       }
       if(test_IA=='Efficacy and Futility'){
         if(os_ithr0[i]>Futility_boundary && os_pwh0i[i]<ialpha){
           cross_IA_h0=1
         }
       }
       
       
       if (stop_IA_ind_h0==0 & os_pwh0[i]<falpha ){
         p2_h0<-p2_h0+1  # counting how many superiority trials under Ha at PA
       }
       
       if (!is.na(os_itpFH_h0[i])) {
         iterFH_h0<-iterFH_h0+1
         stop_IA_ind_FH_h0=0
         if(test_IA=='Efficacy'||test_IA=='Efficacy and Futility'){
           if (os_itpFH_h0[i]<ialpha){
             p1_FH_h0<-p1_FH_h0+1  # counting how many superiority trials under Ha at interim
             stop_IA_ind_FH_h0=1
           }
         }
         if(test_IA=='Futility'||test_IA=='Efficacy and Futility'){
           if(os_ithr0[i]>Futility_boundary){
             p1f_FH_h0<-p1f_FH_h0+1 # counting how many Futility trials under Ha at interim
             stop_IA_ind_FH_h0=1
           }
         }
         if(test_IA=='Efficacy and Futility'){
           if(os_ithr0[i]>Futility_boundary && os_itpFH_h0[i]<ialpha){
             cross_IA_FH_h0=1
           }
         }
         
         
         if (stop_IA_ind_FH_h0==0 & os_pFHh0[i]<falpha ){
           p2_FH_h0<-p2_FH_h0+1  # counting how many superiority trials under Ha at PA
         }
       }
       
     }
      
    }
    totala<-p1_h0+p2_h0   ## total alpha
    totala_FH<-p1_FH_h0+p2_FH_h0   ## total alpha under FH
    ### ouput organizing... ###

    ## primary OS power, alpha, events, subjects ##

    
      
      
    textHA=paste('The testing results under HA are calculated based on',i1,'simulations that achieve target number of events in both interim analysis(IA) and primary analysis(PA).')
    textH0=paste('The testing results under H0 are calculated based on',i0,'simulations that achieve target number of events in both interim analysis(IA) and primary analysis(PA).')
    plower=(1-pilevel)/2
    pupper=1-plower

   
    iteration1<-i1
    iteration0<-i0
    simu_summary<-matrix(c(nreps,i1_IA,nreps-i1_IA,
                           i1_IA,i1,i1_IA-i1,
                           nreps,i0_IA,nreps-i0_IA,
                           i0_IA,i0,i0_IA-i0
                           ),byrow=TRUE,nrow=4, dimnames=list(c("Interim analysis under HA",
                                                                "Primary analysis under HA",
                                                                "Interim analysis under H0",
                                                                "Primary analysis under H0"),
                                                              c("Number of Simulations", "Achieve target events", "Not achieve target events")))
    
    if(i0==0||i1==0){
      message('No simulations achieved target number of events in both interim analysis(IA) and primary analysis(PA) under H0 or HA.')
      return(list(iteration0=i0,iteration1=i1,simu_summary=simu_summary,power=NULL, OC_interim=NULL,samplesize=NULL, 
                  hzratio=NULL, hzrc=NULL,
                  hzratio2=NULL, median=NULL, osrate=NULL, duration=NULL, duration1=NULL,
                  pred_all_HA=pred_all_HA,pred_all_H0=pred_all_H0,target_d=target_d,textHA=textHA,textH0=textH0))
    }
    
    if(i0!=0&i1!=0){
    
    
    power104<-matrix(c(p1/i1,p2/i1, totalpw/i1,
                       pFH1/iterFH,pFH2/iterFH,totalpwFH/iterFH,
                       p1_h0/i0, p2_h0/i0, totala/i0,
                       p1_FH_h0/iterFH_h0, p2_FH_h0/iterFH_h0, totala_FH/iterFH_h0),
                     byrow=T,nrow=4, dimnames=list(c("Power from log-rank test",
                                                     "Power from Fleming-Harrington test with (0,1)",
                                                     "Rejection rate from log-rank test under H0",
                                                     "Rejection rate from Fleming-Harrington test with (0,1) under H0"),
                                                   c("IA", "PA", "Total")))
    
    OC_interim<-matrix(c(p1/i1,p1f/i1, cross_IA,
                         p1_h0/i0, p1f_h0/i0, cross_IA_h0,
                         pFH1/iterFH,p1f_FH/iterFH,cross_IA_FH,
                         p1_FH_h0/iterFH_h0,p1f_FH_h0/iterFH_h0,cross_IA_FH_h0
    ),
    byrow=T,nrow=4, dimnames=list(c("Log-rank test under HA",
                                    "Log-rank test under H0",
                                    'Fleming-Harrington test with (0,1) under HA',
                                    'Fleming-Harrington test with (0,1) under H0'),
                                  c("Stopping for Efficacy","Stopping for Futility",
                                    'Warning for crossover between efficacy boundary and futility boundary')))

    
    samplesize<-matrix(c(mean(nA[e1==1]), mean(nC[e1==1]), mean(nA[e1==1]+nC[e1==1]),
                         mean(os_evta[e1==1]), mean(os_evtc[e1==1]), mean(os_evta[e1==1]+os_evtc[e1==1]) ,
                         mean(os_itevta[e1==1]), mean(os_itevtc[e1==1]), mean(os_itevta[e1==1]+os_itevtc[e1==1]),
                         mean(b4_eventsa[e1==1]), mean(b4_eventsc[e1==1]), mean(b4_eventsa[e1==1]+b4_eventsc[e1==1]),
                         mean(b4_eventsai[e1==1]), mean(b4_eventsci[e1==1]), mean(b4_eventsai[e1==1]+b4_eventsci[e1==1])

    ), byrow=T, nrow=5,
    ncol=3, dimnames=list(c("Randomized Subjects", "Average events in PA", "Average events in IA",
                            paste("Average events before", round(lag0/30.4375,digits=2),'months in PA'),
                            paste("Average events before", round(lag0/30.4375,digits=2),'months in IA')),c(treatment_label[2], treatment_label[1], "Total")))


    
    hzratio<-matrix(c(mean(os_hr[e1==1]), mean(os_ithr[e1==1]) ),
                    nrow=1,dimnames=list("Average hazard ratios",c("PA","IA")))
    
    hzrc<-matrix(c(hrle8/i1,hrgt8le9/i1,hrgt9/i1),
                 nrow=1,dimnames=list("Summary of hazard ratios",c("HR<=0.8","0.8<HR<=0.9","HR>0.9")))

    
    hzratio2<-matrix(c(mean(b4_os_hr[e1==1]), mean(b4_os_hrlow[e1==1]), mean(b4_os_hrup[e1==1]),
                       mean(pst_os_hr[e1==1]), mean(pst_os_hrlow[e1==1]), mean(pst_os_hrup[e1==1])),
                     nrow=2,byrow=T,dimnames=list(c(paste("Average hazard ratios before", round(lag0/30.4375,digits=2), 'months'),
                                                    paste("Average hazard ratios after", round(lag0/30.4375,digits=2),'months')), c(
                                                      "mean", paste("Lower",paste0(2.5,"%")),
                                                      paste("Upper",paste0(97.5,"%"))
                                                    )))

 
    median<-matrix(c(mean(stats::na.omit(os_itmediana[e1==1])), mean(stats::na.omit(os_itmedianc[e1==1])),
                     mean(stats::na.omit(os_mediana[e1==1])), mean(stats::na.omit(os_medianc[e1==1])))/30.4375, nrow=2, ncol=2, byrow=T,
                   dimnames=list(c("Average of median survial time (months) in IA", "Average of median survival time (months) in PA"),c(treatment_label[2], treatment_label[1])))

   
    duration<-matrix(c(mean(accrualtime[e1==1]),mean(interim_timing[e1==1]), mean(os_duration[e1==1]))/30.4375,
                     nrow=1,dimnames=list("Average study durations (months)",c("Accrual ", "IA", "PA")))
    
    nrepindx=1:nreps
    atemp<-pred_all_HA$event_pred$newEvents[pred_all_HA$event_pred$newEvents$draw%in%nrepindx[e1==1],]
    q = 1 - c(0.5, plower, pupper)
    interim_pred_day = rep(NA, length(q))
    final_pred_day = rep(NA, length(q))
    d0=0
    sdf <- function(t, target_d, d0, newEvents) {
      sumdata <- newEvents %>%
        dplyr::group_by(.data$draw) %>%
        dplyr::summarize(n = sum(.data$totalTime <= t & .data$event == 1) + d0)
      mean(sumdata$n < target_d)
    }
    t0=1
    tmax = max(atemp$totalTime[atemp$event==1])


    if (sdf(tmax, target_IA_d, d0, atemp) == 0) {
      newIA <- atemp %>%
        dplyr::group_by(.data$draw) %>%
        dplyr::filter(.data$event == 1) %>%
        dplyr::arrange(.data$draw, .data$totalTime) %>%
        dplyr::filter(dplyr::row_number() == target_IA_d - d0)
      interim_pred_day <- ceiling(stats::quantile(newIA$totalTime, c(0.5, plower, pupper)))


    } else {

      for (j in 1:length(q)) {
        # check if the quantile can be estimated from observed data
        if (sdf(tmax, target_IA_d, d0, atemp) <= q[j]) {
          interim_pred_day[j] = stats::uniroot(function(x)
            sdf(x, target_IA_d, d0, atemp) - q[j],
            c(t0, tmax), tol = 1)$root
          interim_pred_day[j] = ceiling(interim_pred_day[j])
        }

      }

    }

    if (sdf(tmax, target_d, d0, atemp) == 0) {

      newFA <- atemp %>%
        dplyr::group_by(.data$draw) %>%
        dplyr::filter(.data$event == 1) %>%
        dplyr::arrange(.data$draw, .data$totalTime) %>%
        dplyr::filter(dplyr::row_number() == target_d - d0)
      final_pred_day <- ceiling(stats::quantile(newFA$totalTime, c(0.5, plower, pupper)))
    } else {

      for (j in 1:length(q)) {

        if (sdf(tmax, target_d, d0, atemp) <= q[j]) {
          final_pred_day[j] = stats::uniroot(function(x)
            sdf(x, target_d, d0, atemp) - q[j],
            c(t0, tmax), tol = 1)$root
          final_pred_day[j] = ceiling(final_pred_day[j])
        }
      }

    }




 
    duration1<-matrix(round(c(mean(interim_timing[e1==1]), interim_pred_day,
                        mean(os_duration[e1==1]),  final_pred_day)/30.4375,digits=2),
                      nrow=2, ncol=4,byrow=T, dimnames=list(c("Average IA timing (months)", "Average PA timing (months)"),
                                                            c("Mean","Median" ,paste("Lower",paste0(plower*100,"%")),
                                                              paste("Upper",paste0(pupper*100,"%"))
                                                            )))

    
    osrate<-matrix(c(mean(os12_a[e1==1]),mean(os12_c[e1==1]),mean(os24_a[e1==1]),mean(os24_c[e1==1]),mean(os48_a[e1==1]),mean(os48_c[e1==1])),
                   nrow=3, ncol=2, byrow=T,dimnames=list(c("1-year survival rate", "2-year survival rate","4-year survival rate"),
                                                         c(treatment_label[2], treatment_label[1])))
 
    

    return(list(iteration0=i0,iteration1=i1,simu_summary=simu_summary,power=power104, OC_interim=OC_interim, samplesize=samplesize,  hzratio=hzratio, hzrc=hzrc,
                hzratio2=hzratio2, median=median, osrate=osrate, duration=duration, duration1=duration1,
                pred_all_HA=pred_all_HA,pred_all_H0=pred_all_H0,target_d=target_d,textHA=textHA,textH0=textH0))
    }

  }

  if(!IA_included){

   
    ###elements to save simulation results
    iterFH<-0
    os_pw<-rep(0,nreps)
    os_pFH<-rep(0,nreps)
    p1<-p2<-pFH1<-pFH2<-0
    p1_h0<-p2_h0<-0
    hrle8<-hrgt8le9<-hrgt9<-0
    nA<-nC<-os_evta<-os_evtc<-rep(0,nreps)
    os_hr<-os_mediana<-os_medianc<-rep(0,nreps)

    b4_os_hr<-pst_os_hr<-b4_os_hrup<-b4_os_hrlow<-pst_os_hrup<-pst_os_hrlow<-rep(0,nreps)
    b4_eventsai<-b4_eventsci<-rep(0,nreps)

    os_pwh0<-os_pwh0i<-rep(0,nreps)

    b4_eventsa<-b4_eventsc<-rep(0,nreps)
    b4lag_eventsa<-b4lag_eventsc<-rep(0,nreps)

    os_itmediana<-os_itmedianc<-rep(0,nreps)

    os_duration<-interim_timing<-os_itpw<-os_itpFH<-os_ithr<-os_itevta<-os_itevtc<-rep(0,nreps)

    maxHR<-maxHR1<-maxHR2<-rep(0,nreps)

    pst_osi<-pst_osic<-b4_oscensori<-rep(0,target_n)
    pst_oscensori<-rep(0,target_n)

    os12_a<-os12_c<-os24_a<-os24_c<-os48_a<-os48_c<-rep(0,nreps)


    accrualtime=rep(NA,nreps)
    
    i1=0 ##number of simulations achieve target number of events in both IA and FA under HA
    e1=rep(0,nreps) ##indicator of whether this simulation achieve the target number of events in both IA and FA under HA


    for(i in 1:nreps){

      a0<-pred_all_HA$event_pred$newEvents
      a1<-a0[a0$draw==i,]
      accrualtime[i]=max(a1$arrivalTime)
      grph1<-1*(a1$treatment==2)
      event_num=sum(a1$event)
      if(target_d<=event_num){
        i1=i1+1
        e1[i]=1

      }
      if(e1[i]==1){
        FA_time_cut<-sort(a1$totalTime[a1$event==1])[target_d]
        
        data_FA<-a1[a1$arrivalTime<=FA_time_cut,]
        
        FA_censor<-1*(data_FA$event&data_FA$totalTime<=FA_time_cut)
        
        FA_surv_time<-pmin(data_FA$totalTime,FA_time_cut)-data_FA$arrivalTime
        grph1f<-grph1[a1$arrivalTime<=FA_time_cut]
        
        
        out<-b4pst(os=FA_surv_time,osc=FA_censor,lag=lag)
        b4os<-out$b4os
        b4osc<-out$b4osc
        pstos<-out$pstos
        pstosc<-out$pstosc
        
        
        
        
        ### calculate HR before and after lag to verify assumptions ###
        ### including HR and its 95% confidence interval                         ###
        
        b4out<-tte(b4os, b4osc, grph1f,"hr",test=test)
        b4_os_hr[i]<-b4out$hr
        b4_os_hrlow[i]<-b4out$hrlow
        b4_os_hrup[i]<-b4out$hrup
        
        pstout<-tte(pstos, pstosc, grph1f,"hr",test=test)
        pst_os_hr[i]<-pstout$hr
        pst_os_hrlow[i]<-pstout$hrlow
        pst_os_hrup[i]<-pstout$hrup
        
        
        
        
        b4_eventsa[i]<-sum(b4osc[grph1f==1])
        b4_eventsc[i]<-sum(b4osc[grph1f==0])
        
        
        
        
        ###PA OS log rank test and COX model under Ha ####
        
        tteout<-tte(FA_surv_time, FA_censor, grph1f,"all",test=test)
        os_pw[i]<-tteout$p
        os_mediana[i]<-tteout$med_a
        os_medianc[i]<-tteout$med_c
        os_evta[i]<-tteout$evt_a
        os_evtc[i]<-tteout$evt_c
        nA[i]<-tteout$n_a
        nC[i]<-tteout$n_c
        os_hr[i]<-tteout$hr
        
        os12_a[i]<-tteout$os12rate_a
        os12_c[i]<-tteout$os12rate_c
        os24_a[i]<-tteout$os24rate_a
        os24_c[i]<-tteout$os24rate_c
        os48_a[i]<-tteout$os48rate_a
        os48_c[i]<-tteout$os48rate_c
        
        os_pFH[i]<-tteout$pFH
        
        
        
        
        
        if ( os_pw[i]<falpha ){
          
          p2<-p2+1  # counting how many superiority trials under Ha at PA
          maxHR2[i]<-os_hr[i]
        }
        
        
        
        
        if (os_pFH[i]<falpha ){
          
          pFH2<-pFH2+1  # counting how many superiority trials under Ha at PA
        }
        
        
        ### count number of HRs in ranges of HR<=0.8, 0.8<HR<=0.9, HR>0.9 for hrle8 hrgt8le9 hrgt9
        
        if(os_hr[i]<=0.8){
          hrle8<-hrle8+1}  # counting how many HRs <= 0.8 at PA
        
        if(os_hr[i]>0.8 & os_hr[i]<=0.9){
          hrgt8le9<-hrgt8le9+1}  # counting how many HRs >0.8&<= 0.9 at PA
        
        if(os_hr[i]>0.9){
          hrgt9<-hrgt9+1}  # counting how many HRs > 0.9 at PA
        
        
        
        
        ## need to figure out the study duration  ##
        os_duration[i]<- FA_time_cut
        

      }

      


    }
    totalpw<-p2        ## total power
    totalpwFH<-pFH2  ## total power for FH Test  


    ###under H0####
    i0=0 ##number of simulations achieve target number of events in both IA and FA under H0
    e0=rep(0,nreps) ##indicator of whether this simulation achieve the target number of events in both IA and FA under H0
    for(i in 1:nreps){
      a0<-pred_all_H0$event_pred$newEvents
      a1<-a0[a0$draw==i,]

      grph1<-1*(a1$treatment==2)
      event_num=sum(a1$event)
      if(target_d<=event_num){
        i0=i0+1
        e0[i]=1
      }

      if(e0[i]==1){
        FA_time_cut<-sort(a1$totalTime[a1$event==1])[target_d]
        
        data_FA<-a1[a1$arrivalTime<=FA_time_cut,]
        
        FA_censor<-1*(data_FA$event&data_FA$totalTime<=FA_time_cut)
        
        FA_surv_time<-pmin(data_FA$totalTime,FA_time_cut)-data_FA$arrivalTime
        grph1f<-grph1[a1$arrivalTime<=FA_time_cut]
        
        
        
        
        ###PA OS log rank test and COX model under H0 ####
        tteout0<-tte(FA_surv_time,FA_censor, grph1f,"logrank",test=test)
        os_pwh0[i]<-tteout0$p
        
        
        
        ### section for alpha calulation under H0 ###
        
        if(os_pwh0[i]<falpha){
          p2_h0<-p2_h0+1}  # counting how many superority trials under H0 at PA
      }
      
    }


    totala<-p2_h0   ## total alpha


    ### ouput organizing... ###

    ## primary OS power, alpha, events, subjects ##
    plower=(1-pilevel)/2
    pupper=1-plower
    iteration1<-i1
    iteration0<-i0
    simu_summary<-matrix(c(nreps,i1,nreps-i1,
                           nreps,i0,nreps-i0
    ),byrow=TRUE,nrow=2, dimnames=list(c("Primary analysis under HA",
                                         "Primary analysis under H0"),
                                       c("Number of Simulations", "Achieve target events", "Not achieve target events")))
    textHA=paste('The testing results under HA are calculated based on',i1,'simulations that achieve target number of events in primary analysis(PA).')
    textH0=paste('The testing results under H0 are calculated based on',i0,'simulations that achieve target number of events in primary analysis(PA).')
    if(i0==0||i1==0){
      message('No simulations achieved target number of events in primary analysis (PA) under H0 or HA.')
      return(list(iteration0=i0,iteration1=i1,simu_summary=simu_summary,power=NULL, OC_interim=NULL,samplesize=NULL, 
                  hzratio=NULL, hzrc=NULL,
                  hzratio2=NULL, median=NULL, osrate=NULL, duration=NULL, duration1=NULL,
                  pred_all_HA=pred_all_HA,pred_all_H0=pred_all_H0,target_d=target_d,textHA=textHA,textH0=textH0))
    }

    if(i0!=0&i1!=0){
      
     
      power104<-matrix(c(p2/i1,pFH2/i1,p2_h0/i0),
                       byrow=T,nrow=3, dimnames=list(c("Power from log-rank test",
                                                       "Power from Fleming-Harrington test with (0,1)",
                                                       "Rejection rate under H0"), c("PA")))
      
      
      samplesize<-matrix(c(mean(nA[e1==1]), mean(nC[e1==1]), mean(nA[e1==1]+nC[e1==1]),
                           mean(os_evta[e1==1]), mean(os_evtc[e1==1]), mean(os_evta[e1==1]+os_evtc[e1==1]) ,
                           mean(b4_eventsa[e1==1]), mean(b4_eventsc[e1==1]), mean(b4_eventsa[e1==1]+b4_eventsc[e1==1])
                           
      ), byrow=T, nrow=3,
      ncol=3, dimnames=list(c("Randomized Subjects", "Average events in PA",
                              paste("Average events before", round(lag0/30.4375,digits=2),' months in PA')),
                            c(treatment_label[2], treatment_label[1], "Total")))
      
      
      
      hzratio<-matrix(c(mean(os_hr[e1==1]) ),
                      nrow=1,dimnames=list("Average of hazard ratios",c("PA")))
      
      hzrc<-matrix(c(hrle8/i1,hrgt8le9/i1,hrgt9/i1),
                   nrow=1,dimnames=list("Summary of hazard ratios",c("HR<=0.8","0.8<HR<=0.9","HR>0.9")))
      
      
      hzratio2<-matrix(c(mean(b4_os_hr[e1==1]), mean(b4_os_hrlow[e1==1]), mean(b4_os_hrup[e1==1]),
                         mean(pst_os_hr[e1==1]), mean(pst_os_hrlow[e1==1]), mean(pst_os_hrup[e1==1])),
                       nrow=2,byrow=T,dimnames=list(c(paste("Average hazard ratios before", round(lag0/30.4375,digits=2), "months"),
                                                      paste("Average hazard ratios after", round(lag0/30.4375,digits=2), "months")), c(
                                                        "mean", paste("Lower",paste0(2.5,"%")),
                                                        paste("Upper",paste0(97.5,"%"))
                                                      )))
      
      median<-matrix(round(c(mean(stats::na.omit(os_mediana[e1==1])), mean(stats::na.omit(os_medianc[e1==1])))/30.4375,digits=2), nrow=1, ncol=2, byrow=T,
                     dimnames=list(c("Average of median survival times (months) in PA"),c(treatment_label[2], treatment_label[1])))
      
      
      duration<-matrix(round(c(mean(accrualtime[e1==1]),mean(os_duration[e1==1]))/30.4375,digits=2),
                       nrow=1,dimnames=list("Average study duration (months)",c("Accrual ", "PA")))
      
      nrepindx=1:nreps
      atemp<-pred_all_HA$event_pred$newEvents[pred_all_HA$event_pred$newEvents$draw%in%nrepindx[e1==1],]
      q = 1 - c(0.5, plower, pupper)
      
      interim_pred_day = rep(NA, length(q))
      final_pred_day = rep(NA, length(q))
      d0=0
      sdf <- function(t, target_d, d0, newEvents) {
        sumdata <- newEvents %>%
          dplyr::group_by(.data$draw) %>%
          dplyr::summarize(n = sum(.data$totalTime <= t & .data$event == 1) + d0)
        mean(sumdata$n < target_d)
      }
      
      tmax = max(atemp$totalTime[atemp$event==1])
      
      
      if (sdf(tmax, target_IA_d, d0, atemp) == 0) {
        newIA <- atemp %>%
          dplyr::group_by(.data$draw) %>%
          dplyr::filter(.data$event == 1) %>%
          dplyr::arrange(.data$draw, .data$totalTime) %>%
          dplyr::filter(dplyr::row_number() == target_IA_d - d0)
        interim_pred_day <- ceiling(stats::quantile(newIA$totalTime, c(0.5, plower, pupper)))
        
        
      } else {
        
        for (j in 1:length(q)) {
          # check if the quantile can be estimated from observed data
          if (sdf(tmax, target_IA_d, d0, atemp) <= q[j]) {
            interim_pred_day[j] = stats::uniroot(function(x)
              sdf(x, target_IA_d, d0, atemp) - q[j],
              c(t0, tmax), tol = 1)$root
            interim_pred_day[j] = ceiling(interim_pred_day[j])
          }
          
        }
        
      }
      
      if (sdf(tmax, target_d, d0, atemp) == 0) {
        
        newFA <- atemp %>%
          dplyr::group_by(.data$draw) %>%
          dplyr::filter(.data$event == 1) %>%
          dplyr::arrange(.data$draw, .data$totalTime) %>%
          dplyr::filter(dplyr::row_number() == target_d - d0)
        final_pred_day <- ceiling(stats::quantile(newFA$totalTime, c(0.5, plower, pupper)))
      } else {
        
        for (j in 1:length(q)) {
          
          if (sdf(tmax, target_d, d0, atemp) <= q[j]) {
            final_pred_day[j] = stats::uniroot(function(x)
              sdf(x, target_d, d0, atemp) - q[j],
              c(t0, tmax), tol = 1)$root
            final_pred_day[j] = ceiling(final_pred_day[j])
          }
        }
        
      }
      
      
      
      
      
      duration1<-matrix(round(c(mean(os_duration[e1==1]),  final_pred_day)/30.4375,digits=2),
                        nrow=1, ncol=4,byrow=T, dimnames=list(c("PA Timing (months)"),
                                                              c("Mean","Median" ,paste("Lower",paste0(plower*100,"%")),
                                                                paste("Upper",paste0(pupper*100,"%"))
                                                              )))
      
      
      osrate<-matrix(c(mean(os12_a[e1==1]),mean(os12_c[e1==1]),mean(os24_a[e1==1]),mean(os24_c[e1==1]),mean(os48_a[e1==1]),mean(os48_c[e1==1])),
                     nrow=3, ncol=2, byrow=T,dimnames=list(c("1-year survival rate", "2-year survival rate","4-year survival rate"),
                                                           c(treatment_label[2], treatment_label[1])))
      
      
      return(list(iteration0=i0,iteration1=i1,simu_summary=simu_summary,power=power104, OC_interim=NA,samplesize=samplesize,  hzratio=hzratio, hzrc=hzrc,
                  hzratio2=hzratio2, median=median, osrate=osrate, duration=duration, duration1=duration1,
                  pred_all_HA=pred_all_HA,pred_all_H0=pred_all_H0,target_d=target_d,textHA=textHA,textH0=textH0))
      
      
    }
    }


}



