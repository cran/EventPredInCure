#' @title Fit time-to-event model
#' @description Fits a specified time-to-event model to the event data.
#'
#' @param df The subject-level event data, including \code{time}
#'   and \code{event}. The data should also include \code{treatment}
#'   coded as 1, 2, and so on, and \code{treatment_description}
#'   for fitting the event model by treatment.
#' @param event_model The event model used to analyze the event data
#'   which can be set to one of the following options:
#'   "exponential", "Weibull", "log-logistic", "log-normal",
#'   "piecewise exponential", "model averaging", "spline","exponential with cured population","weibull with cured population",
#'   "log-normal with cured population","log-logistic with cured population" or "piecewise exponential with cured population".
#'   The model averaging uses the \code{exp(-bic/2)} weighting and
#'   combines Weibull and log-normal models. The spline model of
#'   Royston and Parmar (2002) assumes that a transformation of
#'   the survival function is modeled as a natural cubic spline
#'   function of log time. By default, it is set to "model averaging".
#' @param piecewiseSurvivalTime A vector that specifies the time
#'   intervals for the piecewise exponential survival distribution or piecewise exponential with cured population.
#'   Must start with 0, e.g., c(0, 60) breaks the time axis into 2
#'   event intervals: [0, 60) and [60, Inf). By default, it is set to 0.
#' @param k The number of inner knots of the spline. The default
#'   \code{k=0} gives a Weibull, log-logistic or log-normal model,
#'   if \code{scale} is "hazard", "odds", or "normal", respectively.
#'   The knots are chosen as equally-spaced quantiles of the log
#'   uncensored survival times. The boundary knots are chosen as the
#'   minimum and maximum log uncensored survival times.
#' @param scale If "hazard", the log cumulative hazard is modeled
#'   as a spline function. If "odds", the log cumulative odds is
#'   modeled as a spline function. If "normal", -qnorm(S(t)) is
#'   modeled as a spline function.
#' @param by_treatment A Boolean variable to control whether or not to
#'   fit the time-to-event data by treatment group. By default,
#'   it is set to \code{FALSE}.
#
#' @param criterion A character variable to denote the criterion in model
#' selection to shown in the figure, which can be set to one of the following
#' options: "aic","bic" or "both". By default,it is set to \code{both}.
#'
#' @return
#' A list of results from the model fit including key information
#' such as the event model, \code{model}, the estimated model parameters,
#' \code{theta}, the covariance matrix, \code{vtheta}, as well as the
#' Bayesian Information Criterion, \code{bic}, and Akaike Information Criterion, \code{aic}.
#'
#' If the piecewise exponential model is used, the location
#' of knots used in the model, \code{piecewiseSurvivalTime}, will
#' be included in the list of results.
#'
#' If the model averaging option is chosen, the weight assigned
#' to the Weibull component is indicated by the \code{w1} variable.
#'
#' If the spline option is chosen, the \code{knots} and \code{scale}
#' will be included in the list of results.
#'
#' When fitting the event model by treatment, the outcome is presented
#' as a list of lists, where each list element corresponds to a
#' specific treatment group.
#'
#' The fitted time-to-event survival curve is also returned.
#'
#'
#' @references \itemize{
#' \item Royston, Patrick, and Mahesh KB Parmar. 
#' "Flexible parametric proportional‐hazards and proportional‐odds models for censored survival data, 
#' with application to prognostic modelling and estimation of treatment effects." 
#' Statistics in medicine 21.15 (2002): 2175-2197.
#' 
#' \item Chen, Tai-Tsang. "Predicting analysis times in randomized clinical trials with cancer immunotherapy." 
#' BMC medical research methodology 16.1 (2016): 1-10.
#' }
#' 
#'
#' @examples
#'
#' event_fit <- fitEvent(df = interimData2,
#'                       event_model = "piecewise exponential",
#'                       piecewiseSurvivalTime = c(0, 180))
#'
#' @export
#'
fitEvent <- function(df, event_model = "model averaging",
                     piecewiseSurvivalTime = 0,
                     k = 0, scale = "hazard",
                     by_treatment = FALSE,criterion="both") {
  erify::check_class(df, "data.frame")

  erify::check_content(tolower(event_model),
                       c("exponential", "weibull", "log-logistic",
                         "log-normal", "piecewise exponential",
                         "model averaging", "spline","exponential with cured population","weibull with cured population",
                         "log-normal with cured population","log-logistic with cured population",
                         "piecewise exponential with cured population"))

  erify::check_content(tolower(criterion),
                       c("aic", "bic",
                         "both"))

  if (piecewiseSurvivalTime[1] != 0) {
    stop("piecewiseSurvivalTime must start with 0");
  }
  if (length(piecewiseSurvivalTime) > 1 &
      any(diff(piecewiseSurvivalTime) <= 0)) {
    stop("piecewiseSurvivalTime should be increasing")
  }

  erify::check_n(k, zero = TRUE)
  erify::check_content(tolower(scale), c("hazard", "odds", "normal"))


  erify::check_bool(by_treatment)

  df <- dplyr::as_tibble(df)
  names(df) <- tolower(names(df))

  if (by_treatment) {
    ngroups = length(table(df$treatment))

    if (!("treatment_description" %in% names(df))) {
      df <- df %>% dplyr::mutate(
        treatment_description = paste0("Treatment ", .data$treatment))
    }
  } else {
    ngroups = 1
    df <- df %>% dplyr::mutate(treatment = 1)
  }

  if (ngroups == 1) {
    by_treatment = FALSE
  }


  # fit by treatment group
  event_fit <- list()
  g1 <- list()
  pcng <-rep(0, ngroups)
  for (i in 1:ngroups) {
    df1 <- df %>% dplyr::filter(.data$treatment == i)

    n0 = nrow(df1)
    d0 = sum(df1$event)
    ex0 = sum(df1$time)

    erify::check_positive(d0, supplement = paste(
      "The number of events must be positive to fit an event model."))

    kmfit <- survival::survfit(survival::Surv(time, event) ~ 1, data = df1)
    kmdf <- dplyr::tibble(time = kmfit$time, surv = kmfit$surv,n.censor=kmfit$n.censor)
    kmdf <- dplyr::tibble(time = 0, surv = 1,n.censor=0) %>%
      dplyr::bind_rows(kmdf)

    if (tolower(event_model) == "exponential") {
      # lambda(t) = lambda
      # S(t) = exp(-lambda*t)

      fit2 <- list(model = 'Exponential',
                   theta = log(d0/ex0),
                   vtheta = 1/d0,
                   bic = -2*(-d0 + d0*log(d0/ex0)) + log(n0),
                   aic = -2*(-d0 + d0*log(d0/ex0)) + 2)

      # fitted survival curve
      dffit2 <- dplyr::tibble(
        time = seq(0, max(df1$time)),
        surv = stats::pexp(.data$time, rate = exp(fit2$theta), lower.tail = FALSE))
      
    } else if (tolower(event_model) == "weibull") {
      # lambda(t) = kappa/lambda*(t/lambda)^(kappa-1)
      # S(t) = exp(-(t/lambda)^kappa)

      reg <- survival::survreg(survival::Surv(time, event) ~ 1,
                               data = df1, dist = "weibull")

      # weibull$shape = 1/reg$scale, weibull$scale = exp(reg$coefficients)
      # we define theta = (log(weibull$scale), -log(weibull$shape))
      # reg$var is for theta = c(reg$coefficients, log(reg$scale))
      fit2 <- list(model = "Weibull",
                   theta = c(as.numeric(reg$coefficients), log(reg$scale)),
                   vtheta = reg$var,
                   bic = -2*reg$loglik[1] + 2*log(n0),
                   aic = -2*reg$loglik[1] + 2*2)

      # fitted survival curve
      dffit2 <- dplyr::tibble(
        time = seq(0, max(df1$time)),
        surv = stats::pweibull(.data$time, shape = exp(-fit2$theta[2]),
                        scale = exp(fit2$theta[1]), lower.tail = FALSE))
    } else if (tolower(event_model) == "log-logistic") {
      # S(t) = 1/(1 + (t/lambda)^kappa)
      reg <- survival::survreg(survival::Surv(time, event) ~ 1,
                               data = df1, dist = "loglogistic")

      # llogis$shape = 1/reg$scale, llogis$scale = exp(reg$coefficients)
      # we define theta = (log(llogis$scale), -log(llogis$shape))
      # reg$var is for theta = c(reg$coefficients, log(reg$scale))
      fit2 <- list(model = "Log-logistic",
                   theta = c(as.numeric(reg$coefficients), log(reg$scale)),
                   vtheta = reg$var,
                   bic = -2*reg$loglik[1] + 2*log(n0),
                   aic = -2*reg$loglik[1] + 2*2)

      # fitted survival curve
      dffit2 <- dplyr::tibble(
        time = seq(0, max(df1$time)),
        surv = stats::plogis(log(.data$time), location = fit2$theta[1],
                      scale = exp(fit2$theta[2]), lower.tail = FALSE))
    } else if (tolower(event_model) == "log-normal") {
      # S(t) = 1 - Phi((log(t) - meanlog)/sdlog)
      reg <- survival::survreg(survival::Surv(time, event) ~ 1,
                               data = df1, dist = "lognormal")

      # we use parameterization theta = (meanlog, log(sdlog))
      # reg$var is for theta = c(reg$coefficients, log(reg$scale))
      fit2 <- list(model = "Log-normal",
                   theta = c(as.numeric(reg$coefficients), log(reg$scale)),
                   vtheta = reg$var,
                   bic = -2*reg$loglik[1] + 2*log(n0),
                   aic=-2*reg$loglik[1] + 2*2)

      # fitted survival curve
      dffit2 <- dplyr::tibble(
        time = seq(0, max(df1$time)),
        surv = stats::plnorm(.data$time, meanlog = fit2$theta[1],
                      sdlog = exp(fit2$theta[2]), lower.tail = FALSE))
    } else if (tolower(event_model) == "piecewise exponential") {
      # lambda(t) = lambda[j] for ucut[j] < t <= ucut[j+1], j = 1,...,J
      # where ucut[1]=0< ucut[2]< ...< ucut[J]< ucut[J+1]=Inf are the knots
      u = piecewiseSurvivalTime[piecewiseSurvivalTime < max(df1$time)]
      ucut = c(u, max(df1$time))
      J = length(u)

      d = rep(NA, J)  # number of events in each interval
      ex = rep(NA, J) # total exposure in each interval
      for (j in 1:J) {
        d[j] = sum((df1$time > ucut[j]) * (df1$time <= ucut[j+1]) *
                     (df1$event == 1))
        ex[j] = sum(pmax(0, pmin(df1$time, ucut[j+1]) - ucut[j]))
      }

      # maximum likelihood estimates and covariance matrix
      if (J > 1) {
        vtheta = diag(1/d)
      } else {
        vtheta = 1/d*diag(1)
      }

      fit2 <- list(model = "Piecewise exponential",
                   theta = log(d/ex),
                   vtheta = vtheta,
                   bic = -2*sum(-d + d*log(d/ex)) + J*log(n0),
                   aic = -2*sum(-d + d*log(d/ex)) + J*2,
                   piecewiseSurvivalTime = u)

      # fitted survival curve
      time = seq(0, max(df1$time))

      lambda = d/ex
      if (J>1) {
        psum = c(0, cumsum(lambda[1:(J-1)] * diff(u)))
      } else {
        psum = 0
      }
      j = findInterval(time, u)
      m = psum[j] + lambda[j]*(time - u[j])
      surv = exp(-m)

      dffit2 <- dplyr::tibble(time, surv)
    } else if (tolower(event_model) == "exponential with cured population") {
     
      temp<-stats::nlminb(start=c(-2,log(d0/ex0)),objective=loglik_Chen_exponential, df=df1)
      theta=temp$par
      fit2 <- list(model = 'exponential with cured population',
                   theta=temp$par,
                   vtheta=MASS::ginv(numDeriv::hessian(loglik_Chen_exponential,temp$par,df=df1)),
                   bic=2*temp$objective+log(n0)*2,
                   aic=2*temp$objective+2*2
      )
      pcng[i]=exp(temp$par[1])/(1+exp(temp$par[1]))
      # fitted survival curve
      dffit2 <- dplyr::tibble(
        time = seq(0, max(df1$time)),
        surv = SP_Chen_exponential(theta,.data$time))

    } else if (tolower(event_model) == "weibull with cured population") {
      reg <- survival::survreg(survival::Surv(time, event) ~ 1,
                               data = df1, dist = "weibull")
      
      # weibull$shape = 1/reg$scale, weibull$scale = exp(reg$coefficients)
      # we define theta = (log(weibull$shape), log(weibull$scale))
    
      
      
      temp<-stats::nlminb(start=c(-2,log(reg$scale),as.numeric(reg$coefficients)),objective=loglik_Chen_weibull, df=df1)
      theta=temp$par
      fit2 <- list(model = 'weibull with cured population',
                   theta=temp$par,
                   vtheta=MASS::ginv(numDeriv::hessian(loglik_Chen_weibull,temp$par,df=df1)),
                   bic=2*temp$objective+log(n0)*3,
                   aic=2*temp$objective+2*3
      )

      # fitted survival curve
      dffit2 <- dplyr::tibble(
        time = seq(0, max(df1$time)),
        surv = SP_Chen_weibull(theta,.data$time))
      pcng[i]=exp(temp$par[1])/(1+exp(temp$par[1]))
    } else if (tolower(event_model) == "log-logistic with cured population") {
      # S(t) = 1/(1 + (t/lambda)^kappa)
      reg <- survival::survreg(survival::Surv(time, event) ~ 1,
                               data = df1, dist = "loglogistic")
      
      # llogis$shape = 1/reg$scale, llogis$scale = exp(reg$coefficients)
      # we define theta = (log(llogis$shape),log(llogis$scale))
      
      
      
      temp<-stats::nlminb(start=c(-2,log(1/reg$scale),as.numeric(reg$coefficients)),objective=loglik_Chen_log_logistic, df=df1)
      theta=temp$par
      fit2 <- list(model = 'log-logistic with cured population',
                   theta=temp$par,
                   vtheta=MASS::ginv(numDeriv::hessian(loglik_Chen_log_logistic,temp$par,df=df1)),
                   bic=2*temp$objective+log(n0)*3,
                   aic=2*temp$objective+2*3
      )
      pcng[i]=exp(temp$par[1])/(1+exp(temp$par[1]))
      # fitted survival curve
      dffit2 <- dplyr::tibble(
        time = seq(0, max(df1$time)),
        surv = SP_Chen_log_logistic(theta,.data$time))
    } else if (tolower(event_model) == "log-normal with cured population") {
      # S(t) = 1 - Phi((log(t) - meanlog)/sdlog)
      reg <- survival::survreg(survival::Surv(time, event) ~ 1,
                               data = df1, dist = "lognormal")
      
      # we use parameterization theta = (meanlog, log(sdlog))
      
      
      temp<-stats::nlminb(start=c(-2,as.numeric(reg$coefficients), log(reg$scale)),objective=loglik_Chen_log_normal, df=df1)
      theta=temp$par
      fit2 <- list(model = 'log-normal with cured population',
                   theta=temp$par,
                   vtheta=MASS::ginv(numDeriv::hessian(loglik_Chen_log_normal,temp$par,df=df1)),
                   bic=2*temp$objective+log(n0)*3,
                   aic=2*temp$objective+2*3
      )
      pcng[i]=exp(temp$par[1])/(1+exp(temp$par[1]))
      # fitted survival curve
      dffit2 <- dplyr::tibble(
        time = seq(0, max(df1$time)),
        surv = SP_Chen_log_normal(theta,.data$time))
    } else if (tolower(event_model) == "piecewise exponential with cured population") {
      u = piecewiseSurvivalTime[piecewiseSurvivalTime < max(df1$time)]
      ucut = c(u, max(df1$time))
      J = length(u)

      temp<-stats::nlminb(start=c(-2,rep(log(0.0030),length(piecewiseSurvivalTime))),objective=loglik_Chen_piecewise_exponential,
                   df=df1,
                   piecewiseSurvivalTime=piecewiseSurvivalTime)


      theta=temp$par
      fit2 <- list(model = 'piecewise exponential with cured population',
                   theta=temp$par,
                   vtheta=MASS::ginv(numDeriv::hessian( loglik_Chen_piecewise_exponential,temp$par,df=df1,piecewiseSurvivalTime=piecewiseSurvivalTime)),
                   bic=2*temp$objective + (J+1)*log(n0),
                   aic=2*temp$objective + (J+1)*2,
                   piecewiseSurvivalTime = u
      )
      pcng[i]=exp(temp$par[1])/(1+exp(temp$par[1]))


      # fitted survival curve
      dffit2 <- dplyr::tibble(
        time = seq(0, max(df1$time)),
        surv =  SP_Chen_piecewise_exponential(theta,.data$time,piecewiseSurvivalTime=piecewiseSurvivalTime))
    } else if (tolower(event_model) == "model averaging") {
      reg1 <- survival::survreg(survival::Surv(time, event) ~ 1,
                                data = df1, dist = "weibull")
      reg2 <- survival::survreg(survival::Surv(time, event) ~ 1,
                                data = df1, dist = "lognormal")
      bic1 <- -2*reg1$loglik[1] + 2*log(n0)
      bic2 <- -2*reg2$loglik[1] + 2*log(n0)

      aic1 <- -2*reg1$loglik[1] + 2*2
      aic2 <- -2*reg2$loglik[1] + 2*2


      w1 = 1/(1 + exp(-0.5*(bic2 - bic1)))


      # model parameters from weibull and log-normal
      theta = c(as.numeric(reg1$coefficients), log(reg1$scale),
                as.numeric(reg2$coefficients), log(reg2$scale))

      # variance-covariance matrix, noting that the covariances
      # between the two sets of parameters are zero as they are estimated
      # from different likelihood functions
      vtheta = as.matrix(Matrix::bdiag(reg1$var, reg2$var))

      # model fit, assuming fixed weight w1
      fit2 <- list(model = "Model averaging",
                   theta = theta,
                   vtheta = vtheta,
                   bic = w1*bic1 + (1-w1)*bic2,
                   aic = w1*aic1 + (1-w1)*aic2,
                   w1 = w1)

      # distribution function for model averaging of Weibull and log-normal
      pmodavg <- function(t, theta, w1, lower.tail = TRUE, log.p = FALSE) {
        shape = exp(-theta[2])
        scale = exp(theta[1])
        meanlog = theta[3]
        sdlog = exp(theta[4])

        p1 = stats::pweibull(pmax(0,t), shape, scale)
        p2 = stats::plnorm(pmax(0,t), meanlog, sdlog)
        p = w1*p1 + (1-w1)*p2

        if (!lower.tail) p = 1 - p
        if (log.p) p = log(p)
        p
      }

      dffit2 <- dplyr::tibble(
        time = seq(0, max(df1$time)),
        surv = pmodavg(.data$time, theta, w1, lower.tail = FALSE))
    } else if (tolower(event_model) == "spline") {
      # g(S(t)) = gamma_0 +gamma_1*x +gamma_2*v_1(x) +... +gamma_{m+1}*v_m(x)

      spl <- flexsurv::flexsurvspline(survival::Surv(time, event) ~ 1,
                                      data = df1, k = k, scale = scale,
                                      method = "Nelder-Mead")

      fit2 <- list(model = "Spline",
                   theta = spl$coefficients,
                   vtheta = spl$cov,
                   bic = -2*spl$loglik + (k+2)*log(n0),
                   aic = -2*spl$loglik + (k+2)*2,
                   knots = spl$knots,
                   scale = spl$scale)

      # fitted survival curve
      dffit2 <- dplyr::tibble(
        time = seq(0, max(df1$time)),
        surv = flexsurv::psurvspline(.data$time, gamma = spl$coefficients,
                                     knots = spl$knots, scale = spl$scale,
                                     lower.tail = FALSE))
    }


    # plot the survival curve
    if (tolower(event_model) == "model averaging") {
      bictext = paste("Weighted BIC:", round(fit2$bic,2))
      aictext= paste("Weighted AIC:", round(fit2$aic,2))
    } else {
      bictext = paste("BIC:", round(fit2$bic,2))
      aictext = paste("AIC:", round(fit2$aic,2))
    }
    if(tolower(event_model)=="exponential with cured population"||
       tolower(event_model)=="weibull with cured population"||
       tolower(event_model)=="log-logistic with cured population"||
       tolower(event_model)=="log-normal with cured population"||
       tolower(event_model)=="piecewise exponential with cured population"){
       pc=paste("Prop of Cured:", round(pcng[i],2))
    } else {
      pc=paste("Prop of Cured: 0")
    }
    
    kmdf_censor=kmdf[kmdf$n.censor>0,]
    if(criterion=="bic"){
      fittedEvent <- plotly::plot_ly() %>%
        plotly::add_lines(
          data=kmdf, x=~time, y=~surv, name="Kaplan-Meier",
          line=list(shape="hv")) %>%
        plotly::add_markers(data=kmdf_censor, x=~time, y=~surv,
                            showlegend = FALSE,
                            marker = list(symbol = "cross-thin",size = 6,line = list(color = 'black',width = 2)),
                            name="censoring"
                            ) %>%
        plotly::add_lines(
          data=dffit2, x=~time, y=~surv, name="fitted") %>%
        plotly::layout(
          xaxis = list(title = "Days since randomization", zeroline = FALSE),
          yaxis = list(title = "Survival probability", zeroline = FALSE),
          title = list(text = "Fitted time to event survival curve"),
          annotations = list(
            x = c(0.75, 0.75,0.75), y = c(0.95, 0.85,0.75), xref = "paper",
            yref = "paper", text = paste('<i>', c(fit2$model, bictext,pc), '</i>'),
            xanchor = "left", font = list(size = 14, color = "red"),
            showarrow = FALSE)) %>%
        plotly::hide_legend()
    }
    if(criterion=="aic"){
      fittedEvent <- plotly::plot_ly() %>%
        plotly::add_lines(
          data=kmdf, x=~time, y=~surv, name="Kaplan-Meier",
          line=list(shape="hv")) %>%
        plotly::add_markers(data=kmdf_censor, x=~time, y=~surv,showlegend = FALSE,
                            marker = list(symbol = "cross-thin",size = 6,line = list(color = 'black',width = 2)),
                            name="censoring"
        ) %>%
        plotly::add_lines(
          data=dffit2, x=~time, y=~surv, name="fitted") %>%
        plotly::layout(
          xaxis = list(title = "Days since randomization", zeroline = FALSE),
          yaxis = list(title = "Survival probability", zeroline = FALSE),
          title = list(text = "Fitted time to event survival curve"),
          annotations = list(
            x = c(0.75, 0.75,0.75), y = c(0.95, 0.85,0.75), xref = "paper",
            yref = "paper", text = paste('<i>', c(fit2$model, aictext,pc), '</i>'),
            xanchor = "left", font = list(size = 14, color = "red"),
            showarrow = FALSE)) %>%
        plotly::hide_legend()


    }
    if(criterion=="both"){
      fittedEvent <- plotly::plot_ly() %>%
        plotly::add_lines(
          data=kmdf, x=~time, y=~surv, name="Kaplan-Meier",
          line=list(shape="hv")) %>%
        plotly::add_markers(data=kmdf_censor, x=~time, y=~surv,showlegend = FALSE,
                            marker = list(symbol = "cross-thin",size = 6,line = list(color = 'black',width = 2)),
                            name="censoring"
        ) %>%
        plotly::add_lines(
          data=dffit2, x=~time, y=~surv, name="fitted") %>%
        plotly::layout(
          xaxis = list(title = "Days since randomization", zeroline = FALSE),
          yaxis = list(title = "Survival probability", zeroline = FALSE),
          title = list(text = "Fitted time to event survival curve"),
          annotations = list(
            x = c(0.75, 0.75,0.75,0.75), y = c(0.95, 0.85,0.75,0.65), xref = "paper",
            yref = "paper", text =  paste('<i>', c(fit2$model, aictext,bictext,pc), '</i>'),
            xanchor = "left", font = list(size = 14, color = "red"),
            showarrow = FALSE)) %>%
        plotly::hide_legend()


    }


    if (by_treatment && ngroups > 1) {
      fittedEvent <- fittedEvent %>%
        plotly::layout(annotations = list(
          x = 0.5, y = 1,
          text = paste0("<b>", df1$treatment_description[1], "</b>"),
          xanchor = "center", yanchor = "middle", showarrow = FALSE,
          xref='paper', yref='paper'))
    }

    if (by_treatment) {
      fit2$treatment = df1$treatment[1]
      fit2$treatment_description = df1$treatment_description[1]
    }

    event_fit[[i]] = fit2
    g1[[i]] = fittedEvent
  }

  if (!by_treatment) {
    event_fit = fit2
    event_fit_plot = fittedEvent
  } else {
    event_fit_plot <- plotly::subplot(g1, nrows = ngroups, titleX = TRUE,
                                      titleY = TRUE, margin = 0.1)
  }



  list(event_fit = event_fit, event_fit_plot = event_fit_plot)
}
