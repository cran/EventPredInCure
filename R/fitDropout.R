#' @title Fit time-to-dropout model
#' @description Fits a specified time-to-dropout model to the dropout data.
#'
#' @param df The subject-level dropout data, including \code{time} and
#'   \code{dropout}. The data should also include \code{treatment}
#'   coded as 1, 2, and so on, and \code{treatment_description}
#'   for fitting the dropout model by treatment.
#' @param dropout_model The dropout model used to analyze the dropout data
#'   which can be set to one of the following options: "exponential",
#'   "Weibull", "log-logistic", "log-normal", or "piecewise exponential".
#'   By default, it is set to "exponential".
#' @param piecewiseDropoutTime A vector that specifies the time
#'   intervals for the piecewise exponential dropout distribution.
#'   Must start with 0, e.g., c(0, 60) breaks the time axis into 2
#'   event intervals: [0, 60) and [60, Inf). By default, it is set to 0.
#' @param by_treatment A Boolean variable to control whether or not to
#'   fit the time-to-dropout data by treatment group. By default,
#'   it is set to \code{FALSE}.
#'
#' @param criterion A character variable to denote the criterion in model
#' selection to shown in the figure, which can be set to one of the following
#' options: "aic","bic" or "both". By default,it is set to \code{both}.
#'
#'
#' @return A list of results from the model fit including key information
#'   such as the dropout model, \code{model}, the estimated model parameters,
#'   \code{theta}, the covariance matrix, \code{vtheta}, as well as the
#'   Bayesian Information Criterion, \code{bic}, and Akaike Information Criterion, \code{aic}.
#'
#'   If the piecewise exponential model is used, the location
#'   of knots used in the model, \code{piecewiseDropoutTime}, will
#'   be included in the list of results.
#'
#'   When fitting the dropout model by treatment, the outcome is presented
#'   as a list of lists, where each list element corresponds to a
#'   specific treatment group.
#'
#'   The fitted time-to-dropout survival curve is also returned.
#'   
#'
#' @references \itemize{
#' \item Royston, Patrick, and Mahesh KB Parmar. 
#' "Flexible parametric proportional‐hazards and proportional‐odds models for censored survival data, 
#' with application to prognostic modelling and estimation of treatment effects." 
#' Statistics in medicine 21.15 (2002): 2175-2197.
#' }
#' 
#' @examples
#'
#' dropout_fit <- fitDropout(df = interimData2,
#'                           dropout_model = "exponential")
#' 
#' @export
#'
fitDropout <- function(df, dropout_model = "exponential",
                       piecewiseDropoutTime = 0,
                       by_treatment = FALSE,criterion="both") {
  erify::check_class(df, "data.frame")

  erify::check_content(tolower(criterion),
                       c("aic", "bic",
                         "both"))

  erify::check_content(tolower(dropout_model),
                       c("exponential", "weibull", "log-logistic",
                         "log-normal", "piecewise exponential"))

  if (piecewiseDropoutTime[1] != 0) {
    stop("piecewiseDropoutTime must start with 0");
  }
  if (length(piecewiseDropoutTime) > 1 &
      any(diff(piecewiseDropoutTime) <= 0)) {
    stop("piecewiseDropoutTime should be increasing")
  }


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

  # fit by treatment group
  dropout_fit <- list()
  g1 <- list()

  for (i in 1:ngroups) {
    df1 <- df %>% dplyr::filter(.data$treatment == i)

    n0 = nrow(df1)
    c0 = sum(df1$dropout)
    ex0 = sum(df1$time)

    erify::check_positive(c0, supplement = paste(
      "The number of dropouts must be positive to fit a dropout model."))

    kmfit <- survival::survfit(survival::Surv(time, dropout) ~ 1, data = df1)
    kmdf <- dplyr::tibble(time = kmfit$time, surv = kmfit$surv)
    kmdf <- dplyr::tibble(time = 0, surv = 1) %>%
      dplyr::bind_rows(kmdf)

    if (tolower(dropout_model) == "exponential") {
      # lambda(t) = lambda
      # S(t) = exp(-lambda*t)

      fit3 <- list(model = 'Exponential',
                   theta = log(c0/ex0),
                   vtheta = 1/c0,
                   bic = -2*(-c0 + c0*log(c0/ex0)) + log(n0),
                   aic = -2*(-c0 + c0*log(c0/ex0)) + 2)

      # fitted survival curve
      dffit3 <- dplyr::tibble(
        time = seq(0, max(df1$time)),
        surv = stats::pexp(.data$time, rate = exp(fit3$theta), lower.tail = FALSE))

    } else if (tolower(dropout_model) == "weibull") {
      # lambda(t) = kappa/lambda*(t/lambda)^(kappa-1)
      # S(t) = exp(-(t/lambda)^kappa)

      reg <- survival::survreg(survival::Surv(time, dropout) ~ 1,
                               data = df1, dist = "weibull")

      # weibull$shape = 1/reg$scale, weibull$scale = exp(reg$coefficients)
      # we define theta = c(log(weibull$scale), -log(weibull$shape))
      # reg$var is for theta = c(reg$coefficients, log(reg$scale))
      fit3 <- list(model = "Weibull",
                   theta = c(as.numeric(reg$coefficients), log(reg$scale)),
                   vtheta = reg$var,
                   bic = -2*reg$loglik[1] + 2*log(n0),
                   aic = -2*reg$loglik[1] + 2*2)

      # fitted survival curve
      dffit3 <- dplyr::tibble(
        time = seq(0, max(df1$time)),
        surv = stats::pweibull(.data$time, shape = exp(-fit3$theta[2]),
                        scale = exp(fit3$theta[1]), lower.tail = FALSE))
    } else if (tolower(dropout_model) == "log-logistic") {
      # S(t) = 1/(1 + (t/lambda)^kappa)
      reg <- survival::survreg(survival::Surv(time, dropout) ~ 1,
                               data = df1, dist = "loglogistic")

      # llogis$shape = 1/reg$scale, llogis$scale = exp(reg$coefficients)
      # we define theta = (log(llogis$scale), -log(llogis$shape))
      # reg$var is for theta = c(reg$coefficients, log(reg$scale))
      fit3 <- list(model = "Log-logistic",
                   theta = c(as.numeric(reg$coefficients), log(reg$scale)),
                   vtheta = reg$var,
                   bic = -2*reg$loglik[1] + 2*log(n0),
                   aic = -2*reg$loglik[1] + 2*2)

      # fitted survival curve
      dffit3 <- dplyr::tibble(
        time = seq(0, max(df1$time)),
        surv = stats::plogis(log(.data$time), location = fit3$theta[1],
                      scale = exp(fit3$theta[2]), lower.tail = FALSE))
    } else if (tolower(dropout_model) == "log-normal") {
      # S(t) = 1 - Phi((log(t) - meanlog)/sdlog)
      reg <- survival::survreg(survival::Surv(time, dropout) ~ 1,
                               data = df1, dist = "lognormal")

      # we use parameterization theta = (meanlog, log(sdlog))
      # reg$var is for c(reg$coefficients, log(reg$scale)) = theta
      fit3 <- list(model = "Log-normal",
                   theta = c(as.numeric(reg$coefficients), log(reg$scale)),
                   vtheta = reg$var,
                   bic = -2*reg$loglik[1] + 2*log(n0),
                   aic = -2*reg$loglik[1] + 2*2)

      # fitted survival curve
      dffit3 <- dplyr::tibble(
        time = seq(0, max(df1$time)),
        surv = stats::plnorm(.data$time, meanlog = fit3$theta[1],
                      sdlog = exp(fit3$theta[2]), lower.tail = FALSE))
    } else if (tolower(dropout_model) == "piecewise exponential") {
      # lambda(t) = lambda[j] for ucut[j] < t <= ucut[j+1], j = 1,...,J
      # where ucut[1]=0< ucut[2]< ...< ucut[J]< ucut[J+1]=Inf are the knots
      u = piecewiseDropoutTime[piecewiseDropoutTime < max(df1$time)]
      ucut = c(u, max(df1$time))
      J = length(u)

      d = rep(NA, J)  # number of events in each interval
      ex = rep(NA, J) # total exposure in each interval
      for (j in 1:J) {
        d[j] = sum((df1$time > ucut[j]) * (df1$time <= ucut[j+1]) *
                     (df1$dropout == 1))
        ex[j] = sum(pmax(0, pmin(df1$time, ucut[j+1]) - ucut[j]))
      }

      # maximum likelihood estimates and covariance matrix
      if (J > 1) {
        vtheta = diag(1/d)
      } else {
        vtheta = 1/d*diag(1)
      }

      fit3 <- list(model = "Piecewise exponential",
                   theta = log(d/ex),
                   vtheta = vtheta,
                   bic = -2*sum(-d + d*log(d/ex)) + J*log(n0),
                   aic = -2*sum(-d + d*log(d/ex)) + J*2,
                   piecewiseDropoutTime = u)

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

      dffit3 <- dplyr::tibble(time, surv)
    }


    bictext = paste("BIC:", round(fit3$bic,2))
    aictext = paste("AIC:", round(fit3$aic,2))
    # plot the survival curve
    if(criterion=="bic"){
      fittedDropout <- plotly::plot_ly() %>%
        plotly::add_lines(
          data=kmdf, x=~time, y=~surv, name="Kaplan-Meier",
          line=list(shape="hv")) %>%
        plotly::add_lines(
          data=dffit3, x=~time, y=~surv, name="fitted") %>%
        plotly::layout(
          xaxis = list(title = "Days since randomization", zeroline = FALSE),
          yaxis = list(title = "Survival probability", zeroline = FALSE),
          title = list(text = "Fitted time to dropout survival curve"),
          annotations = list(
            x = c(0.75, 0.75), y = c(0.95, 0.85), xref = "paper",
            yref = "paper", text = paste('<i>', c(fit3$model, bictext), '</i>'),
            xanchor = "left", font = list(size = 14, color = "red"),
            showarrow = FALSE)) %>%
        plotly::hide_legend()
    }

    if(criterion=="aic"){
      fittedDropout <- plotly::plot_ly() %>%
        plotly::add_lines(
          data=kmdf, x=~time, y=~surv, name="Kaplan-Meier",
          line=list(shape="hv")) %>%
        plotly::add_lines(
          data=dffit3, x=~time, y=~surv, name="fitted") %>%
        plotly::layout(
          xaxis = list(title = "Days since randomization", zeroline = FALSE),
          yaxis = list(title = "Survival probability", zeroline = FALSE),
          title = list(text = "Fitted time to dropout survival curve"),
          annotations = list(
            x = c(0.75, 0.75), y = c(0.95, 0.85), xref = "paper",
            yref = "paper", text = paste('<i>', c(fit3$model, aictext), '</i>'),
            xanchor = "left", font = list(size = 14, color = "red"),
            showarrow = FALSE)) %>%
        plotly::hide_legend()
    }

    if(criterion=="both"){
      fittedDropout <- plotly::plot_ly() %>%
        plotly::add_lines(
          data=kmdf, x=~time, y=~surv, name="Kaplan-Meier",
          line=list(shape="hv")) %>%
        plotly::add_lines(
          data=dffit3, x=~time, y=~surv, name="fitted") %>%
        plotly::layout(
          xaxis = list(title = "Days since randomization", zeroline = FALSE),
          yaxis = list(title = "Survival probability", zeroline = FALSE),
          title = list(text = "Fitted time to dropout survival curve"),
          annotations = list(
            x = c(0.75, 0.75,0.75), y = c(0.95, 0.85,0.75), xref = "paper",
            yref = "paper", text =  paste('<i>', c(fit3$model, aictext,bictext), '<i>'),
            xanchor = "left", font = list(size = 14, color = "red"),
            showarrow = FALSE)) %>%
        plotly::hide_legend()
    }


    if (by_treatment && ngroups > 1) {
      fittedDropout <- fittedDropout %>%
        plotly::layout(annotations = list(
          x = 0.5, y = 1,
          text = paste0("<b>", df1$treatment_description[1], "</b>"),
          xanchor = "center", yanchor = "middle", showarrow = FALSE,
          xref='paper', yref='paper'))
    }

    if (by_treatment) {
      fit3$treatment = df1$treatment[1]
      fit3$treatment_description = df1$treatment_description[1]
    }

    dropout_fit[[i]] = fit3
    g1[[i]] = fittedDropout
  }

  if (!by_treatment || ngroups == 1) {
    dropout_fit = fit3
    dropout_fit_plot = fittedDropout
  } else {
    dropout_fit_plot <- plotly::subplot(g1, nrows = ngroups, titleX = TRUE,
                                        titleY = TRUE, margin = 0.1)
  }



  list(dropout_fit = dropout_fit, dropout_fit_plot = dropout_fit_plot)
}
