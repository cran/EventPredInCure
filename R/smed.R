#' @title Function to output summary statistics from survfit function output
#' @description Provide  summary statistics from survfit function output
#'
#' @param x an object returned by "survfit"
#'
#'
#' @return "smed" returns a matrix of 5 columns of
# 			number of subjects, events, median, 0.95LCL, 0.95UCL.
# 			The matrix returned has rownames as the
# 			group labels (eg., treatment arms) if any.
#'
#' @examples
#' n <- 500
#' event <- runif(n,1, 5)
#' osc<-1*(event<=4)
#' os <- pmin(event,4)
#' 
#' fit1<-survival::survfit(survival::Surv(os,osc)~1)
#' smed(fit1)
#' 
#' @export
#'
smed <- function(x) {

  ox <- utils::capture.output(print(x))
  n <- length(ox)
  tmp <- t(sapply(ox[4:n],
                  function(l) strsplit(l, split=' +')[[1]]))
  nres <- strsplit(ox[3],split=' +')[[1]][2:6]
  res <- matrix(suppressWarnings(as.numeric(tmp[,2:6])), ncol=5,
                dimnames=list(tmp[,1], nres))
  res


}
