#' @title Function to plot KM plot for time-to-event data in simulation
#' @description Plot KM plot for each arm at one realization
#' @param test0 an object obtained from test_procedure
#' @param plot.index the index of simulation 
#' @return A KM plot for events in each arm
#'
#' @examples
#' \donttest{
#' fit1 <- list(model = "piecewise uniform",
#'              theta = -0.58, 
#'              vtheta=0, accrualTime =0)
#' fit2<-list()
#' fit2[[1]] <- list(model = "weibull with cured population and delayed treatment", 
#'                   theta = c(0.5,0,6.5,0,1), 
#'                   vtheta = matrix(0,5,5))
#' fit2[[2]] <- list(model = "weibull with cured population and delayed treatment", 
#'                  theta = c(0.5,0,6.5,46,0.65), 
#'                  vtheta = matrix(0,5,5))
#' fit3<-list()
#' fit3[[1]] <- list(model = "weibull with cured population and delayed treatment", 
#'                   theta = c(0.5,0,6.5,0,1), 
#'                   vtheta = matrix(0,5,5))
#' fit3[[2]] <- list(model = "weibull with cured population and delayed treatment", 
#'                   theta = c(0.5,0,6.5,0,1),
#'                   vtheta = matrix(0,5,5))
#' fit4 <- list(model = "exponential", 
#'                    theta =log(0.0003), 
#'                    vtheta=0)
#'                    
#' test1<-test_procedure(pilevel=0.9,nyears=4,enroll_fit=fit1,
#'                       dropout_fit=fit4,enroll_prior=fit1,event_prior_h0=fit3,
#'                       event_prior_ha=fit2,dropout_prior=NULL,
#'                       target_n=200,target_IA_d=40,
#'                       target_d=60,ialpha=0.016,falpha=0.0450,
#'                       lag=46,by_fitted_enroll=FALSE,
#'                       by_fitted_dropout=FALSE,treatment_label=c('a','b'),
#'                       ngroups=2,alloc=c(1,1),nreps=10, IA_included=TRUE)
#' test_plot(test1)
#' }
#' @export
#'
test_plot<-function(test0,plot.index=1){


 target_FA_d<-test0$target_d
  pred_all<-test0$pred_all_HA
  target_n<-pred_all$enroll_pred$target_n
  i=plot.index

  a0<-pred_all$event_pred$newEvents
  a1<-a0[a0$draw==i,]
  grph1<-1*(a1$treatment==2)
  event_num=sum(a1$event)
  if(target_FA_d>event_num){
    message(paste("Warning: Simulation",i, ":The number of the events in the current setting is smaller the
               required number of events in FA"))
  }else{
    FA_time_cut<-sort(a1$totalTime[a1$event==1])[target_FA_d]
    
    data_FA<-a1[a1$arrivalTime<=FA_time_cut,]
    
    FA_censor<-1*(data_FA$event&data_FA$totalTime<=FA_time_cut)
    
    FA_surv_time<-pmin(data_FA$totalTime,FA_time_cut)-data_FA$arrivalTime
    grph1f<-grph1[a1$arrivalTime<=FA_time_cut]
    
    
    kmfit0<- survival::survfit(Surv(FA_surv_time[grph1f==0],FA_censor[grph1f==0])~1)
    kmdf0 <- dplyr::tibble(time = kmfit0$time, surv = kmfit0$surv,n.censor=kmfit0$n.censor)
    kmdf0 <- dplyr::tibble(time = 0, surv = 1,n.censor=0) %>%
      dplyr::bind_rows(kmdf0)
    kmfit1<- survival::survfit(Surv(FA_surv_time[grph1f==1],FA_censor[grph1f==1])~1)
    kmdf1 <- dplyr::tibble(time = kmfit1$time, surv = kmfit1$surv,n.censor=kmfit1$n.censor)
    kmdf1 <- dplyr::tibble(time = 0, surv = 1,n.censor=0) %>%
      dplyr::bind_rows(kmdf1)
    
    
    kmdf0_censor=kmdf0[kmdf0$n.censor>0,]
    kmdf1_censor=kmdf1[kmdf1$n.censor>0,]
    g1 <- plotly::plot_ly() %>%
      plotly::add_lines(data=kmdf0, x=~(time/30.4375), y=~surv, name="Control",line=list(shape="hv", width=1,color='red')) %>%
      plotly::add_markers(data=kmdf0_censor, x=~(time/30.4375), y=~surv,showlegend = FALSE,marker = list(symbol = "cross-thin",size = 6,line = list(color = 'black',width = 2
      ))) %>%
      plotly::add_lines(data=kmdf1, x=~(time/30.4375), y=~surv, name="Treatment",line=list(shape="hv", width=1,color='blue')) %>%
      plotly::add_markers(data=kmdf1_censor, x=~(time/30.4375), y=~surv,showlegend = FALSE,
                          marker = list(symbol = "cross-thin",size = 6,line = list(color = 'black', width = 2)),
                          name='censoring') %>%
      plotly::layout(xaxis = list(title = "Time (months)",
                                  zeroline = FALSE),
                     yaxis = list(title = "Survival Probability", zeroline = FALSE),
                     legend = list(x = 0, y = 1.2, orientation = "h"),
                     title=paste0("KM Plot for the Simu ", plot.index))
    g1
  }
}
