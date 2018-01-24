# ultimately this will be a function

# let's have  alook at one of the salamanders
load("model-outputs/Lissotriton helveticus.rData") # -> out 
sims_list <- out$BUGSoutput$sims.list
rm(out)
spname <- "Lissotriton helveticus"

detProb <- function(sims_list, spname=NULL){
  # calculates and plots the detection probability for an occupancy model output
  require(sparta)
  require(reshape2)
  require(plyr)
  require(boot)
  require(ggplot2)
  
  # the base: alpha.p is common to all models: 
  # it's the logit probability of detection on a single species list
  pDet1 <- sims_list$alpha.p
  # pDet1 is an array of dims equal to (niter, nyr)
  
  if("beta1" %in% names(sims_list)){
    # we ran the Julian Date option
    # So let's scale the detection probabilities to end June (day 180)
    pDet1 <- apply(pDet1, 2, function(x) 
      x + 180 * sims_list$beta1[,1] +  180^2 * sims_list$beta2[,1]
      )
  }
  
  # now calculate the equivalent values for lists of length 2 and 4 
  if("LL.p" %in% names(sims_list)){
    # the model was fitted with continuous list length
    pDet2 <- pDet1 + sims_list$LL.p * log(2)
    pDet4 <- pDet1 + sims_list$LL.p * log(4)
  } else if("dtype2.p" %in% names(sims_list)){
    # the model was fitted with categorical list length
    pDet2 <- pDet1 + sims_list$dtype2.p[,1]
    pDet4 <- pDet1 + sims_list$dtype3.p[,1]
  } 
  # there is also an option to ignore list length, 
  # in which case the probability of detection is assumed to be constant across surveys
  # i.e. if the survey was systematic
  
  pDet <- melt(list(pDet1, pDet2, pDet4))
  names(pDet) <- c("it", "year", "lgt_pDet", "ListLength")
  pDet$ListLength[pDet$ListLength==3] <- 4 # the "third" category is for a list of length 4
  
  pDet$pDet <- inv.logit(pDet$lgt_pDet)
  
  # now summarize these posterior distributions
  pDet_summary <-ddply(
        pDet, .(year, ListLength), summarise, 
        mean_pDet = mean(pDet),
        lower95CI = quantile(pDet, 0.025),
        upper95CI = quantile(pDet, 0.975))
  
  # now plot the detection over time
  gp <- ggplot(data=pDet_summary, x=year, y=mean_pDet) +
    geom_line(aes(x=year, y=mean_pDet, col=factor(ListLength))) +
    geom_ribbon(aes(x=year, ymin=lower95CI, ymax=upper95CI, fill=factor(ListLength)), alpha=0.2) +
    ylab("Detection probability") +
    ggtitle(spname) +
    theme_bw()
  gp 

}


recording_phenology <- function(sims_list, spname=NULL){
  # now calculate the phenology of detection
  # calculates and plots the detection probability for an occupancy model output
  require(sparta)
  require(reshape2)
  require(plyr)
  require(boot)
  require(ggplot2)
  
  # the base: alpha.p is common to all models: 
  # it's the logit probability of detection on a single species list
  pDet1 <- sims_list$alpha.p
  # pDet1 is an array of dims equal to (niter, nyr)

  jul_dates <- 1:36*10
  
  if("beta1" %in% names(sims_list)){
    # we ran the Julian Date option
    # So let's scale the detection probabilities to end June (day 180)
    JDadj <- sapply(jul_dates, function(jd){
      jd * sims_list$beta1[,1] +  jd* sims_list$beta2[,1]
    })
  }
  
  pDet <- melt(JDadj)
  names(pDet) <- c("it", "jd","lgt_pDet")
  pDet$pDet <- inv.logit(pDet$lgt_pDet)
  
  # now summarize these posterior distributions
  pDet_summary <-ddply(
    pDet, .(jd), summarise, 
    mean_pDet = mean(pDet),
    lower95CI = quantile(pDet, 0.025),
    upper95CI = quantile(pDet, 0.975))
  
  # now convert the jds back to their equivalent Julian Dates
 o  Ï◊¿
}
