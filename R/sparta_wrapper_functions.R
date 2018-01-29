# calculate the linear trends

#@ models - a list of all the sparta models for each species

calculateTrends<-function(models){
  
  #get trends for each model
  trends <- lapply(models, occurrenceChange, firstYear=min(df$Year), lastYear=max(df$Year))
  names(trends) <- gsub(sp_mods, pa="\\.rdata", repl="")
  
  #convert into a data frame
  outputs <- data.frame(
    mean.trend = sapply(trends, function(x) x$mean),
    CI.lower = sapply(trends, function(x) x$CIs[1]),
    CI.upper = sapply(trends, function(x) x$CIs[2])) 
  
  #return it
  return(outputs)
}

#get annual predictions for each species
#@ models - a list of all the sparta models for each species

annualPredictions <- function(models){

  library(plyr)
  ldply(models,function(x){
  
  #get annual predictions
  temp <- data.frame(summary(x))
  temp$Year<-as.numeric(row.names(summary(x)))
  temp$Species <- x$SPP_NAME
  
  #get RHat values
  bugsOutput <- x$BUGSoutput$summary
  bugsOutput <- data.frame(bugsOutput[grepl("psi.fs",row.names(bugsOutput)),])
  temp$Rhat <- as.numeric(bugsOutput$Rhat)
  
  return(temp)
})
  
}

#plot these predictions (restrict to species with more than 50 observations)
#@ myAnnualPredictions - the annual predictions returned by the above function
#@ rawData - the original data file of species occurrence records

plotPredictions <- function(myAnnualPredictions,rawData){
  
require(ggplot2)
ggplot(data=subset(myAnnualPredictions,Species %in% names(table(rawData$Species))[table(rawData$Species)>50])) +
  geom_line(aes(x = Year, mean))+
  geom_point(aes(x = Year, mean,colour = factor(Rhat<1.1)))+
  geom_ribbon(aes(x=Year, ymin = quant_025, ymax = quant_975), alpha=0.50)+
  theme_bw() +
  scale_x_continuous(labels=c(1990,1995,2000,2005,2010))+
  facet_wrap( ~ Species) +
  theme(legend.position = "none")
}