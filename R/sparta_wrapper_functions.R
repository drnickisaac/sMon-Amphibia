#given the directory in which the sparta model outputs are found, put all the models in a list
#@param dir = directory of sparta model outputs

getSpartaModels<-function(dir){
  
  if(!dir%in%list.files()) stop('no such directory found')  
  
  sp_mods <- list.files(dir)[grepl(".rdata",list.files(dir))] 
  
  models <- lapply(sp_mods, function(sp) {
    load(file=paste0(paste0(dir,"/"),sp))
    return(out)
  }
  )
  
  names(models) <- gsub(sp_mods, pa="\\.rdata", repl="")
  
  return(models)
}

#get annual predictions of occupancy for each species
#@param models = output of getSpartaModels()

annualPredictions <- function(models){
 
  require(sparta)
  require(plyr)
  
  out<-ldply(models,function(x){
  
  #get bugs output
  temp  <- x$BUGSoutput$summary
  temp1 <- temp[grep("psi.fs\\[", row.names(temp)),]
  temp2 <- temp[grep("psi.fs.r_", row.names(temp)),]
  temp <- data.frame(rbind(temp1,temp2))
  names(temp) <- gsub("X2.5.","quant_025", names(temp)) 
  names(temp) <- gsub("X97.5.","quant_975", names(temp)) 
  temp$Region<- NA
  temp$Region[grep("psi.fs\\[",x=row.names(temp))]<-c("all")
  temp$Region[grep("psi.fs.r",x=row.names(temp))]<- substr(x=gsub(x=gsub(x=row.names(temp)[grep("psi.fs.r",x=row.names(temp))],"psi.fs.r_",""),"[0-9]",""),1,nchar(x=gsub(x=gsub(x=row.names(temp)[grep("psi.fs.r",x=row.names(temp))],"psi.fs.r_",""),"[0-9]",""))-2)
  #add on year and species info
  temp$Year <- x$min_year:x$max_year
  temp$Species <- x$SPP_NAME  
    
  #get records per species
  temp$nuRecords <- x$species_observations
  temp$nuSites <- x$nsites
  
  #reorganise
  temp<-temp[,c("Species","Year","mean","quant_025","quant_975","Rhat","n.eff","nuRecords","nuSites","Region")]
  
  return(temp)
})

  out<-out[,-1]#get rid of superfluous first column
  return(out)

}

#plot these predictions (restrict to species with more than 50 observations)
#@param myAnnualPredictions = output from annualPredictions()

plotPredictions <- function(myAnnualPredictions, regions_to_include=NULL){

#decide on year breaks to nearest decade
  if(is.null(regions_to_include)) regions_to_include <- levels(as.factor(myAnnualPredictions$Region))
  regions_to_include <- gsub("-","_",regions_to_include)
  df<- subset(myAnnualPredictions,nuRecords>50 & Region %in% c(regions_to_include))
surveyYears<-sort(unique(df$Year))
surveyYears<-surveyYears[surveyYears%%10==0]
pointcols<- ifelse(df$Rhat<1.1, "cadetblue3","salmon")
pointpch<- ifelse(df$Rhat<1.1, 16,6)
require(ggplot2)
  
#plot
ggplot(data=df) +
  geom_line(aes(x = Year, mean, color=Region), lty=1, lwd=1.3)+
  geom_ribbon(aes(x=Year, ymin = quant_025, ymax = quant_975, fill=Region), alpha=0.30)+
  geom_point(aes(x = Year, mean),color= pointcols, size=1.5, shape=pointpch)+
  theme_bw() +
  scale_x_continuous(breaks = surveyYears, labels = surveyYears)+
  facet_wrap( ~ Species) +
  theme(legend.position ="bottom") +
  ylab("Predicted occupancy proportion")
}


#get estimates of each species linear population trends and 95% CI
#@param models = output of getSpartaModels()
calculateTrends<-function(models){

  #get trends for each model
  trends <- lapply(models, function(x) occurrenceChange(bayesOut= x, firstYear=x$min_year, lastYear=x$max_year))
  names(trends) <- sapply(models,function(x) x$SPP_NAME)
  
  #convert into a data frame
  outputs <- data.frame(
    mean.trend = sapply(trends, function(x) x$mean),
    CI.lower = sapply(trends, function(x) x$CIs[1]),
    CI.upper = sapply(trends, function(x) x$CIs[2])) 
  
  #add number of total records for each species
  outputs$nuRecords <- sapply(models,function(x) x$species_observations)
  
  # indicates wheter the trend we see is 'significant' or not
  outputs$sig<- ifelse(outputs$CI.lower <0 & outputs$CI.upper <0, "-", 
                       ifelse(outputs$CI.lower >0 & outputs$CI.upper >0, "+", "0") )
  
  #return it
  return(outputs)
}



#Plot the MCMC chains for each occupancy parameter
#@models is the output of getSpartaModels

plotModels<-function(models,param="psi.fs\\["){
  
  #create a directory to put the traceplots
  newdir <- file.path("model-outputs", "traceplots")
  dir.create(newdir,showWarnings = FALSE)
  if (!grepl(newdir,getwd())){
    setwd(newdir)
  }
  
  #remove any previous plotting files, if there are any
  if(length(list.files())>0){
    file.remove(list.files())
  }
  
  #for each species, do the following
  lapply(models,function(x){
    require(reshape2)
    #get the chains
    df <- melt(x$BUGSoutput$sims.array)
    #give sensible names
    names(df)<-c("Index","Chain","Parameter","Estimate")
    #subset to occupancy parameter
    df <- subset(df,grepl(param,df$Parameter))
    
    #if there are many parameters (i.e. years), just plot every other year
    if(length(unique(df$Parameter))>20){
      df$ParamNu <- as.numeric(sub(".*\\[([^][]+)].*", "\\1", df$Parameter))
      df <- df[df$ParamNu%%2==0,]
    }
    
    #plot it
    require(ggplot2)
    ggplot(df)+
      geom_path(aes(x=Index,y=Estimate,colour=factor(Chain)))+
      facet_wrap(~Parameter,scales="free",ncol=4)+
      theme_bw()+
      theme(legend.position="none")
    ggsave(paste0(x$SPP_NAME,"_traceplot.png"))
    
  })
  
}
