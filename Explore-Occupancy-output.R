# explore model output
library(sparta)

# read in the model outputs
list.files('model-outputs') -> sp_mods

models <- lapply(sp_mods, function(sp) {
  load(file=paste0('model-outputs/',sp))
  return(out)
  }
  )
names(models) <- gsub(sp_mods, pa="\\.rdata", repl="")

# plot the occupancy
lapply(models, plot)

# now calculate the linear trends
trends <- lapply(models, occurrenceChange, firstYear=1980, lastYear=2013)
names(trends) <- gsub(sp_mods, pa="\\.rdata", repl="")

outputs <- data.frame(
  mean.trend = sapply(trends, function(x) x$mean),
  CI.lower = sapply(trends, function(x) x$CIs[1]),
  CI.upper = sapply(trends, function(x) x$CIs[2])) 

# do we get a different answer starting in 2010?
#sapply(models, occurrenceChange, firstYear=1980, lastYear=2010)

# let's look at the midwife toad nin more detail
summary(models$`Alytes obstetricans`)

models$`Alytes obstetricans`$BUGSoutput$summary
