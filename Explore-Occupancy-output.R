# explore model output
library(sparta)

# read in the model outputs
list.files('model-outputs') -> sp_mods

models <- lapply(sp_mods, function(sp) {
  load(file=paste0('model-outputs/',sp))
  return(out)
  }
  )

# plot the occupancy
lapply(models, plot)
