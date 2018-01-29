# explore model output
library(sparta)

# read in the model outputs
list.files('model-outputs') -> sp_mods

models <- lapply(sp_mods, function(sp) {
  load(file=paste0('model-outputs/',sp))
  return(out)
})

names(models) <- gsub(sp_mods, pa="\\.rdata", repl="")

# check whether the target directory already exists. If not, create it.
if(!dir.exists('figs')) dir.create('figs')

# plot the occupancy and save a copy
for (i in 1:length(models)){
  p <- plot(models[[i]])
  png(file=paste0("figs/", names(models)[i], ".png"))
  print(p)
  dev.off()
}