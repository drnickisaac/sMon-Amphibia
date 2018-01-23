library(sparta)

# import a clean version of the data
load('derived-data/anura_date_precision.rData') # -> anura
load('derived-data/newts_date_precision.rData') # -> newts

# set the sparta options
sparta_options <- c('ranwalk', # prior on occupancy is set by last year's posterior
                    'jul_date', # use the Julian date as a covariate on the detection probability
                    'catlistlength', # categorises the visits into three sets of 'qualities'
                    'halfcauchy') # prior on the precisions

#####  let's run a model for each species. # this is really slow!
system.time({
  occmods <- lapply(list(anura, newts), function(data){
                occDetModel(taxa= as.character(data$Species), 
                       site= as.character(data$MTB_Q), 
                       time_period = as.Date(data$ENDE),
                       modeltype = sparta_options,
                       n_iterations = 20000
                )})
})