
library(sparta)

# DAVID to modify to that this script now imports the clean version of the anura data

source("R/Raw_data_preparation_for_amphibian_data.R")

## we work now with amph_date_precision


##### let's set up a data set suitable for running an Occupancy model

anura_fod <- formatOccData(taxa= as.character(amph_date_precision$Species[amph_date_precision$Group=="frogs"]), 
                           site= as.character(amph_date_precision$MTB_Q[amph_date_precision$Group=="frogs"]), 
                           time_period = as.Date(amph_date_precision$ENDE[amph_date_precision$Group=="frogs"]), 
                           includeJDay = TRUE)
# Warning - half the data appear to be duplicates - to investigate
str(anura_fod)


table(anura_fod$occDetdata$L) 


# set the sparta options
sparta_options <- c('ranwalk', # prior on occupancy is set by last year's posterior
                    'jul_date', # use the Julian date as a covariate on the detection probability
                    'catlistlength',# categorises the visits into three sets of 'qualities'
                    'halfcauchy') ## adds hyperprior on the precision

#####  let's run a model. # this is really slow!
anura_occmod <- occDetModel(taxa= as.character(amph_date_precision$Species[amph_date_precision$Group=="frogs"]), 
                            site= as.character(amph_date_precision$MTB_Q[amph_date_precision$Group=="frogs"]), 
                            time_period = as.Date(amph_date_precision$ENDE[amph_date_precision$Group=="frogs"]), 
                            modeltype= sparta_options,
                            n_iterations = 20000)

newts_occmod <- occDetModel(taxa= as.character(amph_date_precision$Species[amph_date_precision$Group=="newts"]), 
                            site= as.character(amph_date_precision$MTB_Q[amph_date_precision$Group=="newts"]), 
                            time_period = as.Date(amph_date_precision$ENDE[amph_date_precision$Group=="newts"]), 
                            modeltype= sparta_options,
                            n_iterations = 20000)



### Load the models
list.files('model-outputs') -> sp_mods

models<- lapply(sp_mods, functions())

