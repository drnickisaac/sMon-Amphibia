library(sparta)

# DAVID to modify to that this script now imports the clean version of the anura data

load('derived-data/anura_date_precision.rData')

anura_fod <- formatOccData(taxa= as.character(anura$Species), 
                           site= as.character(anura$MTB_Q), 
                           time_period = as.Date(anura$ENDE), 
                           includeJDay = TRUE)
# Warning - half the data appear to be duplicates - to investigate
str(anura_fod)

table(anura_fod$occDetdata$L) 


# set the sparta options
sparta_options <- c('ranwalk', # prior on occupancy is set by last year's posterior
                    'jul_date', # use the Julian date as a covariate on the detection probability
                    'catlistlength', # categorises the visits into three sets of 'qualities'
                    'halfcauchy') # prior on the precisions

#####  let's run a model. # this is really slow!
anura_occmod <- occDetModel(taxa= as.character(anura$Species), 
                            site= as.character(anura$MTB_Q), 
                            time_period = as.Date(anura$ENDE),
                            modeltype = sparta_options,
                            n_iterations = 5000
)