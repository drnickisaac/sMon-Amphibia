library(sparta)

# import a clean version of the data
load('derived-data/anura_date_precision.rData') # -> anura
#load('derived-data/newts_date_precision.rData') # -> newts

anura -> data
fod <- formatOccData(taxa = as.character(data$Species), 
                site = as.character(data$MTB_Q), 
                time_period = as.Date(data$ENDE),
                includeJDay = TRUE
    )
str(fod)

summary(fod$occDetdata$Jul_date) # most in range 100-200

# why do we get duplicates