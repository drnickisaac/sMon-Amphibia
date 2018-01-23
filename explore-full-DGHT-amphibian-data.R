

library(sparta)
library(sf)
library(lubridate)

data <- st_read('raw-data/Data_DGHT_Schulte/Data_DGHT_Schulte.dbf')
dght_dates<- st_read("raw-data/Data_DGHT_Schulte/Data_DGHT_Schulte.shp")

# later, try reading directly from the zip file
#data <- unz('raw-data/Data_DGHT_Schulte.zip')

dim(data) # 251222     24

source('R/DGHT_Amphibians.R')

# reduce the data to the subset
dataDiagnostics(taxa = amph_date_precision$Species,
                site = amph_date_precision$MTB_Q,
                time_period = amph_date_precision$end_year)

amph_date_precision <- subset(amph_date_precision, end_year %in% 1980:2013)

table(amph_date_precision$Family)

newt <- subset(amph_date_precision, Family %in% c('Salamandridae'))
anura <- subset(amph_date_precision, !Family %in% c('Salamandridae'))
       
sapply(list(newt, anura), function(data){
  dataDiagnostics(taxa = data$Species,
                site = data$MTB_Q,
                time_period = data$end_year)
}
)

table(amph_date_precision$Species, amph_date_precision$Family)
# there is a problem with the newt data - every list has two species


#####

anura_fod <- formatOccData(taxa= as.character(anura$Species), 
                           site= as.character(anura$MTB_Q), 
                           time_period = as.Date(anura$ENDE), 
                           includeJDay = TRUE)
# Warning - half the data appear to be duplicates
str(anura_fod)

hist(anura_fod$occDetdata$L)
