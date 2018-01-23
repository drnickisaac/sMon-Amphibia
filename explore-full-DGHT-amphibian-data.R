# sMon group Amphibians
#

library(sparta)
library(sf)
library(lubridate)

# REad the data from a local copy of the raw records
data <- st_read('raw-data/Data_DGHT_Schulte/Data_DGHT_Schulte.dbf')
dght_dates<- st_read("raw-data/Data_DGHT_Schulte/Data_DGHT_Schulte.shp")

# it should be possible to read this read directly from the zip file
#data <- st_read(unz('raw-data/Data_DGHT_Schulte.zip')) # not tried

dim(data) # 251222     24

## throw out all thse that have year range as precision
amph_year_precision<- dght_dates[(dght_dates$start_year==dght_dates$end_year),]

## filter for data with only years
amph_year_precision$day_st<- lubridate:: day(amph_year_precision$BEGINN)
amph_year_precision$day_end<- lubridate:: day(amph_year_precision$ENDE)
amph_year_precision$month_st<- lubridate:: month(amph_year_precision$BEGINN)
amph_year_precision$month_end<- lubridate:: month(amph_year_precision$ENDE)

amph_date_precision<- amph_year_precision[!((amph_year_precision$day_st==1) & (amph_year_precision$month_st==1)),]

## Only data with day information; throw out na's
amph_date_precision<- amph_date_precision[!is.na(amph_date_precision$BEGINN),]

nrow(amph_date_precision) # 102043

# plots (NJBI - I've commented thiese out in order to save time and memory)
#plot(dght_dates[3]) 
#plot(amph_date_precision [3], col="black",add=T)
##plot(amph_year_precision[3],add=T, col="red")

#temp<-ddply(dat2.1,.(start_year,MTB_Q),summarise,nu=length(unique(BEGINN)))

## How many have been visited more than once; mor than one sampling dates in any year?
#table(temp$nu>=2)
# more than 5000
# how are those entries distributed?
#amph_more_than_1_visit<- amph_date_precision[amph_date_precision$MTB_Q %in% temp$MTB_Q[temp$nu>=2],]

# run the sparta diagnostic to look at list length over time
dataDiagnostics(taxa = amph_date_precision$Species,
                site = amph_date_precision$MTB_Q,
                time_period = amph_date_precision$end_year)

# reduce the data to the subset from 1980
amph_date_precision <- subset(amph_date_precision, end_year %in% 1980:2013)

table(amph_date_precision$Family)

newt <- subset(amph_date_precision, Family %in% c('Salamandridae'))
anura <- subset(amph_date_precision, !Family %in% c('Salamandridae'))
  
# do the dagnostics separately for newts and frogs, with the post 1980 subset     
sapply(list(newt, anura), function(data){
  dataDiagnostics(taxa = data$Species,
                site = data$MTB_Q,
                time_period = data$end_year)
  }
  )

# there is a problem with the newt data - every list has two species
table(amph_date_precision$Species, amph_date_precision$Family)
# and there are only two spcies of newts in the dataset - if so you are riunning the old version of the data


##### let's set up a data set suitable for running an Occupancy model

anura_fod <- formatOccData(taxa= as.character(anura$Species), 
                           site= as.character(anura$MTB_Q), 
                           time_period = as.Date(anura$ENDE), 
                           includeJDay = TRUE)
# Warning - half the data appear to be duplicates
str(anura_fod)

table(anura_fod$occDetdata$L) 


#####  let's run a model

