### Updating and cleaning the amphibian dataset so that we all work with all 20 speces from 1980 to 2014; Groups assigned: newts and frogs
library(sparta)
library(sf)
library(lubridate)

# REad the data from a local copy of the raw records
#data <- st_read('raw-data/Data_DGHT_Schulte/Data_DGHT_Schulte.dbf')
dght_dates<- st_read("raw data/all_specs_all_plots.shp")
dght_dates$Group<-NA
dght_dates$Group[dght_dates$taxon %in% c("Salamandra atra","Ichthyosaura alpestris","Lissotriton helveticus","Salamandra salamandra","Triturus cristatus","Lissotriton vulgaris")]<-c("newts")
dght_dates$Group[!(dght_dates$taxon %in% c("Salamandra atra","Ichthyosaura alpestris","Lissotriton helveticus","Salamandra salamandra","Triturus cristatus","Lissotriton vulgaris"))]<-c("frogs")


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


# plots (NJBI - I've commented thiese out in order to save time and memory)
#plot(dght_dates[3]) 
#plot(amph_date_precision [3], col="black",add=T)
##plot(amph_year_precision[3],add=T, col="red")
library(plyr)

visits_per_year_group<-ddply(amph_date_precision,.(start_year,MTB_Q,Group),summarise,nu=length(unique(BEGINN)))

## How many have been visited more than once; mor than one sampling dates in any year?
#table(temp$nu>=2)
# more than 5000
# how are those entries distributed?
#amph_more_than_1_visit<- amph_date_precision[amph_date_precision$MTB_Q %in% temp$MTB_Q[temp$nu>=2],]

# run the sparta diagnostic to look at list length over time
#dataDiagnostics(taxa = amph_date_precision$Species,
#                site = amph_date_precision$MTB_Q,
#                time_period = amph_date_precision$end_year)

# reduce the data to the subset from 1980 to 2013
amph_date_precision <- subset(amph_date_precision, end_year %in% 1980:2013)


