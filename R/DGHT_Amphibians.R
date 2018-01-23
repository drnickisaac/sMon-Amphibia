
#amphib_dght<- readRDS("C:/owncloud/David/sMon - DE/Daten/Data Portal/CSVs/DGHT_Schulte_all_specs.rds")
#### Brandenburg data: time intervals are no real dates;
## we could use these data as an additional information 
## in the hierarchical model at the end of the analyses.
## 


### We can compute "raster frequencies" as a measure of 
##  sampling intensity



#### Which date information do we have?

### We have data with days and data with only years
## therefore we need the original data from Schulte DGHT
#library(sf)
#dght_dates<- st_read("C:/owncloud/David/sMon - DE/Daten/Eingegangene Daten/Amphibiendaten/DGHT-Schulte/Einzeldatens?tze_ausg_Arten/Data_DGHT_Schulte/Data_DGHT_Schulte.shp")

######## END OF NJBI COMMENTS, so the rest can be read as source()

### If we have days we can also incoroporate the
## phenology of the species. These would look like this:

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

#plot(dght_dates[3]) #NJBI
plot(amph_year_precision[3],add=T, col="red")
plot(amph_date_precision [3], col="black",add=T)

temp<-ddply(dat2.1,.(start_year,MTB_Q),summarise,nu=length(unique(BEGINN)))

## How many have been visited more than once; mor than one sampling dates in any year?
table(temp$nu>=2)
# more than 5000
# how are those entries distributed?
amph_more_than_1_visit<- amph_date_precision[amph_date_precision$MTB_Q %in% temp$MTB_Q[temp$nu>=2],]
plot(amph_more_than_1_visit [3], col="red", add=T)


###### we loose a lot of federal states, but increase the 
## data quality


## On the other hand we could analyze whole germany
## 
### We can do both!

# How many MTB_Q do we have with how many visits per year?

library(plyr)
head(temp)
hist(temp$nu)


#############
### 2 groups: do more exploratory graphs
###           write the code for the model

## separate/indicate two groups: frogs and newts

