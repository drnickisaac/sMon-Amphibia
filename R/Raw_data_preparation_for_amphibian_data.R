### Updating and cleaning the amphibian dataset so that we all work with all 20 speces from 1980 to 2014; Groups assigned: newts and frogs
library(sparta)
library(sf)
library(lubridate)
library(RColorBrewer)
library(reshape2)
library(ggplot2)
library(sp)
library(colorRamps)

### Read in Federal states Data

fed_states<- st_read("raw-data/Bundeslaender.shp")
fed_states_WGS84<- st_transform(fed_states, crs="+proj=longlat +datum=WGS84 +no_defs")
rm(fed_states)


## The geometry value for the saxony state data should be MTB_Q centers

MTB_Q<- read.csv("raw-data/German_grid_central_coordinates.csv", header=T)

head(MTB_Q$MTB_Q)
MTB_Q$MTB_Q[1]<- paste0("0",MTB_Q$MTB_Q[1])
MTB_Q$MTB_Q[2]<- paste0("0",MTB_Q$MTB_Q[2])
MTB_Q$MTB_Q[3]<- paste0("0",MTB_Q$MTB_Q[3])
MTB_Q$MTB_Q[4]<- paste0("0",MTB_Q$MTB_Q[4])


# Read the data from a local copy of the raw records
dght_dates<- st_read("raw-data/all_specs_all_plots.shp")

dght_dates$Group<-NA
dght_dates$Group[dght_dates$Species %in% c("Salamandra atra","Ichthyosaura alpestris","Lissotriton helveticus","Salamandra salamandra","Triturus cristatus","Lissotriton vulgaris")]<-c("newts")
dght_dates$Group[!(dght_dates$Species %in% c("Salamandra atra","Ichthyosaura alpestris","Lissotriton helveticus","Salamandra salamandra","Triturus cristatus","Lissotriton vulgaris"))]<-c("frogs")

dght_dates$lon<-NA
dght_dates$lat<-NA

for (i in 1:nrow(MTB_Q)){
  ind1<- which(dght_dates$MTB_Q==MTB_Q$MTB_Q[i])
  dght_dates$lon[ind1]<-MTB_Q$lon[i]
  dght_dates$lat[ind1]<-MTB_Q$lat[i]
}

dght_dates<- as.data.frame(dght_dates)
dght_dates<- dght_dates[!names(dght_dates)%in%c("geometry"),]

table(is.na(dght_dates$lon))

dght_dates<-dght_dates[!is.na(dght_dates$lon),]

dght_dates<- st_as_sf(dght_dates, coords=c("lon","lat"), crs="+proj=longlat +datum=WGS84 +no_defs")

dght_coords<- do.call(rbind,st_geometry(dght_dates))

dght_dates$lon<-dght_coords[,1]
dght_dates$lat<-dght_coords[,2]


################## We have additional information fro the state of saxony 
### read in information about Amphibians given by the state of Saxony
sachsen_amphib<- st_read("raw-data/Sachsen_Auszug ZenA Amphibien.shp")
sachsen_amphib_WGS84<- st_transform(sachsen_amphib, crs="+proj=longlat +datum=WGS84 +no_defs")
rm(sachsen_amphib)

## There are some entries in the species data where we have no clear species info
## e.g. "Molch indet."

sachsen_amphib_WGS84<-sachsen_amphib_WGS84[!sachsen_amphib_WGS84$Art_wiss %in% c("Pelophylax indet.","Molch indet.",
                                                                                 "Braunfrosch indet.","Amphibia","Bufo"),]

names(sachsen_amphib_WGS84)[9]<-c("Species")


## Get rid of those without date precision
sachsen_amphib_WGS84<- subset(sachsen_amphib_WGS84,!is.na(sachsen_amphib_WGS84$Datum))

## Add MTB_Q info
sachsen_amphib_WGS84$MTB_Q<- paste0(sachsen_amphib_WGS84$MTB,sachsen_amphib_WGS84$MTBQ)

## Add group info
sachsen_amphib_WGS84$Group<- NA
sachsen_amphib_WGS84$Group[sachsen_amphib_WGS84$Art_wiss %in% c("Salamandra atra","Ichthyosaura alpestris","Lissotriton helveticus","Salamandra salamandra","Triturus cristatus","Lissotriton vulgaris")]<-c("newts")
sachsen_amphib_WGS84$Group[!(sachsen_amphib_WGS84$Art_wiss %in% c("Salamandra atra","Ichthyosaura alpestris","Lissotriton helveticus","Salamandra salamandra","Triturus cristatus","Lissotriton vulgaris"))]<-c("frogs")



names(sachsen_amphib_WGS84)

#### Add federal state Information to the Amphibian Datasets

dght_dates<- st_intersection(dght_dates,fed_states_WGS84[c("Name","geometry")])
sachsen_amphib_WGS84<- st_intersection(sachsen_amphib_WGS84,fed_states_WGS84[c("Name","geometry")])
sachsen_amphib_WGS84<- subset(sachsen_amphib_WGS84,Name %in% c("Sachsen"))

pts_amphib_sachsen<- do.call(rbind, st_geometry(sachsen_amphib_WGS84))


sachsen_amphib_WGS84$lon<-NA 
sachsen_amphib_WGS84$lat<-NA

for (i in 1:nrow(MTB_Q)){
  ind1<- which(sachsen_amphib_WGS84$MTB_Q==MTB_Q$MTB_Q[i])
  sachsen_amphib_WGS84$lon[ind1]<-MTB_Q$lon[i]
  sachsen_amphib_WGS84$lat[ind1]<-MTB_Q$lat[i]
}

sachsen_amphib_WGS84_1<- as.data.frame(sachsen_amphib_WGS84)
sachsen_amphib_WGS84_1<- sachsen_amphib_WGS84_1[!names(sachsen_amphib_WGS84_1) %in% c("geometry")]

sachsen_amphib_WGS84_1<- st_as_sf(sachsen_amphib_WGS84_1, coords=c("lon","lat"), crs="+proj=longlat +datum=WGS84 +no_defs")

pts_amphib_sachsen<- do.call(rbind, st_geometry(sachsen_amphib_WGS84_1))

sachsen_amphib_WGS84_1$lon<- pts_amphib_sachsen[,1]
sachsen_amphib_WGS84_1$lat<- pts_amphib_sachsen[,2]


### Commented out to save memory and time
#plot(sachsen_amphib_WGS84_1[1])

#Sachsen_amph_occs<- dataDiagnostics(taxa=as.character(sachsen_amphib_WGS84$Art_wiss),
#                                    site=as.character(sachsen_amphib_WGS84$MTB_Q),
#                                    time_period=as.Date(sachsen_amphib_WGS84$Datum, format="%d.%m.%Y")
#                                    )


##########################################################################################
##########################################################################################
##########################################################################################


#### Combine dght info and sachsen info
names(dght_dates)
table(is.na(dght_dates$BEGINN))
dght_dates_1<- dght_dates[c("Species","MTB_Q","BEGINN","ENDE","Name","lon","lat","Group","start_year","end_year","geometry")]
names(dght_dates_1)<- c("Species","MTB_Q","BEGINN","ENDE","Bundesland","lon","lat","Group","start_year","end_year","geometry")


amphib_sachsen_1<- sachsen_amphib_WGS84_1[c("Species","MTB_Q","Datum","Name","Group","lon","lat","geometry")]
amphib_sachsen_1$Datum<- as.Date(amphib_sachsen_1$Datum, format="%d.%m.%Y")
amphib_sachsen_1$start_year<-lubridate:: year(amphib_sachsen_1$Datum)
amphib_sachsen_1$end_year<- lubridate:: year(amphib_sachsen_1$Datum)
amphib_sachsen_1$BEGINN<- amphib_sachsen_1$Datum


names(amphib_sachsen_1)[4]<- c("Bundesland")
names(amphib_sachsen_1)[3]<- c("ENDE")




### There are some entries wtihout BEGINN or ENDE Date...
table(is.na(amphib_sachsen_1$BEGINN))

table(is.na(dght_dates$ENDE))
table(is.na(dght_dates_1$ENDE))
table(is.na(dght_dates_1$BEGINN))
View(dght_dates_1[is.na(dght_dates_1$ENDE),])

dght_dates_2<-dght_dates_1[!is.na(dght_dates_1$BEGINN) & !is.na(dght_dates_1$ENDE),]
table(is.na(dght_dates_2$BEGINN))
table(is.na(dght_dates_2$ENDE))



names(dght_dates_2) %in% names(amphib_sachsen_1)

dght_all<- rbind(dght_dates_2,amphib_sachsen_1)


## There are some inconsistencies in the taxonomy
unique(dght_all$Species)
dght_all$Species[dght_all$Species=="Bufo calamita"]<- c("Epidalea calamita")
dght_all$Species[dght_all$Species=="Bufotes viridis"]<- c("Bufo viridis")
table(dght_all$Species=="Triturus carnifex")

dght_all<-dght_all[(dght_all$Species!="Triturus carnifex"),]

dght_all$Species<-as.character(dght_all$Species)

table(is.na(dght_all$start_year))
table(is.na(dght_all$end_year))

names(dght_all)
dght_all<- arrange(dght_all,Species,ENDE, Bundesland)

length(unique(dght_all$Species))

#plot(dght_all[1])


#dataDiagnostics(taxa=dght_all$Species,
#                site=dght_all$MTB_Q,
#                time_period = dght_all$ENDE)

#hist(dght_all$end_year)

# reduce the data to the subset from 1980 to 2014

dght_1980_2014<- dght_all[dght_all$end_year %in% 1980:2014,]

dght_1980_2014$Group[dght_1980_2014$Species %in% c("Salamandra atra","Ichthyosaura alpestris","Lissotriton helveticus","Salamandra salamandra","Triturus cristatus","Lissotriton vulgaris")]<-c("newts")
dght_1980_2014$Group[!(dght_1980_2014$Species %in% c("Salamandra atra","Ichthyosaura alpestris","Lissotriton helveticus","Salamandra salamandra","Triturus cristatus","Lissotriton vulgaris"))]<-c("frogs")

#plot(dght_1980_2014[1])

#dataDiagnostics(taxa = dght_1980_2014$Species,
#                site = dght_1980_2014$MTB_Q,
#                time_period = dght_1980_2014$ENDE)



####
## filter for data with only years

amph_year_precision<- dght_1980_2014[(dght_1980_2014$start_year==dght_1980_2014$end_year),]

names(amph_year_precision)
head(amph_year_precision$ENDE)

#plot(amph_year_precision[9], pch=22, add=T, col="black")

amph_year_precision$day_st<- lubridate:: day(amph_year_precision$ENDE)
amph_year_precision$month_st<- lubridate:: month(amph_year_precision$ENDE)


#dataDiagnostics(taxa=amph_year_precision$Species,
#                site=amph_year_precision$MTB_Q,
#                time_period = amph_year_precision$end_year)

## Only data with day information
## rationale: entries with date on the first of January are probably entries with only year precision

amph_date_precision<- amph_year_precision[!((amph_year_precision$day_st==1) & (amph_year_precision$month_st==1)),]


#plots (NJBI - I've commented these out in order to save time and memory)
#plot(dght_dates[3]) 
#plot(amph_date_precision [3], col="black",add=T)
##plot(amph_year_precision[3],add=T, col="red")
#library(plyr)

# visits_per_year_group<-ddply(amph_date_precision,.(start_year,MTB_Q,Group),summarise,nu=length(unique(ENDE)))

phenology_per_month_group<-ddply(amph_date_precision,.(month_st,MTB_Q,Group),summarise,nu=length(unique(ENDE)))

phenology_per_month_group$month_st<- lubridate:: month(phenology_per_month_group$month_st, label=T)


#p<- ggplot(phenology_per_month_group, aes(x=month_st, y=nu)) + geom_bar(stat="identity") 
#p <- p + facet_wrap( ~ Group) 
#p

## Why is there an increasing number in December?
View(amph_date_precision[amph_date_precision$month_st==12 | amph_date_precision$month_st==11,])
# How many entries? are these outliers=
length(amph_date_precision$Species[amph_date_precision$month_st==12 | amph_date_precision$month_st==11])
## Around 10000 entries; probably no outlier

## Is it for certain species?
unique(amph_date_precision$Species[amph_date_precision$month_st==12 | amph_date_precision$month_st==11])
# no

## Is it for certain years?
unique(amph_date_precision$end_year[amph_date_precision$month_st==12 | amph_date_precision$month_st==11])
# No, many years

## 2 Possible explanations:
## a) The entries come from projetcs that look at populations specifically in winter
## b) The given date might be the date of entry into the database
## I think b) is more probable

## How many have been visited more than once; mor than one sampling dates in any year?
# table(temp$nu>=2)
# more than 5000
# how are those entries distributed?
# amph_more_than_1_visit<- amph_date_precision[amph_date_precision$MTB_Q %in% temp$MTB_Q[temp$nu>=2],]



# run the sparta diagnostic to look at list length over time
#dataDiagnostics(taxa = amph_date_precision$Species,
#                site = amph_date_precision$MTB_Q,
#                time_period = amph_date_precision$ENDE)



##### The data up to now contains all data from all states.
## Now we only take those federal states that we have 
## Reduce the dataset to the states we are allowed to analyze with date precision
## i.e. Nordrhein-Westfale, Sachsen und Sachsen-Anhalt

amph_prec_date_final<- subset(amph_date_precision, Bundesland %in% c("Nordrhein-Westfalen","Sachsen", "Sachsen-Anhalt"))


#dataDiagnostics(taxa=amph_prec_date_final$Species,
#                site=amph_prec_date_final$MTB_Q,
#                time_period = amph_prec_date_final$ENDE)


### for a nicely colored map:
## add defined colors for species
colors<- topo.colors(20)
species<- c(levels(dght_dates$Species))
spec_cols<- cbind(species,colors)
names(spec_cols)<-c("Species","color")
#plot(c(1:20),c(1:20), col=spec_cols[,2], pch=16)


dght_1980_2014$color<-NA

for (i in 1:nrow(spec_cols)){
  ind1<- which(dght_1980_2014$Species==spec_cols[i,1])
  dght_1980_2014$color[ind1]<-spec_cols[i,2]
}


amph_year_precision$color<-NA

for (i in 1:nrow(spec_cols)){
  ind1<- which(amph_year_precision$Species==spec_cols[i,1])
  amph_year_precision$color[ind1]<-spec_cols[i,2]
}

amph_date_precision$color<-NA

for (i in 1:nrow(spec_cols)){
  ind1<- which(amph_date_precision$Species==spec_cols[i,1])
  amph_date_precision$color[ind1]<-spec_cols[i,2]
}

amph_prec_date_final$color<-NA

for (i in 1:nrow(spec_cols)){
  ind1<- which(amph_prec_date_final$Species==spec_cols[i,1])
  amph_prec_date_final$color[ind1]<-spec_cols[i,2]
}



#### Show distribution of points with the different time resolutions
#par(mfrow=c(2,2), mar=c(2,2,2,2))
#plot(fed_states_WGS84[1], col=NA, main =c("All occurence records"))
#points(dght_1980_2014$lon,dght_1980_2014$lat, pch=1, col=dght_1980_2014$color,  cex=0.4)#

#plot(fed_states_WGS84[1], col=NA, main =c("Occurrence records \n with year precision"))
#points(amph_year_precision$lon,amph_year_precision$lat, pch=1, col=amph_year_precision$color,cex=0.4)

#plot(fed_states_WGS84[1], col=NA, main =c("Occurrence records \n with date precision"))
#points(amph_date_precision$lon,amph_date_precision$lat, pch=1, col=amph_date_precision$color, cex=0.4)

#plot(fed_states_WGS84[1], col=NA, main =c("Occurrence records \n for analysis"))
#points(amph_prec_date_final$lon,amph_prec_date_final$lat, pch=1, col=amph_prec_date_final$color, cex=0.4)




##### Here I create the regional_codes table to include Federal states as a random 
## factor in the sparta models


#### Create a matrix for all datasets with "Bundesland" as 0/1 for every MTB_Q
### This is necessary for the regional_code argument in the occDetModel function we will use later
## This introduces a random factor "Bundesland" for each MTB_Q 


require(data.table)
MTB_Q_dght_1980_2014<-as.data.frame(dght_1980_2014[c("MTB_Q","Bundesland")] )
MTB_Q_dght_1980_2014<- MTB_Q_dght_1980_2014[!names(MTB_Q_dght_1980_2014) %in% c("geometry")]
MTB_Q_Bl_dght1980_2013<-as.data.frame(dcast(setDT(MTB_Q_dght_1980_2014), MTB_Q ~ Bundesland))

for(i in 2:ncol(MTB_Q_Bl_dght1980_2013)){
  MTB_Q_Bl_dght1980_2013[,i]<- as.numeric(MTB_Q_Bl_dght1980_2013[,i])
}
for (i in 2:ncol(MTB_Q_Bl_dght1980_2013)){
  ind1<- which(MTB_Q_Bl_dght1980_2013[,i]>0)
 MTB_Q_Bl_dght1980_2013[ind1,i]<-1
}


MTB_Q_amph_year_precision<-as.data.frame(amph_year_precision[c("MTB_Q","Bundesland")] )
MTB_Q_amph_year_precision<- MTB_Q_amph_year_precision[!names(MTB_Q_amph_year_precision) %in% c("geometry")]
MTB_Q_amph_year_precision<-as.data.frame(dcast(setDT(MTB_Q_amph_year_precision), MTB_Q ~ Bundesland))

for(i in 2:ncol(MTB_Q_amph_year_precision)){
  MTB_Q_amph_year_precision[,i]<- as.numeric(MTB_Q_amph_year_precision[,i])
}

for (i in 2:ncol(MTB_Q_amph_year_precision)){
  ind1<- which(MTB_Q_amph_year_precision[,i]>0)
  MTB_Q_amph_year_precision[ind1,i]<-1
}



MTB_Q_amph_date_precision<-as.data.frame(amph_date_precision[c("MTB_Q","Bundesland")] )
MTB_Q_amph_date_precision<- MTB_Q_amph_date_precision[!names(MTB_Q_amph_date_precision) %in% c("geometry")]
MTB_Q_amph_date_precision<-as.data.frame(dcast(setDT(MTB_Q_amph_date_precision), MTB_Q ~ Bundesland))

for(i in 2:ncol(MTB_Q_amph_date_precision)){
  MTB_Q_amph_date_precision[,i]<- as.numeric(MTB_Q_amph_date_precision[,i])
}

for (i in 2:ncol(MTB_Q_amph_date_precision)){
  ind1<- which(MTB_Q_amph_date_precision[,i]>0)
  MTB_Q_amph_date_precision[ind1,i]<-1
}

MTB_Q_amph_prec_date_final<-as.data.frame(amph_prec_date_final[c("MTB_Q","Bundesland")] )
MTB_Q_amph_prec_date_final<- MTB_Q_amph_prec_date_final[!names(MTB_Q_amph_prec_date_final) %in% c("geometry")]
MTB_Q_amph_prec_date_final<-as.data.frame(dcast(setDT(MTB_Q_amph_prec_date_final), MTB_Q ~ Bundesland))

for(i in 2:ncol(MTB_Q_amph_prec_date_final)){
  MTB_Q_amph_prec_date_final[,i]<- as.numeric(MTB_Q_amph_prec_date_final[,i])
}

for (i in 2:ncol(MTB_Q_amph_prec_date_final)){
  ind1<- which(MTB_Q_amph_prec_date_final[,i]>0)
  MTB_Q_amph_prec_date_final[ind1,i]<-1
}




# now save copies of the frogs and newts separately
# put copies in a folder called derived-data (create if it doesn't already exist)
if(!dir.exists('derived-data')) dir.create('derived-data')

## For all date structures 

anura_dght_1980_2014 <- subset(dght_1980_2014, Group == 'frogs')
save(anura_dght_1980_2014, file='derived-data/anura_dght_1980_2014.rData')

newts_dght_1980_2014 <- subset(dght_1980_2014, Group == 'newts')
save(newts_dght_1980_2014, file='derived-data/newts_dght_1980_2014.rData')

save(MTB_Q_Bl_dght1980_2013, file='derived-data/regional_codes_dght_1980_2014.rData')


#### For data with year precision

anura_year <- subset(amph_year_precision, Group == 'frogs')
save(anura_year, file='derived-data/anura_year_precision_1980_2014.rData')

newts_year <- subset(amph_year_precision, Group == 'newts')
save(newts_year, file='derived-data/newts_year_precision_1980_2014.rData')

save(MTB_Q_amph_year_precision, file='derived-data/regional_codes_year_prec_apmh.rData')

#### For data with date precision

anura_date <- subset(amph_date_precision, Group == 'frogs')
save(anura_date, file='derived-data/anura_date_precision_1980_2014.rData')

newts_date <- subset(amph_date_precision, Group == 'newts')
save(newts_date, file='derived-data/newts_date_precision_1980_2014.rData')

save(MTB_Q_amph_date_precision, file='derived-data/regional_codes_date_prec_apmh.rData')

#### For the data with date precision from the federal states that we analyse

anura_analysis <- subset(amph_prec_date_final, Group == 'frogs')
save(anura_analysis, file='derived-data/anura_date_precision_analysis_1980_2014.rData')

newts_analysis <- subset(amph_prec_date_final, Group == 'newts')
save(newts_analysis, file='derived-data/newts_date_precision_analysis_1980_2014.rData')

save(MTB_Q_amph_prec_date_final, file='derived-data/regional_codes_date_prec_apmh_analysis.rData')
