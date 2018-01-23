rm(list=ls())
source('R/Exploratory-functions.R')

data1 <- read.csv('raw-data/Amphibia_DGHT_20_specs.csv')
data2 <- read.csv('raw-data/Amphibia_Reptilia_Herpetof_Onl.csv')

# can we append the datasets together?
all(names(data1) %in% names(data2)) # FALSE 

# ok, let's just apply across a list
data <- list(data1, data2)

sapply(data, names)
sapply(data, nrow) # 300 300
sapply(data, num_sites) # 267 122
sapply(data, num_spp) # 19 25

#visits
nrow(unique(data1[,c('MTB_Q', 'end_year')])) #291
nrow(unique(data2[,c('MTB_Q', 'Jahr')])) #167

