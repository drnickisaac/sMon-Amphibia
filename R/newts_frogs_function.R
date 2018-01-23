## Function to subset news and frogs
## the function takes the dataframe entered via the argument "data"
## and adds a new colum called "Group" that assorts the amphibian species given in the column specified
## by "taxon" to two groups, either "newts" or "frogs"
newt_frog<- function (data=data, taxon=taxon){
  data$Group[data$taxon %in% c("Salamandra atra","Ichthyosaura alpestris","Lissotriton helveticus","Salamandra salamandra","Triturus cristatus","Lissotriton vulgaris")]<-c("newts")
  data$Group[!(data$taxon %in% c("Salamandra atra","Ichthyosaura alpestris","Lissotriton helveticus","Salamandra salamandra","Triturus cristatus","Lissotriton vulgaris"))]<-c("frogs")
  
}
