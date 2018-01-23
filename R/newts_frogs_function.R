## Function to subset news and frogs
newt_frog<- function (data=data, taxon=taxon){
  data$Group[data$taxon %in% c("Salamandra atra","Ichthyosaura alpestris","Lissotriton helveticus","Salamandra salamandra","Triturus cristatus","Lissotriton vulgaris")]<-c("newts")
  data$Group[!(data$taxon %in% c("Salamandra atra","Ichthyosaura alpestris","Lissotriton helveticus","Salamandra salamandra","Triturus cristatus","Lissotriton vulgaris"))]<-c("frogs")
  
}
newt_frog(amph_more_than_1_visit, taxon=Species)
table(amph_more_than_1_visit$Group)
