filterPlotData<-function(mData,dataset,habitatDetails,habitat,family){
  
  res<-mData%>%
    group_by(Year,Site,Habitat,siteID,Family)%>%
    summarize(value=sum(value))#passer au niveau hiérarchique Family (discard Taxonomy)
  
  if(habitatDetails) res<-filter(res,Habitat%in%habitat)#filter data for selected habitats
  
  if(!TRUE%in%(c(NA,"Toutes","Cumul")%in%family)) res<-filter(res,Family%in%family)#filter data for selected Families
  
  lsG<-"Year"#lsG is the list of columns used to Group data
  
  if(habitatDetails & length(habitat)>1){
    lsG<-c(lsG,"Habitat")
  }else if(!"Cumul"%in%family){
    lsG<-c(lsG,"Family")
  }
      
  res<-group_by_at(res,lsG)
  
  if(dataset=="fish")
    res<-summarize(res,value=sum(value))#value is biomass can be added up
  else
    res<-summarize(res,value=mean(value))# value is percent cover better to average
  
  return(res)
}