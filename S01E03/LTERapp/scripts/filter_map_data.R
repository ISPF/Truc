filterMapData<-function(mData,dataset,siteCoords,family,year){
  res<-mData%>%
    group_by(Year,siteID,Family)%>%
    summarize(.groups = "keep",value=sum(value))#passer au niveau hiérarchique Family (discard Taxonomy)
  
  if(dataset=="coral"){
    res$siteID<-substr(res$siteID,1,4)
    res<-summarize(.groups="keep",res,value=mean(value))
  }
  
  
  if(!"Cumul"%in%family) res<-filter(res,Family%in%family)
  res<-res%>%
    group_by(Year,siteID)%>%
    summarize(.groups="keep",value=sum(value))
  
  res<-filter(res,Year==year)
  
  res$value<-log(res$value)
  
  fRes<-left_join(siteCoords,res)
  return(fRes)
}
