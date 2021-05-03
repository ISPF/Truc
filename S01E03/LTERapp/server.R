#Data : http://mcr.lternet.edu/data

library(shiny)
library(data.table)
library(DT)
library(tidyverse)
library(sf)
library(leaflet)

# source("scripts/filter_plot_data.R")
# source("scripts/filter_map_data.R")

fishData<-fread("data/fishLTER_red.csv",sep=";",dec=",")
coralData<-fread("data/coralsLTER_red.csv",sep=";",dec=",")
siteCoords<-st_read("data/siteCoords.kml")

server <- function(input, output,session){
# reactive Objects --------------------------------------------------------
  
  # lterData<-reactive({
  #   if(input$selDataset=="fish")
  #     return(fishData)
  #   if(input$selDataset=="coral")
  #     return(coralData)
  # })
  
  # lineChart<-reactive({
  #   if(is.null(lterData()))
  #     return(NULL)
  #   # if(is.null(input$habitatDetails))
  #   #   return(NULL)
  #   # if(is.null(input$selFamily))
  #   #   return(NULL)
  #   # if(is.na(input$selFamily[1]))
  #   #   return(NULL)
  # 
  #   lterDataPlot<-lterData()%>%
  #     group_by(Year,Site,Habitat,siteID,Family)%>%
  #     summarize(value=sum(value))
  # 
  #   lterDataPlot<-lterDataPlot%>%
  #     group_by(Year,Family)%>%
  #     summarize(value=mean(value))
  # 
  #   p<-ggplot(lterDataPlot,aes(x=Year,y=value,group=Family,color=Family))+
  #     geom_line(lwd=1)+
  #     geom_point(size=3)
  #   
  #   return(p)
  # 
  #   # lterDataPlot<-filterPlotData(mData = lterData(),
  #   #                              dataset = input$selDataset,
  #   #                              habitatDetails = input$habitatDetails,
  #   #                              habitat = input$selHabitat,
  #   #                              family=input$selFamily)
  #   # 
  #   # if(ncol(lterDataPlot)==2){
  #   #   p<-ggplot(lterDataPlot,aes(x=Year,y=value))
  #   # }else{
  #   #   plotGroup<-names(lterDataPlot)[2]
  #   #   p<-ggplot(lterDataPlot,aes(x=Year,y=value,group=get(plotGroup),color=get(plotGroup)))
  #   # }
  #   # 
  #   # p<-p+
  #   #   geom_line(lwd=1)+
  #   #   geom_point(size=3)
  #   # 
  #   # return(p)
  # })
  
  # leafMap<-reactive({
  #   if(is.null(lterData()))
  #     return(NULL)
  #   if(is.null(input$selFamily))
  #     return(NULL)
  #   if(is.na(input$selFamily[1]))
  #     return(NULL)
  #   if("Toutes"%in%input$selFamily)
  #     return(NULL)
  # 
  #   lterDataMap<-filterMapData(mData = lterData(),
  #                              dataset = input$selDataset,
  #                              siteCoords = siteCoords,
  #                              family = input$selFamily,
  #                              year = input$selYear)
  # 
  # 
  #   m<-leaflet(lterDataMap) %>%
  #     addProviderTiles("Esri.WorldImagery") %>%
  #     addCircleMarkers(radius=~value*input$selSize,color="red",stroke=TRUE,fillOpacity=0.8)
  #   return(m)
  # })
  
  # leafMapData<-reactive({
  #   if(is.null(lterData()))
  #     return(NULL)
  #   if(is.null(input$selFamily))
  #     return(NULL)
  #   if(is.na(input$selFamily[1]))
  #     return(NULL)
  #   if("Toutes"%in%input$selFamily)
  #     return(NULL)
  #   
  #   lterDataMap<-filterMapData(mData = lterData(),
  #                              dataset = input$selDataset,
  #                              siteCoords = siteCoords,
  #                              family = input$selFamily,
  #                              year = input$selYear)
  #   
  #   return(lterDataMap)
  # })

# sideBarPanel outputs ----------------------------------------------------
  # output$showSelHabitat<-renderUI({
  #   if(is.null(lterData()))
  #     return(NULL)
  #   if(!input$habitatDetails)
  #     return(NULL)
  #   habUnq<-unique(sort(lterData()$Habitat))
  #   return(checkboxGroupInput(inputId = "selHabitat",
  #                      label = "Choissisez le ou les habitat(s) d'intérêt",
  #                      choices = habUnq))
  # })
  # 
  # output$showSelFamily<-renderUI({
  #   if(is.null(lterData()))
  #     return(NULL)
  # 
  #   lsChoices<-unique(sort(lterData()$Family))
  #   if(input$selDataset=="fish") lsChoices<-c("Cumul",lsChoices)
  # 
  #   if(!is.null(input$habitatDetails) & input$habitatDetails & length(input$selHabitat)>1){
  #     multChoice<-FALSE
  #   }else{
  #     multChoice<-TRUE
  #     lsChoices<-c("Toutes",lsChoices)
  #   }
  # 
  #   return(selectInput(inputId = "selFamily",
  #                      label = "Choissisez les familles taxonomiques d'intérêt",
  #                      selected=1,
  #                      choices=lsChoices,
  #                      multiple = multChoice))
  # })


# Main Panel outputs ------------------------------------------------------
  # output$selDatasetMessage<-renderUI({
  #   return(HTML(paste0("<h2>2005-2019 ",input$selDataset, " counts</h2>")))
  # })

# tab Table outputs -------------------------------------------------------
  # output$downloadData<-downloadHandler(
  #   filename = function(){paste0(input$selDataset,"_LTER.csv")},
  #   content=function(file){
  #     if(input$selDataset=="fish")
  #       tempData<-fishData
  #     if(input$selDataset=="coral")
  #       tempData<-coralData
  #     fwrite(tempData,file,sep=";",row.names = F)
  #     #fwrite(lterData(),file,sep=";",dec=",",row.names=F)
  #   }
  # )
  
  output$lterDataTable <- DT::renderDataTable({
    if(input$selDataset=="fish")
      return(fishData)
    if(input$selDataset=="coral")
      return(coralData)
    #return(lterData())
  },options=list(pageLength=25))
  

# tab LineChart outputs ---------------------------------------------------
  # output$lineChartPlot<-renderPlot({
  #   if(is.null(lineChart()))
  #     return(NULL)
  #   return(lineChart())
  # })

# tab Map outputs ---------------------------------------------------------
  # output$showSelYear<-renderUI({
  #   minYear<-ifelse(input$selDataset=="fish",2006,2005)
  #   return(sliderInput(inputId = "selYear",label = "Choisissez l'année d'intérêt",min = minYear,max=2019,value = 2005,step=1,width='80%'))
  # })

  # output$map<-renderLeaflet({
  #   if(is.null(leafMap()))
  #     return(NULL)
  #   return(leafMap())
  # })
  
  
  
  
  # observe({
  #   if(!is.null(leafMapData())){
  #     leafletProxy("map",data=leafMapData())%>%
  #       clearMarkers()%>%
  #       addCircleMarkers(radius=~value*input$selSize,color="red",stroke=TRUE,fillOpacity=0.8,)
  #   }
  # })
  
  # output$map<-renderLeaflet({
  #   m<-leaflet(siteCoords) %>%
  #     addProviderTiles("Esri.WorldImagery")%>%
  #     addCircleMarkers(radius=3,color="orange",stroke=TRUE,fillOpacity=0.8)
  #   return(m)
  # })
}
