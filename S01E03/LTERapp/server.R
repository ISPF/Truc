library(shiny)
library(data.table)
library(DT)
library(sf)
library(leaflet)
library(plotly)
library(tidyverse)

source("scripts/filter_plot_data.R")
source("scripts/filter_map_data.R")

#Data : http://mcr.lternet.edu/data
fishData<-fread("data/fishLTER_red.csv",sep=";",dec=",")
coralData<-fread("data/coralsLTER_red.csv",sep=";",dec=",")
siteCoords<-st_read("data/siteCoords.kml")
names(siteCoords)[1]<-"siteID"

server <- function(input, output,session){
# reactive Objects --------------------------------------------------------
  lterData<-reactive({
    if(input$selDataset=="fish")
      return(fishData)
    if(input$selDataset=="coral")
      return(coralData)
  })
  
  lineChart<-reactive({
    if(is.null(input$selFamily) || is.na(input$selFamily[1]))
      return(NULL)

    lterDataPlot<-filterPlotData(mData = lterData(),
                                 dataset = input$selDataset,
                                 habitatDetails = input$habitatDetails,
                                 habitat = input$selHabitat,
                                 family=input$selFamily)
   
    p<-plot_ly(lterDataPlot,x=~Year,y=~value)
    if(ncol(lterDataPlot)==2){
      p<-add_trace(p,type="scatter",mode="markers+lines")
    }else{
      plotGroup<-names(lterDataPlot)[2]
      p<-add_trace(p,type="scatter",mode="markers+lines",color=~get(plotGroup))

    }
    return(p)
  })
  
  leafMapData<-reactive({
    if(is.null(input$selYear))
      return(NULL)
    if(is.null(input$selFamily) || (is.na(input$selFamily[1]) || "Toutes"%in%input$selFamily))
      return(NULL)

    lterDataMap<-filterMapData(mData = lterData(),
                               dataset = input$selDataset,
                               siteCoords = siteCoords,
                               family = input$selFamily,
                               year = input$selYear)

    return(lterDataMap)
  })

# sideBarPanel outputs ----------------------------------------------------
  output$showSelHabitat<-renderUI({
    if(is.null(lterData()))
      return(NULL)
    if(!input$habitatDetails)
      return(NULL)
    habUnq<-unique(sort(lterData()$Habitat))
    return(checkboxGroupInput(inputId = "selHabitat",
                       label = "Choissisez le ou les habitat(s) d'intérêt",
                       choices = habUnq))
  })

  output$showSelFamily<-renderUI({
    if(is.null(lterData()))
      return(NULL)

    lsChoices<-unique(sort(lterData()$Family))
    if(input$selDataset=="fish") lsChoices<-c("Cumul",lsChoices)

    if(input$habitatDetails & length(input$selHabitat)>1){
      multChoice<-FALSE
    }else{
      multChoice<-TRUE
      lsChoices<-c("Toutes",lsChoices)
    }

    return(selectInput(inputId = "selFamily",
                       label = "Choissisez les familles taxonomiques d'intérêt",
                       selected=1,
                       choices=lsChoices,
                       multiple = multChoice))
  })


# Main Panel outputs ------------------------------------------------------
  output$selDatasetMessage<-renderUI({
    yearSt<-ifelse(input$selDataset=="coral","2005","2006")
    return(HTML(paste0("<h2>",yearSt,"-2019 ",input$selDataset, " counts</h2>")))
  })

# tab Table outputs -------------------------------------------------------
  output$downloadData<-downloadHandler(
    filename = function(){paste0(input$selDataset,"_LTER.csv")},
    content=function(file){
      fwrite(lterData(),file,sep=";",dec=",",row.names=F)
    }
  )
  
  output$lterDataTable <- DT::renderDataTable({
    return(lterData())
  },options=list(pageLength=25))
  

# tab LineChart outputs ---------------------------------------------------
  output$lineChartPlot<-renderPlotly({
    return(lineChart())
  })

# tab Map outputs ---------------------------------------------------------
  output$showSelYear<-renderUI({
    minYear<-ifelse(input$selDataset=="fish",2006,2005)
    return(sliderInput(inputId = "selYear",label = "Choisissez l'année d'intérêt",min = minYear,max=2019,value = 2005,step=1,width='80%'))
  })

  observe({
    if(!is.null(leafMapData())){
      leafletProxy("map",data=leafMapData())%>%
        clearMarkers()%>%
        addCircleMarkers(radius=~value*input$selSize,color="red",stroke=TRUE,fillOpacity=0.8,)
    }
  })

  output$map<-renderLeaflet({
    m<-leaflet(siteCoords) %>%
      addProviderTiles("Esri.WorldImagery")%>%
      addCircleMarkers(radius=3,color="orange",stroke=TRUE,fillOpacity=0.8)
    return(m)
  })
}
