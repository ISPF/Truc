#docShiny: https://shiny.rstudio.com/
#shinyWidgets: https://shiny.rstudio.com/gallery/widget-gallery.html


ui <- fluidPage(
  titlePanel("Moral Coral Reef - Long Term Ecological Research"),  # Application title
  br(), #HTML: <br>
  sidebarLayout(
    
    # sidebarPanel ------------------------------------------------------------
    sidebarPanel(
      radioButtons(inputId = "selDataset",
                  label="Sélectionnez le jeu de données à visualiser",
                  inline=TRUE,
                  choiceNames = c("Substrat","Poissons"),
                  choiceValues = c("coral","fish")
                  ),
      # tags$hr(),
      # conditionalPanel(condition = "input.tabs!='Table'",
      #                  conditionalPanel(condition = "input.tabs=='Line Chart'",
      #                                   fluidRow(
      #                                     column(6,checkboxInput(inputId = "habitatDetails",label = "Partitionner les données selon habitats",value = FALSE)),
      #                                     column(6,htmlOutput("showSelHabitat"))
      #                                     ),#end fluidRow
      #                                   ),#end conditionalPanel'== 'LineChart
      #                  br(),
      #                  htmlOutput("showSelFamily"),
      #                  # conditionalPanel(condition="input.tabs=='Map'",
      #                  #                  sliderInput(inputId = "selSize",label="Ajustez taille relative des points",value = 1,min = 0.2,max=5,step=0.1)
      #                  #                  )# end conditionalPanel == 'Map'
      #                   )#end conditionalPanel !='Table'
       
    ),#end sidebarPanel
    
    
    # mainPanel ---------------------------------------------------------------
    mainPanel(
      #htmlOutput("selDatasetMessage"),
      tabsetPanel(id="tabs",
        tabPanel("Table",
                 br(), 
                 # downloadButton("downloadData"),
                 # br(),br(),
                 DT::dataTableOutput("lterDataTable")
                 ),#end tab table
        tabPanel("Line Chart",
                 br(), 
                 strong("Ici on va mettre un line chart pour visualiser les données !"),
                 # br(),br(),
                 # plotOutput("lineChartPlot")
                 ),#end tab lineChart
        tabPanel("Map",
                 br(), 
                 em("Ici on va mettre une carte leaflet pour visualiser les données !"),
                 # br(),
                 # htmlOutput("showSelYear"),
                 # br(),
                 # leafletOutput("map")
                 )#end tab map
      )#end tabsetPanel
    )#end mainPanel
    
    
    
  )#end sidebarLayout
)#end fluidPage