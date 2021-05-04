source("src/functions.R", encoding = "UTF-8")

#Collectes <- getSQLTableWinLinux("EnqueteEmploi", "dbo", "Collecte")
#fwrite(Collectes, "input/Collectes.csv")
Collectes <- fread("input/Collectes.csv")

Collectes <- Collectes[Reporting >= 1]

Suivi <- data.table(NumFA = "")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tagList(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="css/style.css")
    )
  ),
  
  dashboardPage(
    
    dashboardHeader(
      title = "Enquête Emploi"
    ),
    
    dashboardSidebar(
      fluidRow(
        align="center",
        "Données actualisées le :",
        textOutput("dateCourante")
      ),
      fluidRow(
        align="center",
        selectInput("sel_collecte",
                    "Collecte",
                    width = "200px", 
                    selected = Collectes[Reporting == 2, Libelle],
                    choices = c(Collectes[,.(Libelle)])
        )
      ),
      fluidRow(
        align="center",
        uiOutput("sel_gestionnaires") 
      ),
      fluidRow(
        align="center",
        selectInput("sel_dataset", "Télécharger données CSV",
                    width = "200px", 
                    choices = c("Avancement", "Paie", "Controles", "Carnets")
        ),
        downloadButton(
          "downloadData", 
          "Télécharger"
        )
      )
    ),
    
    dashboardBody(
      fluidRow(
        box(
          collapsible = TRUE, 
          title = "Indicateurs généraux",
          solidHeader = TRUE,
          width = 12,
          status = "primary",
          infoBoxOutput("tauxCollecte"),
          infoBoxOutput("tauxReussite"),
          infoBoxOutput("statutCollecte")
        )
      ),
      
      fluidRow(
        tabBox(
          id = 'dataset',
          width = 12,
          tabPanel("Avancement", dataTableOutput("avancement")),
          tabPanel("Paie", dataTableOutput("Paie")),
          tabPanel("Controles", dataTableOutput("controles")),
          tabPanel("Carnets de tournée", dataTableOutput("carnets")),
          tabPanel("FA Non-autorisées", 
                   box(   
                     title = "Déclarer un remplacement de FA (réserve, inversion, ...)",
                     solidHeader = TRUE,
                     status = "primary",
                     uiOutput("msgRemplacement"),
                     htmlOutput("listeNumFA"),
                     actionButton("valid_remplacement", tags$b("Valider"))
                   ),
                   dataTableOutput("FANonAutorisees")
          ),
          tabPanel("FA expirées", dataTableOutput("FAExpirees")),
          tabPanel("Derniers transferts", 
                   tags$h4("Dernier export de l'hyperviseur :"), 
                   textOutput("transfertsHyp"),
                   tags$h4("Dates de dernière récupération de fichier par les gestionnaires : "),
                   dataTableOutput("transfertsGes"),
                   tags$h4("Dates de dernier envoi de fichier par les enquêteurs :"),
                   dataTableOutput("transfertsEnq")
          ),
          tabPanel("Graphiques", plotlyOutput("grapheSuivi"))
        )
      )
    )
  )
)

# Wrap your UI with secure_app
# ui <- secure_app(ui)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # observeEvent(input$sel_gestionnaires, {
  #   
  #   # récupération en base de données et rendu des informations de suivi
  #   Suivi <- getSQLTableWinLinux("EnqueteEmploi", collecte, "Suivi")
  #   output$avancement <- renderDataTable({ df_suivi[StatutPeriode!="Grappe à venir"] }, options = list(scrollX = TRUE), filter = 'top')
  #   
  #   if (input$sel_gestionnaires != "Tous les gestionnaires") { 
  #     Suivi <- Suivi[NomGestionnaire==input$sel_gestionnaires]
  #   }
  # })
  
  observeEvent(input$sel_collecte, {
    
    collecte <- input$sel_collecte
    
#    liste_gestionnaires()
    
    # calcul et rendu du libellé de collecte
#    output$infoRemplacement <- renderUI({
#       
#       tags$div(class = "msgInfo",
#                "Pour déclarer un remplacement de FA (réserve, inversion, ...), merci de renseigner le fichier ",
#                tags$a(target="_blank", href = paste("file://J:/filserver/Projets", sep=""), # Enquête sur l'emploi\\A - Echange fichiers\\", collecte, "\\Hyperviseur\\DataModelEclate\\Correction\\Remplacements.txt
#                paste("\"J:\\Projets\\Enquête sur l'emploi\\A - Echange fichiers\\", collecte, "\\Hyperviseur\\DataModelEclate\\Correction\\Remplacements.txt\"", sep=""),
#                ),
#                tags$div(class = "msgInfo",
#                         "Ces déclarations sont ensuite chargées en base de données toutes les heures."
#                )
#       )
#     })

    output$listeNumFA <- renderUI({
     list(
       column(4, align="center", selectInput("NumFARemplacee", "FA remplacée", c("Numéro FA", Suivi$NumFA), width = "120px")), 
       column(4, align="center", selectInput("NumFARemplacante", "FA remplaçante", c("Numéro FA", Suivi$NumFA), width = "120px"))
     )
    })
    
    Suivi <- suivi(collecte)
    
    output$dateCourante <- renderText({ format(Sys.time(), "%d/%m/%Y à %H:%M:%S") }) # retourne la date courante au format texte pour affichage

    output$tauxCollecte   <- infoBoxTauxCollecte(Suivi)
    output$tauxReussite   <- infoBoxTauxReussite(Suivi)
    output$statutCollecte <- infoBoxStatutCollecte(Collectes, collecte)

    output$avancement     <- avancement(Suivi) 
    output$Paie           <- paie(Suivi)
    output$grapheSuivi    <- grapheSuivi(Suivi)
    
    output$controles      <- controles(collecte)
    output$carnets        <- carnets(collecte)
    output$FANonAutorisees <- FANonAutorisees(Suivi)
    output$FAExpirees     <- FAExpirees(Suivi)
    output$transfertsHyp  <- transfertsHyp(Suivi)
    output$transfertsGes  <- transfertsGes(Suivi)
    output$transfertsEnq  <- transfertsEnq(Suivi)
  })

  dataset <- reactive({ 
    cat(input$sel_dataset)
    switch(input$sel_dataset,
           "Avancement" = suivi(input$sel_collecte),
           "Paie" = dtPaie(suivi(input$sel_collecte)),
           "Controles" = dtCarnets(input$sel_collecte),
           "Carnets" = dtCarnets(input$sel_collecte)
    )
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste('EE-', input$sel_collecte, '-', input$sel_dataset, '-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(dataset(), con)
    })
  
  observeEvent(input$valid_remplacement, {
    insertRemplacement(input, output)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

