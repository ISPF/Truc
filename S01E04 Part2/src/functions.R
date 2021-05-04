library(data.table)
library(DBI)
library(DT)
library(shiny)
library(shinydashboard)
library(shinymanager)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(viridis)
library(scales)


credentials <- data.frame(
  user = c("shiny"), # mandatory
  password = c("shiny"), # mandatory
  admin = c(FALSE),
  stringsAsFactors = FALSE
)

liste_gestionnaires <- function() {

  Gestionnaires <- getQueryFromSQLWinLinux(paste("select distinct Prénom + ' ' + Nom as Nom FROM dbo.Gestionnaire G INNER JOIN ", collecte, ".AffectationZone AZ on G.IdGest = AZ.IdGest", sep=""))
  output$sel_gestionnaires <- renderUI({
    selectInput("sel_gestionnaires", 
                "Gestionnaires", 
                c("Tous les gestionnaires", Gestionnaires$Nom), 
                width = "230px")
  })
}

# calcul et rendu du taux de collecte
infoBoxTauxCollecte <- function(Suivi) {
  
  # calcul des indicateurs de collecte
  nbFATotal <- nrow(Suivi)
  nbFAObligatoires <- nrow(Suivi[OBLIGATOIRE==1])
  nbFACollectees <- nrow(Suivi[OBLIGATOIRE==1 & StatutType!="Non-réalisé"])

  renderInfoBox({
    infoBox(
      "Taux de collecte", 
      ifelse(nbFAObligatoires == 0, 0, percent(nbFACollectees / nbFAObligatoires, accuracy = .1)),
      paste(nbFACollectees, " FA collectées sur ", nbFAObligatoires),
      icon = icon("tachometer-alt"),
      color = "blue",
      fill = TRUE)
  })
}

# calcul et rendu du taux de réussite
infoBoxTauxReussite <- function(Suivi) {
  
  nbFATotal <- nrow(Suivi)
  nbFACollectees <- nrow(Suivi[OBLIGATOIRE==1 & StatutType!="Non-réalisé"])
  nbFAReussies <- nrow(Suivi[StatutType=="Succès"])
  
  renderInfoBox({
    infoBox(
      "Taux de réussite", 
      ifelse(nbFACollectees == 0, 0, percent(nbFAReussies / nbFACollectees, accuracy = .1)),
      paste(nbFAReussies, " FA réussies sur ", nbFACollectees, sep=''),
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green",
      fill = TRUE
    )
  })
}

# calcul et rendu des dates et du statut de la collecte
infoBoxStatutCollecte <- function(Collectes, collecte) { 

  dateDeb <- strptime(Collectes[Libelle==collecte,DateDebut], "%Y-%m-%d")
  dateFin <- strptime(Collectes[Libelle==collecte,DateFin], "%Y-%m-%d")
  dateCur <- strptime(Sys.Date(), "%Y-%m-%d")
  
  if ( dateCur > dateFin ) {
    statutCollecte <- "Collecte terminée"
    grappeCollecte <- ""
  } else if ( dateCur < dateDeb ) {
    statutCollecte <- "Collecte à venir"
    grappeCollecte <- ""
  } else {
    nbjours <- as.numeric(difftime(dateCur, dateDeb, units = "days"))
    semCol  <- trunc(nbjours / 7 + 1)
    jourCol <- nbjours %% 7 + 1
    statutCollecte <- "Collecte en cours"
    grappeCollecte <- paste("Grappe", semCol, "- Jour", jourCol)
  }
  
  renderInfoBox({
    infoBox(
      statutCollecte, 
      grappeCollecte,
      paste("Du", format(dateDeb, "%d/%m/%Y"), "au", format(dateFin, "%d/%m/%Y")),
      icon = icon("calendar"),
      color = "aqua",
      fill = TRUE
    )
  })
}

# récupération en base de données des informations de suivi
suivi <- function(collecte) {
#  collecte <- "AUS21"
#  Suivi <- getSQLTableWinLinux("EnqueteEmploi", collecte, "Suivi")
#  fwrite(Suivi, paste0("input/",collecte, ".Suivi.csv"))
  Suivi <- fread(paste0("input/",collecte, ".Suivi.csv"))
  
}

# récupération en base de données des informations de suivi
dtControles <- function(collecte) {
  #collecte <- "SOC21"
  #Controles <- getSQLTableWinLinux("EnqueteEmploi", collecte, "Controles")
  #fwrite(Suivi, paste0("input/",collecte, ".Controles.csv"))
  
  #Controles <- getSQLTableWinLinux("EnqueteEmploi", collecte, "Controles")
  Controles <- fread(paste0("input/",collecte, ".Controles.csv"))
}

# récupération en base de données des informations de suivi
dtCarnets <- function(collecte) {
  #collecte <- "TMG20"
  #Carnets <- getSQLTableWinLinux("EnqueteEmploi", collecte, "CarnetsMenages")
  #fwrite(Suivi, paste0("input/",collecte, ".Carnets.csv"))
  
  #Carnets <- getSQLTableWinLinux("EnqueteEmploi", collecte, "CarnetsMenages")
  Carnets <- fread(paste0("input/",collecte, ".Carnets.csv"))
}

# récupération en base de données et rendu des informations de suivi
avancement <- function(Suivi) {
  dtAvancement <- datatable(Suivi[StatutPeriode!="Grappe à venir" & StatutCorrige != "FAC"], options = list(scrollX = TRUE), rownames=F) %>% 
          formatStyle(columns = "StatutCorrige", 
                      valueColumns = "StatutType", 
                      color = styleEqual(c("Succès","Echec enquêteur","Echec terrain","Non-réalisé"), 
                                         c("green", "blue","teal","red"))
          )
  renderDataTable(dtAvancement)
}

# calcul des éléments de paie en fonction des informations de suivi
dtPaie <- function(Suivi) {
  PaieDetail   <- Suivi[Remuneration > 0, .(PrenomEnqueteur, Remuneration)]
  PaieParMnt   <- dcast(PaieDetail, PrenomEnqueteur ~ Remuneration, fun = length)  
  PaieNbTotal  <- PaieDetail[, .("Nombre Total"=.N), PrenomEnqueteur]  
  PaieMntTotal <- PaieDetail[, .("Montant Total"=sum(Remuneration)), PrenomEnqueteur]  
  Paie <- PaieParMnt[PaieNbTotal[PaieMntTotal, on = .(PrenomEnqueteur=PrenomEnqueteur)], on = .(PrenomEnqueteur=PrenomEnqueteur)]
}

# rendu des éléments de paie
paie <- function(Suivi) {
  renderDataTable({ dtPaie(Suivi) }, options = list(scrollX = TRUE, pageLength = 25, dom = "t"), rownames=F)
}

# rendu des contrôles à réaliser
controles <- function(collecte) {
  renderDataTable({ dtControles(collecte) }, options = list(scrollX = TRUE), rownames=F)
}

# rendu des carnets de tournée
carnets <- function(collecte) {
  renderDataTable({ dtCarnets(collecte) }, options = list(scrollX = TRUE), rownames=F)
}

# récupération en base de données et rendu des FA non-autorisées
FANonAutorisees <- function(Suivi) {
  FANonAutorisees <- Suivi[OBLIGATOIRE==0 & Autorise==0 & StatutType!="Non-réalisé", .(NomGestionnaire, PrenomEnqueteur, Zone, NumSemaine, NumFA, Statut, DateInterview, FA_Origine_Declaree)]
  renderDataTable({ FANonAutorisees }, options = list(scrollX = TRUE, dom = "t"), rownames=F)
}

# récupération en base de données et rendu des FA expirées
FAExpirees <- function(Suivi) {
  FAExpirees <- Suivi[StatutCorrige=="EXP", .(NumFA, NomGestionnaire, PrenomEnqueteur, NumSemaine)]
  renderDataTable({ FAExpirees }, options=list(dom = "t"), rownames=F)
}

# récupération en base de données et rendu des derniers transferts gestionnaires
transfertsHyp <- function(Suivi) {
  transfertsHyp <- format(Suivi[Suivi[, .I[which.max(DateExportHyp)]]]$DateExportHyp, "%d/%m/%Y à %H:%M:%S")
  renderText({ transfertsHyp })
}

# récupération en base de données et rendu des derniers transferts gestionnaires
transfertsGes <- function(Suivi) {
  transfertsGes<-Suivi[, .(DateRecup=format(max(DateRecupGes, na.rm = T), "%Y/%m/%d à %H:%M:%S")), by=.(NomGestionnaire)]
  renderDataTable({ transfertsGes }, options=list(dom = "t"), rownames=F)
}

# récupération en base de données et rendu des derniers transferts enquêteurs
transfertsEnq <- function(Suivi) {
  transfertsEnq <- Suivi[, .(DateRecup=format(max(DateExportEnq, na.rm = T), "%Y/%m/%d à %H:%M:%S")), by=.(Zone, NomGestionnaire,PrenomEnqueteur)]
  renderDataTable({ transfertsEnq }, options=list(pageLength = 50, dom = "t"), rownames=F )
}

# réalisation du graphique de suivi par enquêteur, semaine et statut
grapheSuivi <- function(Suivi) {
  data <- Suivi[StatutPeriode!="Grappe à venir" & StatutCorrige != "FAC",
                .(NomGestionnaire, PrenomEnqueteur, NumSemaine, StatutCorrige)]
  data[, .N, by=.(StatutCorrige)][order(N, decreasing = T)]
  
  Graphe <- data[, .(Nb=.N), by=.(PrenomEnqueteur, StatutCorrige, NumSemaine)]
  Graphe[, StatutCorrige:=factor(StatutCorrige)]
  Graphe[, StatutCorrige:=forcats::fct_reorder(StatutCorrige, -Nb, min)]
  
  renderPlotly({
    ggplotly(ggplot(Graphe, aes(x=NumSemaine, y=Nb, fill=StatutCorrige), height=800)+
               geom_bar(stat="identity")+
               theme_ispf()+
               theme(panel.spacing.y = unit(1, "lines"), strip.text.x = element_text(size = 8))+
               scale_fill_manual(values=c(ispfPalette, viridis(10)))+
               facet_wrap(~PrenomEnqueteur, ncol = 4)+
               theme(legend.position = "right", legend.direction = "vertical")
             , height=150*ceiling(n_distinct(Graphe[,PrenomEnqueteur])/4)
    )
  })
}

insertRemplacement <- function(input, output) {

  if(input$NumFARemplacee=="Numéro FA" |  input$NumFARemplacante=="Numéro FA") {
    message <- "Les numéros FA remplacée et remplaçante doivent être renseignés !"
    style   <- "msgErreur"
  } else  if (input$NumFARemplacee == input$NumFARemplacante ) {
    message <- "Les numéros FA remplacée et remplaçante ne doivent pas être identiques !"
    style   <- "msgErreur"
  } else {
    con <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = sqlServer, Database = databaseName, encoding="ISO-8859-1")
    tableSQL <- getSQLTableWinLinux("EnqueteEmploi", input$sel_collecte, "Remplacement")
    if(nrow(tableSQL[OldNumFA == input$NumFARemplacee]) > 0) {
      message <- paste(renderText("La FA", input$NumFARemplacee, "a déjà été déclarée comme remplacée"))
      style   <- "msgErreur"
    } else if(nrow(tableSQL[NewNumFA == input$NumFARemplacante]) > 0) {
      message <- paste(renderText("La FA", input$NumFARemplacante, "a déjà été déclarée comme remplaçante"))
      style   <- "msgErreur"
    } else {
      remplacement <- data.table("OldNumFA" = input$NumFARemplacee, "NewNumFA" = input$NumFARemplacante) 
      dbAppendTable(con, Id(schema = input$collecte, table = "Remplacement"), remplacement, row.names = NULL)
      message <- "Ce remplacement a bien été intégré."
      style   <- "msgSucces"
    }
  }
  output$msgRemplacement <- renderUI({ tags$div(class = style, message) })
}

# Misc --------------------------------------------------------------------

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

# SQL Server --------------------------------------------------------------

getSQLTableWinLinux <- function(databaseName, schemaName, tableName){
  cat (sprintf("Extraction des donnees de la table %s\t: ", paste(schemaName, tableName, sep = ".")))
  if(get_os()=="windows")
    con <- dbConnect(odbc::odbc(),Driver = "SQL Server",Server = sqlServer,Database = databaseName, encoding="ISO-8859-1")
  else
    con <- dbConnect(odbc::odbc(),Driver = "ODBC Driver 17 for SQL Server", Server = sqlServer,Database = databaseName, UID = sqlLogin, PWD = sqlPassword)
  res <- tbl(con, in_schema(schemaName, tableName)) %>% collect()
  setDT(res)
  cat(sprintf("%s lignes extraites\n", nrow(res)))
  res
}

getQueryFromSQLFileWinLinux <- function(sqlFile, isWindows=T){
  #cat (sprintf("Requete %s\t: ", sqlFile))
  if(get_os()=="windows")
    con <- dbConnect(odbc::odbc(),Driver = "SQL Server",Server = sqlServer,Database = databaseName, encoding="ISO-8859-1")
  else
    con <- dbConnect(odbc::odbc(),Driver = "ODBC Driver 17 for SQL Server", Server = sqlServer,Database = databaseName, UID = sqlLogin, PWD = sqlPassword)
  res <- dbGetQuery(con, statement = read_file(sqlFile))
  setDT(res)
  cat(sprintf("%s lignes extraites\t", nrow(res)))
  res
}

getQueryFromSQLWinLinux <- function(sqlQuery, isWindows=T){
  #cat (sprintf("Requete %s\t: ", sqlQuery))
  if(get_os()=="windows")
    con <- dbConnect(odbc::odbc(),Driver = "SQL Server",Server = sqlServer,Database = databaseName, encoding="ISO-8859-1")
  else
    con <- dbConnect(odbc::odbc(),Driver = "ODBC Driver 17 for SQL Server", Server = sqlServer,Database = databaseName, UID = sqlLogin, PWD = sqlPassword)
  res <- dbGetQuery(con, statement = sqlQuery)
  setDT(res)
  cat(sprintf("%s lignes extraites\t", nrow(res)))
  res
}


# Code ggplot2 ------------------------------------------------------------

#ispfPalette <- c("#0071B2", "#A349A4", "#F07D17", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7", "#345687")

#windowsFonts("Roboto" = windowsFont("Roboto Light"))


createGenericGraphe <- function(i, keyList, originalOrder=T, legendPosition="none", legendDirection="none",
                                str_wrap_cle=15, str_wrap_variable=15,
                                ylabelFunction=scales::number_format(accuracy = 1),
                                axis.text.x.angle=0){
  
  message(sprintf("Création du graphique %s",i))
  graphe <- readCSVFileAndMelt(i, keyList, str_wrap_cle, str_wrap_variable, isOrdered = originalOrder)
  g <- ggplot(graphe) +
    scale_y_continuous(labels = ylabelFunction)+
    scale_fill_manual(values=ispfPalette)+
    scale_colour_manual(values=ispfPalette)+
    theme_ispf()+
    ylab("")+  xlab("")+
    theme(legend.position = legendPosition, legend.direction = legendDirection,
          axis.text.x = element_text(angle = axis.text.x.angle, hjust = 1))
  g
}


theme_ispf <- function (base_size = 8, base_family = "Roboto Light") 
{
  bgcolor <- "#FFFFFF"
  ret <- theme(rect = element_rect(fill = bgcolor, linetype = 0, colour = NA), 
               text = element_text(size = base_size, family = base_family), 
               title = element_text(size = base_size,hjust = 0.5, family = "Roboto Light"), 
               plot.title = element_text(hjust = 0.5, family = "Roboto Light"), 
               axis.title.x = element_blank(),
               axis.title.y = element_text(hjust = 0.5, family = base_family),
               panel.grid.major.y = element_line(colour = "#D8D8D8"), 
               panel.grid.minor.y = element_blank(),
               panel.grid.major.x = element_blank(), 
               panel.grid.minor.x = element_blank(), 
               panel.border = element_blank(), 
               panel.background = element_blank(),
               legend.key = element_rect(fill = "#FFFFFF00"),
               plot.margin=grid::unit(c(0,0,0,0), "mm"),
               legend.position = "bottom",
               legend.direction = "horizontal",
               legend.title = element_blank(),
               legend.margin=margin(t = 0, unit='cm'),
               legend.key.width=unit(0.2, "cm"),
               legend.text = element_text(size = base_size, family = base_family))
  ret
}

