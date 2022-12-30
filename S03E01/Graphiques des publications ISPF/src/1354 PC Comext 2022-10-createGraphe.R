library(ggrepel)
library(ggplot2)
library(extrafont)
library(gridExtra)
library(showtext)


cat("Création des graphiques en PDF\n")

graphe1 <- readCSVFile(1)
graphe1[, Date:=as.Date(Date, format="%Y-%m-%dT%H:%M:%SZ")]
graphe1[, `Importations civiles`:=as.integer(`Importations civiles`/10^6)]
graphe1[, `Exportations locales`:=as.integer(`Exportations locales`/10^6)]
grapheImpExp <- melt(graphe1,id.vars="Date")

g01 <- ggplot() + 
  geom_bar(data=grapheImpExp[variable=="Exportations locales"], 
           mapping = aes(x = Date, y = value * 40000/2000, fill="Exportations locales"), stat = "identity" )+ 
  geom_line(data=grapheImpExp[variable=="Importations civiles"],
            mapping = aes(x = Date, y = value, colour="Importations civiles"))+
  scale_x_date(date_labels = "%b %Y",
               breaks = seq(min(grapheImpExp$Date), max(grapheImpExp$Date),by = "1 year"),
               minor_breaks = "1 year")+
  scale_y_continuous(name = expression("Importations civiles en millions de F.CFP"), 
                    labels = scales::number_format(accuracy = 1),
                    limits=c(0,40000),
                   sec.axis = sec_axis(~ . * 2000 / 40000 , name = "Exportations locales en millions F.CFP",labels = scales::number_format(accuracy = 1)))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank())
g01



plotImpExp <- function(ValeurVariable, Titre, SousTitre, Diviseur=1){
  bgcolor <- "#FFFFFF"
  base_family <-  "Roboto Light"
  base_size <- 8
  g <- ggplot() + 
    geom_line(data=grapheImpExp[variable==ValeurVariable], 
              mapping = aes(x = Date, y = value/Diviseur, colour=ValeurVariable))+
    scale_x_date(date_labels = "%m/%y",
                 breaks = seq(min(grapheImpExp$Date), max(grapheImpExp$Date),by = "1 year"),
                 minor_breaks = "1 year")+
    scale_y_continuous(labels = scales::number_format(accuracy = 1))+
    scale_colour_manual(values=ispfPalette)+
    ggtitle(label = Titre)+
    #par(mar = c(1,1,10,1)) +
    theme(legend.position = "none", 
          plot.title = element_text(hjust=0.5, family="Roboto Light", size=8, face="bold"),
          plot.subtitle = element_text(hjust=0.5, family="Roboto Light", size=6),
          panel.border = element_rect(colour = "black", fill=NA, size=0.1),
          panel.background = element_blank(),
          # rect = element_rect(fill = bgcolor, linetype = 0, colour = NA), 
          # text = element_text(size = base_size, family = base_family), 
          title = element_text(hjust = 0.5, family = base_family, face="bold"), 
          axis.title.y = element_text(hjust = 0.5, family="Roboto Light", size=6, face="bold"),
          panel.grid.major.y = element_line(colour = "#D8D8D8"), 
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          # legend.key = element_rect(fill = "#FFFFFF00"),
          # plot.margin=grid::unit(c(0,0,8,0), "mm"),
          # legend.direction = "vertical",
          # legend.title = element_blank(),
          #legend.key.width=unit(2, "cm")
          )+
    xlab("")+ylab(SousTitre)
    
  g
}

g02 <- plotImpExp("Gazole", "Gazole", "Millions de litres", Diviseur = 10^6)
g03 <- plotImpExp("Médicaments", "Médicaments", "Millions de F.CFP", Diviseur = 10^6)
g04 <- plotImpExp("Voitures de tourisme", "Voitures de tourisme", "Millions de F.CFP", Diviseur = 10^6)
g05 <- plotImpExp("Ciments", "Ciments", "Kilotonnes", Diviseur = 10^6)
g06 <- plotImpExp("Perles brutes", "Perles brutes", "kg")
g08 <- plotImpExp("Vanille", "Vanille", "kg")
g09 <- plotImpExp("Poissons, crustacés", "Poissons", "Tonnes", Diviseur = 10^3)


gGrille <- grid.arrange(g05,g02,g04,g06,g09,g08,ncol=3, nrow =2)

g1 <- grid.arrange(g01, gGrille, ncol=1, nrow =2)


saveGrapheFiles(1, largeurCM = 18, hauteurCM = 15)

