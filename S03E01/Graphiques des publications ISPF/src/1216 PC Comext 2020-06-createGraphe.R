library(ggrepel)
library(ggplot2)
library(extrafont)
library(gridExtra)
library(showtext)

font_add_google("Roboto", "Roboto")
loadfonts(device = "win")
windowsFonts("Roboto" = windowsFont("Roboto Light"))
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")
cat("Création des graphiques en PDF\n")

graphe1 <- fread("graphe1.txt", encoding = "UTF-8")
graphe1[, Date:=as.Date(Date, format="%Y-%m-%dT%H:%M:%SZ")]
graphe1[, `Importations civiles`:=as.integer(`Importations civiles`/10^6)]
graphe1[, `Exportations locales`:=as.integer(`Exportations locales`/10^6)]
grapheImpExp <- melt(graphe1,id.vars="Date")




g1 <- ggplot() + 
  geom_bar(data=grapheImpExp[variable=="Exportations locales"], 
           mapping = aes(x = Date, y = value * 40000/2000, fill="Exportations locales"), stat = "identity" )+ 
  geom_line(data=grapheImpExp[variable=="Importations civiles"],
            mapping = aes(x = Date, y = value, colour="Importations civiles"))+
  scale_x_date(date_labels = "%b %Y",
               breaks = seq(min(grapheImpExp$Date), max(grapheImpExp$Date),by = "1 year"),
               minor_breaks = "1 year")+
  scale_y_continuous(name = expression("Importations civiles"), 
                    labels = scales::number_format(accuracy = 1),
                   sec.axis = sec_axis(~ . * 2000 / 40000 , name = "Exportations locales"))+
  scale_fill_manual(values=ispfPalette)+
  labs(caption = "Millions de F.CFP")+
  theme_ispf()+
  xlab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank())
g1

plotImpExp <- function(ValeurVariable, Titre, SousTitre, Diviseur=1){
  bgcolor <- "#FFFFFF"
  base_family <-  "Roboto Light"
  base_size <- 8
  g <- ggplot() + 
    geom_line(data=grapheImpExp[variable==ValeurVariable], 
              mapping = aes(x = Date, y = value/Diviseur, colour=ValeurVariable))+
    scale_x_date(date_labels = "%b %Y",
                 breaks = seq(min(grapheImpExp$Date), max(grapheImpExp$Date),by = "1 year"),
                 minor_breaks = "1 year")+
    scale_y_continuous(labels = scales::number_format(accuracy = 1))+
    scale_colour_manual(values=ispfPalette)+
    ggtitle(label = Titre)+
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
          # #plot.margin=grid::unit(c(0,0,0,0), "mm"),
          # legend.direction = "vertical",
          # legend.title = element_blank(),
          # legend.key.width=unit(0.2, "cm")
          )+
    xlab("")+ylab(SousTitre)
    
  g
}

g2 <- plotImpExp("Gazole", "Gazole", "Millions de litres", Diviseur = 10^6)
g3 <- plotImpExp("Médicaments", "Médicaments", "Millions de F.CFP", Diviseur = 10^6)
g4 <- plotImpExp("Voitures de tourisme", "Voitures de tourisme", "Nombre")
g5 <- plotImpExp("Ciments", "Ciments", "Kilotonnes", Diviseur = 10^6)
g6 <- plotImpExp("Perles brutes", "Perles brutes", "kg", Diviseur = 10^3)
g7 <- plotImpExp("Noni", "Purée et jus de noni","Tonnes", Diviseur = 10^3)
g8 <- plotImpExp("Vanille", "Vanille", "kg")
g9 <- plotImpExp("Poissons, crustacés", "Poissons", "Tonnes", Diviseur = 10^3)

#g10 <- grid.arrange(g2,g3,g4,g5, ncol=2, nrow =2)
#g11 <- grid.arrange(g6,g7,g8,g9, ncol=2, nrow =2)

#g12 <- grid.arrange(g2,g3,g6,g7,g4,g5,g8,g9,ncol=4, nrow =2)
g12 <- grid.arrange(g5,g2,g4,g6,g9,g8,ncol=3, nrow =2)



suppressMessages(ggsave("graphe1.pdf", g1, width = 9, height=9, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe2.pdf", g12, width = 18, height=12, units = "cm", device=cairo_pdf))
