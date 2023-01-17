library(data.table)
library(ggplot2)
library(gganimate)
library(gifski)
# library(ggpol)

#fichierSource <- "J:/SIEP/2 - Groupe/Demographie/Demande extérieure/2022/APF/Pyramide_anime/Pyramide.csv"
fichierSource <- "C:/Users/nath_/Documents/BDD/Pyramide.csv"

#Sources
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7", "#345687")

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

decorateDataTable <- function(dt){
  dt[, Sexe:=as.factor(Sexe)]
  dt[Sexe=="1", Sexe:="Hommes"]
  dt[Sexe=="2", Sexe:="Femmes"]
  dt
}

createggplotPyramide <- function(dt){
  ggplot(dt,aes(x = Age,y=Pop, label=paste0("% des 60 ans et +\n",round(Part60,2)*100, " %") )) +
    geom_bar(data = dt, stat = "identity", aes(fill = factor(Sexe)),alpha=0.8, width=1) +
    coord_flip() + 
    xlab("") +  ylab("") + theme_ispf()+
    scale_fill_manual(values=ispfPalette)+
    scale_colour_manual(values=ispfPalette)+
    theme_ispf(base_size = 12)+
    theme(legend.position = "bottom", legend.direction = "horizontal")+
    scale_x_continuous(breaks=seq(0,90,5)) +
    scale_y_continuous(labels = abs)+
    geom_vline(xintercept = 60, color="red",size=1.5) +
    geom_text(aes(x = 75, y=-2000), color="red", size=5)
}

Pyramide<-fread(fichierSource, 
                          sep=";")

Pyramide[Age=='90+',Age:='90']
Pyramide[, Age:=as.numeric(Age)]
Pyramide[, Part60:=sum(abs(Pop)*(Age>=60))/sum(abs(Pop)),Annee]


Pyramide<-decorateDataTable(Pyramide)

PyramidePlot <- createggplotPyramide(Pyramide)+
  transition_states(Annee, transition_length = 1, state_length = 1,  wrap = TRUE) +
  ggtitle("Population de Polynésie française \n Année : {closest_state}", subtitle = "Source : ISPF")

animate(PyramidePlot, fps = 10, renderer=gifski_renderer("Pyramide.gif"))

