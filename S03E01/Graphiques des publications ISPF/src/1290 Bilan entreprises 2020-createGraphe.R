library(ggrepel)
windowsFonts("Roboto" = windowsFont("Roboto Light"))
#font_import()
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

cat("Création des graphiques en PDF\n")
i <- 4

createSimpleGGPlot <- function(i){
  graphe <- fread(sprintf("graphe%d.txt", i), encoding = "UTF-8", header = T)
  graphe <- melt(graphe,id.vars="Année")
  #graphe3$variable <- factor(graphe3$variable,levels(graphe3$variable)[c(4,1,5,3,2,6)])
  g <- ggplot(graphe, aes(x=Année, y=value, fill=variable, colour=variable)) +
    geom_line()+
    geom_point()+
    scale_y_continuous(labels = scales::number_format(accuracy = 1), )+
    scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''))+
    scale_fill_manual(values=ispfPalette)+
    scale_colour_manual(values=ispfPalette)+
    theme_ispf()+
    ylab("")+
    theme(legend.position = "bottom",legend.direction = "horizontal")
  g
}

graphe1 <- readCSVFile(1)
#graphe1 <- melt(graphe1,id.vars="Annee")
#graphe1$Annee <- as.factor(graphe1$Annee)



g1 <- ggplot(graphe1,aes(x = stringr::str_wrap(Type,15),y=repartition, fill=Secteur)) +
  geom_col()+
  geom_text(aes(label=scales::percent(repartition, accuracy = 0.1)),  position = position_stack(vjust = .5),color="white", size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  coord_flip()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom",
        legend.direction = "horizontal")


g1


graphe2 <- readCSVFile(2)

g2 <- ggplot(arrange(graphe2,desc(Creations)) ,aes(x = reorder(ZoneGeo, Creations),y=Creations)) +
  geom_col(aes(fill = ZoneGeo))+
  geom_text(aes(label=scales::number(Creations)),  position = position_stack(vjust = .5),color="white", size=2)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  coord_flip()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position='none')

g2
  

graphe3 <- readCSVFile(3)
graphe3 <- melt(graphe3,id.vars="Secteur")
graphe3$variable <- as.integer(as.character(graphe3$variable))



g3 <- ggplot(graphe3,aes(x = variable,y=value, fill=Secteur, colour=Secteur)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=scales::number(value)),  position = position_stack(vjust = .5),color="white", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  theme_ispf()+
  theme(legend.key.height = unit(1, "cm"))+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g3



saveGrapheFiles(1, largeurCM = 9, hauteurCM = 4)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 8)
