library(ggrepel)
windowsFonts("Roboto" = windowsFont("Roboto Light"))
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7", "#345687")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")
cat("Création des graphiques en PDF\n")


graphe1 <- readCSVFile(1)
graphe1 <- melt(graphe1,id.vars="Année")

grapheBar <- graphe1[variable=="PIB"]
grapheLines <- graphe1[variable=="Variation annuelle"]


g1 <- ggplot(graphe1,aes(x = Année,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "stack")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none")
g1


graphe2 <- readCSVFile(2)
graphe2 <- melt(graphe2,id.vars="indicateur")
graphe2[,indicateur:=stringr::str_wrap(indicateur, 20)]
grapheBar <- graphe2[indicateur=="Contribution\ntrimestrielle au PIB"]
grapheLines <- graphe2[indicateur=="Evolution T-1"]
g2 <- ggplot(graphe2) +
  geom_bar(grapheBar,    mapping=aes(x=variable, group=indicateur, y=value, fill=indicateur), stat="identity",width = 0.8, position="stack")+
  geom_line(grapheLines, mapping=aes(x=variable, group=indicateur, y=value*30, fill=indicateur))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(name = "Contribution\ntrimestrielle au PIB",
                     labels = scales::number_format(accuracy = 1),
                     sec.axis = sec_axis(~./30, 
                                         name = "Evolution T-1",
                                         labels = scales::percent_format(accuracy = 1)))+
  theme_ispf()+
  xlab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal")

g2


graphe3 <- readCSVFile(3)
graphe3 <- melt(graphe3,id.vars="Indicateur")
graphe3[,Indicateur:=stringr::str_wrap(Indicateur, 20)]
g3 <- ggplot(graphe3,aes(x = Indicateur,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none")
g3


saveGrapheFiles(1, largeurCM = 9, hauteurCM = 4)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 7)