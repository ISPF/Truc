library(ggrepel)
library(ggplot2)
library(extrafont)

library(showtext)
font_add_google("Roboto", "Roboto")
loadfonts(device = "win")
windowsFonts("Roboto" = windowsFont("Roboto Light"))
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")
cat("Création des graphiques en PDF\n")

graphe1 <- readCSVFile(1)
graphe1 <- melt(graphe1,id.vars="Région")

g1 <- ggplot(graphe1, aes(x = Région,y=value, group=variable,fill = variable)) +
  geom_bar(stat="identity") +
  geom_text(data=graphe1,
            aes(x = Région,y=value, label=percent(value, accuracy = 1)),  position = position_stack(vjust = .5),color="white", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g1


graphe2 <- readCSVFile(2)
graphe2 <- melt(graphe2,id.vars="Région")

g2 <- ggplot(graphe2, aes(x = Région,y=value, group=variable,fill = variable)) +
  geom_bar(aes(x = variable, y=value, fill=Région), stat="identity", width=.5, position = position_stack(reverse = T))+
  geom_text(aes(x = variable, y=value, label=number(value, accuracy = 1)),  position = position_stack(vjust = .5,reverse = T),color="white", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(0,170000))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g2



graphe3 <- readCSVFile(3)
graphe3 <- melt(graphe3,id.vars="Compagnie")

g3 <- ggplot(graphe3, aes(x = Compagnie,y=value, group=variable,fill = variable)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(data=graphe3,
            aes(x = Compagnie,y=value, label=percent(value, accuracy = 1)),  position = position_dodge(width = 0.8),color="black", size=2, vjust = -0.5)+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g3


graphe4 <- readCSVFile(4)
graphe4 <- melt(graphe4,id.vars="Région")

g4 <- ggplot(graphe4, aes(x = variable,y=value, group=Région, colour=Région)) +
  geom_line(aes(x = variable,y=value, group=Région, colour=Région))+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(0,25000))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g4


graphe5 <- readCSVFile(5)
graphe5 <- melt(graphe5,id.vars="But")

g5 <- ggplot(graphe5, aes(x = But,y=value, group=variable,fill = variable)) +
  geom_bar(mapping=aes(x = But,y=value, fill=variable), stat="identity", width=.8, position = "dodge")+
  geom_text(aes(x = But,y=value, fill=variable, label=scales::percent(value, accuracy = 0.1)), 
            position = position_dodge(width = 0.8),color="black", size=2, vjust = -0.5)+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g5



graphe6 <- readCSVFile(6)
graphe6 <- melt(graphe6,id.vars="Ile")

g6 <- ggplot(graphe6, aes(x =Ile,y=value, group=variable,fill = variable)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(data=graphe6,
            aes(x = Ile,y=value, label=percent(value, accuracy = 1)),  position = position_dodge(width = 0.8),color="black", size=2, vjust = -0.5)+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g6






graphe8 <- readCSVFile(8)
graphe8 <- melt(graphe8,id.vars="Région")

g8 <- ggplot(graphe8, aes(x = variable,y=value, group=Région, colour=Région)) +
  geom_line(mapping=aes(x = variable,y=value, group=Région, colour=Région), stat="identity")+
  scale_y_continuous(limits=c(60,150))+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(0,160))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g8




graphe9 <- readCSVFile(9)
#graphe2[,`Recettes à l'export`:=as.integer(`Recettes à l'export`/10^6)]
graphe9 <- melt(graphe9,id.vars="Année")
graphe9[,variable:=stringr::str_wrap(variable,20)]


g9 <- ggplot() + 
  geom_bar(data=graphe9[variable!="CMR"], 
           mapping = aes(x = factor(Année), y = value, fill=variable), stat="identity", position="dodge")+ 
  geom_line(data=graphe9[variable=="CMR"],size=1.1,
            mapping = aes(x = factor(Année), y = value*45000,group=1, colour=variable))+
  scale_y_continuous(name = expression("RM et RevPar"), 
                     labels = scales::number_format(accuracy = 1),
                     limits=c(0,50000),
                     sec.axis = sec_axis(~ ./45000  , name = "CMR",labels = scales::percent_format(accuracy = 1),))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank()) + 
  guides(col = guide_legend(ncol = 2))
g9




graphe10 <- readCSVFile(10)
graphe10 <- melt(graphe10,id.vars="Région")

g10 <- ggplot(graphe10, aes(x =Région,y=value, group=variable,fill = variable)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(data=graphe10,
            aes(x = Région,y=value, label=percent(value, accuracy = 1)),  position = position_dodge(width = 0.8),color="black", size=2, vjust = -0.5)+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits=c(-1,0.1))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g10



#####Ancien 2019


graphe1 <- readCSVFile(1)

g1 <- createGenericGraphe(1, "Année", 
                          legendPosition = "bottom", legendDirection = "horizontal",axis.text.x.angle = 90,
                          ylabelFunction = percent_format(accuracy = 1))+
  geom_bar(aes(x = Année,y=value, fill=variable), stat="identity", width=.8, position = "stack")+
  geom_text(aes(x = Région,y=value, label=percent(value, accuracy = 1)),  position = position_stack(vjust = .5),color="white", size=2)
g1


g2 <- createGenericGraphe(2, "Région", legendPosition = "bottom", legendDirection = "horizontal",
                          ylabelFunction = number_format(accuracy = 1))+
  geom_bar(aes(x = variable, y=value, fill=Région), stat="identity", width=.5, position = position_stack(reverse = T))+
  geom_text(aes(x = variable, y=value, label=number(value, accuracy = 1)),  position = position_stack(vjust = .5,reverse = T),color="white", size=2)
g2


g3 <- createGenericGraphe(3, "Compagnie", axis.text.x.angle = 90, originalOrder = T, ylabelFunction = percent_format(accuracy = 1), str_wrap_cle = 10, legendPosition = "bottom", legendDirection = "horizontal")+
  geom_bar(aes(x=Compagnie, y=value, fill=variable), stat="identity", position = position_dodge(0.8))+
  geom_text(aes(x = Compagnie,y=value, fill=variable, label=scales::percent(value, accuracy = 1)), 
            position = position_dodge(width = 0.8),color="black", size=2, vjust = -0.5)+
  scale_y_continuous(limits=c(0,.65), labels = percent_format(accuracy = 1))

g3

g4 <- createGenericGraphe(4, "Région", 
                          legendPosition = "bottom", legendDirection = "horizontal", axis.text.x.angle = 90)+
  geom_line(aes(x = variable,y=value, group=Région, colour=Région))
g4

g5 <- createGenericGraphe(5, "But de séjour", legendPosition = "none", axis.text.x.angle = 90, ylabelFunction = percent_format(accuracy = 1))+
  geom_bar(mapping=aes(x = `But de séjour`,y=value, fill=variable), stat="identity", width=.8, position = "dodge")+
  geom_text(aes(x = `But de séjour`,y=value, fill=variable, label=scales::percent(value, accuracy = 0.1)), 
            position = position_dodge(width = 0.8),color="black", size=2, vjust = -0.5)
g5

g6 <- createGenericGraphe(6, "Île visitée", legendPosition = "none", axis.text.x.angle = 90, ylabelFunction = percent_format(accuracy = 1))+
  geom_bar(mapping=aes(x = `Île visitée`,y=value, fill=variable), stat="identity", position = "dodge")+
  geom_text(aes(x = `Île visitée`,y=value, fill=variable, label=scales::percent(value, accuracy = 1)), 
            position = position_dodge(width = 0.8),color="black", size=2, vjust = -0.5)
g6



g6 <- createGenericGraphe(6, "Île visitée", legendPosition = "none", axis.text.x.angle = 90, ylabelFunction = percent_format(accuracy = 1))+
  geom_bar(mapping=aes(x = `Île visitée`,y=value, fill=variable), stat="identity", position = "dodge")+
  geom_text(aes(x = `Île visitée`,y=value, fill=variable, label=scales::percent(value, accuracy = 1)), 
            position = position_dodge(width = 0.8),color="black", size=2, vjust = -0.5)
g6

g7 <- createGenericGraphe(7, "Région", legendPosition = "bottom",legendDirection = "horizontal", axis.text.x.angle = 90, ylabelFunction = number_format(accuracy = 1))+
  geom_line(mapping=aes(x = variable,y=value, group=Région, colour=Région), stat="identity")+
  scale_y_continuous(limits=c(60,150))
g7


graphe8 <- readCSVFileAndMelt(8, "Année")
grapheBar <- graphe8[variable!="CMR"]
grapheLines <- graphe8[variable=="CMR"]
g8 <- ggplot() +
  geom_bar(grapheBar,    mapping=aes(x=Année, y=value, group=variable, fill=variable), stat="identity",width = 0.8, position="dodge")+
  geom_line(grapheLines, mapping=aes(x=Année, y=value*45000, group=variable, fill=variable), colour=ispfPalette[1])+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(name = "RMC et RevPar",
                     labels = scales::number_format(accuracy = 1),
                     sec.axis = sec_axis(~./45000, name = "CMR", labels = scales::percent_format(accuracy = 1)))+
  theme_ispf()+
  xlab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.text.x = element_text(angle = 90, hjust = 1))

g8
g9 <- createGenericGraphe(9, "Région", legendPosition = "bottom",legendDirection = "horizontal", axis.text.x.angle = 90,
                           ylabelFunction = percent_format(accuracy = 1))+
  geom_bar(mapping=aes(x = Région,y=value, fill=variable), stat="identity", position = "dodge")+
  geom_text(aes(x = Région,y=value, fill=variable, label=scales::percent(value, accuracy = 1)), 
            position = position_dodge(width = 0.8),color="black", size=2, vjust = -0.5)
g9

saveGrapheFiles(1, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(5, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(6, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(7, largeurCM = 9, hauteurCM = 8)
saveGrapheFiles(8, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(9, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(10, largeurCM = 9, hauteurCM = 5)
