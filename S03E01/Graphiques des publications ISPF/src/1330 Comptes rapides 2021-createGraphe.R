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
graphe1 <- fread("graphe1.csv", encoding = "UTF-8")
graphe1 <- melt(graphe1,id.vars="Année")

#graphe1[variable=="2021e",variable:="2021"]

dataLabels <- graphe1[Année=="2021e"]

dataLabels[variable!="2021e", value:=NA]


g1 <- ggplot(graphe1) +
  aes(x = Année, y = value, group = variable, color=variable) +
  geom_line(size = 0.5) +
  #geom_text(data=dataLabels, aes(label=scales::number(value, accuracy = 1)),  position = position_dodge(width=0.8),color="black", size=2)+
  annotate(geom="text", x="2021e", y=0.07, label="7 %", color="black", size=2)+
  annotate(geom="text", x="2021e", y=0.024, label="2,4 %", color="black", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(-0.1,0.1))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
g1



###a faire
graphe2 <- readCSVFile(2)
graphe2 <- fread("graphe2.csv", encoding = "UTF-8")
graphe2 <- melt(graphe2,id.vars="Année")
graphe2[,variable:=stringr::str_wrap(variable,15)]

g2 <- ggplot(graphe2,aes(x = Année,y=value, groupe=variable)) +
  geom_bar(aes(fill = variable), stat = "identity", position="dodge") +
  #geom_text(aes(label=scales::percent(value, accuracy = 0.1)),
            #position = position_stack(vjust = .5),
            #color="white", size=2)+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark=""), breaks = seq(2018,2021,1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(-0.1,0.1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("") +  ylab("") + theme_ispf()+
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g2

graphe3 <- readCSVFile(3)
graphe3 <- fread("graphe3.txt", encoding = "UTF-8")
graphe3 <- melt(graphe3,id.vars="Année")
graphe3[,variable:=stringr::str_wrap(variable,18)]

g3 <- ggplot(graphe3) +
  aes(x = Année, y = value, group = variable, color=variable) +
  geom_line(size = 0.5) +
  annotate(geom="text", x="2021e", y=113, label="113", color="black", size=2)+
  annotate(geom="text", x="2021e", y=98, label="98", color="black", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(90,120))+
  xlab("123")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g3

graphe4 <- readCSVFile(4)
graphe4 <- fread("graphe4.txt", encoding = "UTF-8")
graphe4 <- melt(graphe4,id.vars="Année")
graphe4[,variable:=stringr::str_wrap(variable,20)]

g4 <- ggplot(graphe4, aes(x = Année,y=value, group=variable)) +
  geom_bar(aes(fill = variable), stat="identity", position="dodge")+
  #geom_text(aes(label=scales::percent(value, accuracy = 0.1)),
            #position = position_stack(vjust = 1), color="black", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark=""), breaks = seq(2018,2021,1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits=c(-0.05,0.05))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g4

graphe5 <- readCSVFile(5)
graphe5 <- fread("graphe5.txt", encoding = "UTF-8")
graphe5 <- melt(graphe5,id.vars="Année")
graphe5[,variable:=stringr::str_wrap(variable,20)]

g5 <- ggplot(graphe5, aes(x = Année,y=value, group=variable)) +
  geom_bar(aes(fill = variable), stat="identity")+
  geom_text(aes(label=scales::number(value, accuracy = 1)),
            position = position_stack(vjust = .5), color="white", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g5

###a faire
graphe6 <- readCSVFile(6)
graphe6 <- fread("graphe6.txt", encoding = "UTF-8")
graphe6 <- melt(graphe6,id.vars="Année")

g6 <- ggplot(graphe6) +
  aes(x = Année, y = value, group = variable, color=variable) +
  geom_line(size = 0.5) +
  annotate(geom="text", x="2021e", y=194,9, label="114,9", color="black", size=2)+
  annotate(geom="text", x="2021e", y=68,9, label="68,9", color="black", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(50,250))+
  xlab("123")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


g6


saveGrapheFiles(1, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(5, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(6, largeurCM = 9, hauteurCM = 6)
