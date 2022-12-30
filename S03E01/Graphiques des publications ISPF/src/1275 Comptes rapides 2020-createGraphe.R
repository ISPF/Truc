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


g1 <- ggplot(graphe1) +
  aes(x = Année, y = value, group = variable, color=variable) +
  geom_line(size = 0.5) +
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
g1




graphe2 <- readCSVFile(2)
graphe2 <- fread("graphe2.txt", encoding = "UTF-8", stringsAsFactors = T)
graphe2[,variable:=stringr::str_wrap(variable,15)]
g2 <- ggplot(graphe2,aes(x = variable,y=value, fill=variable)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=scales::percent(value, accuracy = 0.1)),
            position = position_stack(vjust = .5),
            color="white", size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("") +  ylab("") + theme_ispf()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g2

graphe3 <- readCSVFile(3)
graphe3 <- fread("graphe3.txt", encoding = "UTF-8")
graphe3 <- melt(graphe3,id.vars="Année")

g3 <- ggplot(graphe3) +
  aes(x = Année, y = value, group = variable, color=variable) +
  geom_line(size = 0.5) +
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(50,120))+
  xlab("123")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  
g3

graphe4 <- readCSVFile(4)
graphe4 <- fread("graphe4.txt", encoding = "UTF-8")
graphe4 <- melt(graphe4,id.vars="Année")
graphe4[,variable:=stringr::str_wrap(variable,12)]

g4 <- ggplot(graphe4, aes(x = Année,y=value, group=variable)) +
  geom_bar(aes(fill = variable), stat="identity")+
  geom_text(aes(label=scales::number(value, accuracy = 1)),
            position = position_stack(vjust = .5), color="white", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g4




saveGrapheFiles(1, largeurCM = 9, hauteurCM = 8)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 8)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 6)
