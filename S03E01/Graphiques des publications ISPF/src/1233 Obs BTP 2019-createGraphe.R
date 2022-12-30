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

graphe1 <- fread("graphe1.txt", encoding = "UTF-8")
graphe1 <- melt(graphe1,id.vars="Année")
graphe1[,variable:=stringr::str_wrap(variable,10)]
g1 <- ggplot(graphe1) +
  aes(x = Année, y = value, group = variable, color=variable) +
  geom_line(size = 0.5) +
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(20,120), breaks=seq(20,120,20))+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ""))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g1

graphe2 <- fread("graphe2.txt", encoding = "UTF-8")
graphe2 <- melt(graphe2,id.vars="Année")
graphe2[,variable:=stringr::str_wrap(variable,10)]
graphe2[, variable:=factor(variable)]
graphe2[, variable:=factor(variable, levels(variable)[c(1,2,3,5,4)])]

g2 <- ggplot(graphe2) +
  aes(x = Année, y = value, group = variable, color=variable) +
  geom_line(size = 0.5) +
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(20,140), breaks=seq(20,140,20))+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ""))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g2
  

graphe3 <- fread("graphe3.txt", encoding = "UTF-8")
graphe3 <- melt(graphe3,id.vars="Année")
graphe3[,variable:=stringr::str_wrap(variable,10)]
graphe3[, variable:=factor(variable)]
graphe3[, variable:=factor(variable, levels(variable)[c(1,2,3,5,4)])]

g3 <- ggplot(graphe3) +
  aes(x = Année, y = value, group = variable, color=variable) +
  geom_line(size = 0.5) +
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), 
                     limits=c(20,160), breaks=seq(20,160,20))+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ""))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g3

graphe4 <- fread("graphe4.txt", encoding = "UTF-8")
graphe4 <- melt(graphe4,id.vars="Secteur")
graphe4[,variable:=stringr::str_wrap(variable,10)]
graphe4[,Secteur:=stringr::str_wrap(Secteur,18)]
graphe4[, Secteur:=factor(Secteur)]
graphe4[, Secteur:=factor(Secteur, levels(Secteur)[c(5,2,3,4,6,1)])]



g4 <- ggplot(graphe4,aes(x = Secteur,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "stack")+
  geom_text(aes(label=scales::percent(value, accuracy = 1)),  position = position_stack(vjust = .5),color="white", size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "right", legend.direction = "vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g4

graphe6 <- fread("graphe6.txt", encoding = "UTF-8")
graphe6 <- melt(graphe6,id.vars="Indicateur")
graphe6[,variable:=stringr::str_wrap(variable,20)]
graphe6[,Indicateur:=stringr::str_wrap(Indicateur,15)]

g6 <- ggplot(graphe6,aes(x = Indicateur,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  geom_text(aes(label=scales::number(value, accuracy = 0.1)),  position = position_dodge(width=0.8),color="black", size=2)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom", legend.direction = "horizontal")
g6

graphe7 <- fread("graphe7.txt", encoding = "UTF-8")
graphe7 <- melt(graphe7,id.vars="Année")
graphe7[,variable:=stringr::str_wrap(variable,20)]
g7 <- ggplot(graphe7) +
  aes(x = Année, y = value, group = variable, color=variable) +
  geom_line(size = 0.5) +
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(40,180), breaks=seq(40,180,20))+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ""))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g7


suppressMessages(ggsave("graphe1.pdf", g1, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe2.pdf", g2, width = 9, height=6, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe3.pdf", g3, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe4.pdf", g4, width = 9, height=6, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe6.pdf", g6, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe7.pdf", g7, width = 9, height=7, units = "cm", device=cairo_pdf))



