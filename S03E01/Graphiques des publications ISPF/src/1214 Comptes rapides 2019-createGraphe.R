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

graphe3 <- fread("graphe3.txt", encoding = "UTF-8")
graphe3 <- melt(graphe3,id.vars="Année")

g3 <- ggplot(graphe3) +
  aes(x = Année, y = value, group = variable, color=variable) +
  geom_line(size = 0.5) +
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(480,600))+
  xlab("123")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  
g3


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

graphe5 <- fread("graphe5.txt", encoding = "UTF-8")
graphe5 <- melt(graphe5,id.vars="Année")
graphe5[,variable:=stringr::str_wrap(variable,6)]

g5 <- ggplot(graphe5, aes(x = Année,y=value, group=variable)) +
  geom_bar(aes(fill = variable), stat="identity")+
  geom_text(aes(label=scales::number(value, accuracy = 1)),
            position = position_stack(vjust = .5), color="white", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g5

graphe6 <- fread("graphe6.txt", encoding = "UTF-8")
graphe6 <- melt(graphe6,id.vars="Année")

g6 <- ggplot(graphe6) +
  aes(x = Année, y = value, group = variable, color=variable) +
  geom_line(size = 0.5) +
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(0,1600),
                     breaks=c(0,400,800,1200,1600))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g6



suppressMessages(ggsave("graphe1.pdf", g1, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe2.pdf", g2, width = 9, height=9, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe3.pdf", g3, width = 8, height=8, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe4.pdf", g4, width = 9, height=5.5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe5.pdf", g5, width = 9, height=6, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe6.pdf", g6, width = 9, height=7, units = "cm", device=cairo_pdf))


