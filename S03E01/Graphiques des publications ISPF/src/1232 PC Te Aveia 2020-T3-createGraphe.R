library(ggrepel)
library(gridExtra)

windowsFonts("Roboto" = windowsFont("Roboto Light"))
#font_import()
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

cat("Création des graphiques en PDF\n")
i <- 2

createSimpleGGPlot <- function(i){
  graphe <- fread(sprintf("graphe%d.txt", i), encoding = "UTF-8", header = T)
  graphe <- melt(graphe,id.vars="Date")
  #graphe3$variable <- factor(graphe3$variable,levels(graphe3$variable)[c(4,1,5,3,2,6)])
  graphe$Date <- as.Date(graphe$Date)
  g <- ggplot(graphe, aes(x=Date, y=value, color=variable)) +
    geom_line()+
    #geom_point()+
    scale_fill_manual(values=ispfPalette)+
    scale_colour_manual(values=ispfPalette)+
    theme_ispf()+
    ylab("")+
    xlab("")+
    theme(legend.position = "top",legend.direction = "horizontal")
  g
}

graphe1 <- fread("graphe1.txt", encoding = "UTF-8", header = T)
graphe1 <- melt(graphe1,id.vars="Année")
graphe1[,variable:=stringr::str_wrap(variable, 25)]

g1 <- ggplot(graphe1) +
  geom_line(aes(x=Année, y=value, group=1), color=ispfPalette[1])+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),
                     breaks = seq(2010,2020,2),
                     limits=c(2010,2020))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(320000,460000))+
  ylab("")+
  xlab("")+
  theme(legend.position = "top",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 90))
g1


graphe2 <- fread("graphe2.txt", encoding = "UTF-8", header = T)
colnames(graphe2)[2] <- "variable"

g2 <- ggplot(graphe2) +
  geom_line(aes(x=Date, y=variable, group=1), color=ispfPalette[1])+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(0,4000000))+
  ylab("")+
  xlab("")+
  theme(legend.position = "top",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 90))
g2


g3 <- createSimpleGGPlot(3) + ggtitle("Cours moyen du pétrole")
g4 <- createSimpleGGPlot(4)+ theme(legend.position = "none")+ggtitle("Indice mensuel des matières\npremières alimentaires\n(Base 100, année 2010)")
g5 <- createSimpleGGPlot(5)+ theme(legend.position = "none")+ggtitle("Indice mensuel\nmétaux et minéraux\n(Base 100, année 2010)")
g6 <- createSimpleGGPlot(6)+ theme(legend.position = "none")+ggtitle("Dollar américain")
g7 <- createSimpleGGPlot(7)+ ggtitle("Dollar australien et néo-zélandais")
g8 <- createSimpleGGPlot(8)+ theme(legend.position = "none")+ggtitle("100 yens")
g9 <- createSimpleGGPlot(9)+ theme(legend.position = "none")+ggtitle("Dow Jones")+ 
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y")
g10 <- createSimpleGGPlot(10)+ theme(legend.position = "none")+ggtitle("Nikkei 225")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y")
g11 <- createSimpleGGPlot(11)+ theme(legend.position = "none")+ggtitle("Eurostoxx")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y")


g12 <- grid.arrange(g3,g4,g5, ncol=3, nrow = 1)
g13 <- grid.arrange(g6,g7,g8, ncol=3, nrow = 1)
g14 <- grid.arrange(g9,g10,g11, ncol=3, nrow = 1)

suppressMessages(ggsave("graphe1.pdf", g1, width = 9, height=6.5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe2.pdf", g2, width = 9, height=6, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe3.pdf", g3, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe4.pdf", g4, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe5.pdf", g5, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe6.pdf", g6, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe7.pdf", g7, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe8.pdf", g8, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe9.pdf", g9, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe10.pdf", g10, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe11.pdf", g11, width = 9, height=5, units = "cm", device=cairo_pdf))

suppressMessages(ggsave("graphe12.pdf", g12, width = 18, height=6, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe13.pdf", g13, width = 18, height=6, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe14.pdf", g14, width = 18, height=6, units = "cm", device=cairo_pdf))


