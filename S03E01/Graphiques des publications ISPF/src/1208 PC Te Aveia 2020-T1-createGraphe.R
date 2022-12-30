library(ggrepel)
library(gridExtra)

windowsFonts("Roboto" = windowsFont("Roboto Light"))
#font_import()
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

cat("Création des graphiques en PDF\n")
i <- 8

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

g1 <- createSimpleGGPlot(1) + theme(legend.position = "none")
g2 <- createSimpleGGPlot(2) + ggtitle("Cours moyen du pétrole")
g3 <- createSimpleGGPlot(3) + theme(legend.position = "none")+ggtitle("Indice mensuel des matières\npremières alimentaires")
g4 <- createSimpleGGPlot(4)+ theme(legend.position = "none")+ggtitle("Indice mensuel\nmétaux et minéraux")
g5 <- createSimpleGGPlot(5)+ theme(legend.position = "none")+ggtitle("Dollar américain")
g6 <- createSimpleGGPlot(6)+ ggtitle("Dollar australien et néo-zélandais")
g7 <- createSimpleGGPlot(7)+ theme(legend.position = "none")+ggtitle("100 yens")
g8 <- createSimpleGGPlot(8)+ theme(legend.position = "none")+ggtitle("Dow Jones")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y")
g9 <- createSimpleGGPlot(9)+ theme(legend.position = "none")+ggtitle("Nikkei 225")+scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y")
g10 <- createSimpleGGPlot(10)+ theme(legend.position = "none")+ggtitle("Eurostoxx")+scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y")


g11 <- grid.arrange(g2,g3,g4, ncol=3, nrow = 1)
g12 <- grid.arrange(g5,g6,g7, ncol=3, nrow = 1)
g13 <- grid.arrange(g8,g9,g10, ncol=3, nrow = 1)

suppressMessages(ggsave("graphe1.pdf", g1, width = 9, height=4, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe2.pdf", g2, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe3.pdf", g3, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe4.pdf", g4, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe5.pdf", g5, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe6.pdf", g6, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe7.pdf", g7, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe8.pdf", g8, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe9.pdf", g9, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe10.pdf", g10, width = 9, height=5, units = "cm", device=cairo_pdf))

suppressMessages(ggsave("graphe11.pdf", g11, width = 18, height=6, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe12.pdf", g12, width = 18, height=6, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe13.pdf", g13, width = 18, height=6, units = "cm", device=cairo_pdf))


