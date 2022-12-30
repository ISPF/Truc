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



g1 <- createSimpleGGPlot(1)
g1 <- g1+
  scale_y_discrete(limits=c(100,150,200,250,300,350),
                   labels=c(100,150,200,250,300,350))+
  theme(legend.position = "none")
g1

g2 <- createSimpleGGPlot(2)+
  scale_x_discrete(limits=c(2009,2010,2012,2014,2016,2018),
                   labels=c(2009,2010,2012,2014,2016,2018))
g4 <- createSimpleGGPlot(4)+
  scale_x_discrete(limits=c(2008,2010,2012,2014,2016,2018),
                   labels=c(2008,2010,2012,2014,2016,2018))

g5 <- createSimpleGGPlot(5)
g5 <- g5+
  theme(legend.position = "right",legend.direction = "vertical")

g6 <- createSimpleGGPlot(6)
g7 <- createSimpleGGPlot(7)







suppressMessages(ggsave("graphe1.pdf", g1, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe2.pdf", g2, width = 9, height=4, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe4.pdf", g4, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe5.pdf", g5, width = 9, height=4, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe6.pdf", g6, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe7.pdf", g7, width = 9, height=5, units = "cm", device=cairo_pdf))


