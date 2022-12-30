library(ggrepel)
windowsFonts("Roboto" = windowsFont("Roboto Light"))
#font_import()
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

cat("Création des graphiques en PDF\n")
i <- 6

createSimpleGGPlot <- function(i){
  graphe <- fread(sprintf("graphe%d.txt", i), encoding = "UTF-8", header = T)
  graphe <- melt(graphe,id.vars="Année")
  #graphe3$variable <- factor(graphe3$variable,levels(graphe3$variable)[c(4,1,5,3,2,6)])
  g <- ggplot(graphe, aes(x=Année, y=value, fill=variable, colour=variable)) +
    geom_line()+
    geom_point()+
    scale_y_continuous(labels = scales::number_format(accuracy = 1))+
    scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''))+
    scale_fill_manual(values=ispfPalette)+
    scale_colour_manual(values=ispfPalette)+
    theme_ispf()+
    ylab("")+
    theme(legend.position = "bottom",legend.direction = "horizontal")
  g
}



g2 <- createSimpleGGPlot(2)
g2 <- g2+
  theme(legend.position = "none")

g3 <- createSimpleGGPlot(3)+
  scale_x_discrete(limits=c(2008,2009,2011,2013,2015,2017),
                   labels=c(2008,2009,2011,2013,2015,2017))
g4 <- createSimpleGGPlot(4)
g5 <- createSimpleGGPlot(5)
g5 <- g5+
  theme(legend.position = "right",legend.direction = "vertical")

g6 <- createSimpleGGPlot(6)+
  scale_x_discrete(limits=c(2008,2009,2011,2013,2015,2017),
                   labels=c(2008,2009,2011,2013,2015,2017))
g7 <- createSimpleGGPlot(7)+
  scale_x_discrete(limits=c(2008,2009,2011,2013,2015,2017),
                   labels=c(2008,2009,2011,2013,2015,2017))

suppressMessages(ggsave("graphe2.pdf", g2, width = 9, height=4, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe3.pdf", g3, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe4.pdf", g4, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe5.pdf", g5, width = 9, height=4, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe6.pdf", g6, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe7.pdf", g7, width = 9, height=5, units = "cm", device=cairo_pdf))


