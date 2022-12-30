library(gridExtra)
cat("Création des graphiques en PDF\n")


createSimpleGGPlot <- function(i){
  graphe <- readCSVFile(i)
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

g1 <- createSimpleGGPlot(1)+ 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits=c(-0.15,0.05))+
  scale_x_date(date_labels = "%b %Y")+
  theme(legend.position = "none")
g1




graphe2 <- readCSVFile(2)
colnames(graphe2) <- c("composante", "value")
graphe2[,composante:=stringr::str_wrap(composante, 35)]
graphe2[, composante:=forcats::fct_reorder(composante, value)]
graphe2[, composante:=factor(composante, levels=c(levels(graphe2$composante)[-7], "Indice général"))]

g2 <- ggplot(data=graphe2) +
  geom_bar(mapping = aes(x=composante, y=value, fill="Evolution"), stat = "identity" )+
  geom_text(aes(x=composante, y=value, label=percent(value, accuracy = .1, decimal.mark = ",")),  size=2)+
 
 scale_fill_manual(values=ispfPalette)+
 theme_ispf()+
 scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
 ylab("")+
 xlab("")+
 theme(legend.position = "none")+
 coord_flip()
g2


g3 <- createSimpleGGPlot(3) + ggtitle("Cours moyen du pétrole")
g4 <- createSimpleGGPlot(4)+ theme(legend.position = "none")+ggtitle("Indice mensuel des matières\npremières alimentaires\n(Base 100, année 2010)")
g5 <- createSimpleGGPlot(5)+ theme(legend.position = "none")+ggtitle("Indice mensuel\nmétaux et minéraux\n(Base 100, année 2010)")
g6 <- createSimpleGGPlot(6)+ theme(legend.position = "none")+ggtitle("Dollar américain")
g7 <- createSimpleGGPlot(7)+ ggtitle("Dollar australien et néo-zélandais")
g8 <- createSimpleGGPlot(8)+ theme(legend.position = "none")+ggtitle("100 yens")
g9 <- createSimpleGGPlot(9)+ theme(legend.position = "none")+ggtitle("Dow Jones")+ 
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2019-11-01","2020-12-30")), breaks = "2 months")
g10 <- createSimpleGGPlot(10)+ theme(legend.position = "none")+ggtitle("Nikkei 225")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2019-11-01","2020-12-30")), breaks = "2 months")
g11 <- createSimpleGGPlot(11)+ theme(legend.position = "none")+ggtitle("Eurostoxx")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2019-11-01","2020-12-30")), breaks = "2 months")


g12 <- grid.arrange(g3,g4,g5, ncol=3, nrow = 1)
g13 <- grid.arrange(g6,g7,g8, ncol=3, nrow = 1)
g14 <- grid.arrange(g9,g10,g11, ncol=3, nrow = 1)

saveGrapheFiles(1, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(5, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(6, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(7, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(8, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(9, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(10, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(11, largeurCM = 9, hauteurCM = 5)

saveGrapheFiles(12, largeurCM = 18, hauteurCM = 6)
saveGrapheFiles(13, largeurCM = 18, hauteurCM = 6)
saveGrapheFiles(14, largeurCM = 18, hauteurCM = 6)

