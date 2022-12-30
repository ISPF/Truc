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


graphe1 <- readCSVFile(1)
graphe1 <- melt(graphe1,id.vars="Date")

g1 <- ggplot(graphe1) +
  aes(x = Date, y = value, group = variable, color=variable) +
  geom_line(size = 0.5) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(2015,2021,1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(700,1000))+
  xlab("123")+   ylab("milliards de F.CFP")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "vertical")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g1


graphe2 <- readCSVFile(2)
graphe2 <- melt(graphe2,id.vars="Date")

g2 <- ggplot(graphe2) +
  aes(x = Date, y = value, group = variable, color=variable) +
  geom_line(size = 0.5) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(2015,2021,1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(1000000,4000000))+
  xlab("123")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "vertical")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g2


graphe3 <- readCSVFile(3)
graphe3 <- melt(graphe3,id.vars="Produit")


g3 <- ggplot(graphe3,aes(x = Produit,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.12))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab(" en %")+  xlab("")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,0.15))+
  theme(legend.position = "none", legend.direction = "horizontal")+
  coord_flip()
g3






g4 <- createSimpleGGPlot(4) + ggtitle("Cours moyen du pétrole")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2016-12-01","2021-12-31")), breaks = "12 months")
g5 <- createSimpleGGPlot(5)+ theme(legend.position = "none")+ggtitle("Indice mensuel des matières\npremières alimentaires\n(Base 100, année 2010)")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2016-12-01","2021-12-31")), breaks = "12 months")
g6 <- createSimpleGGPlot(6)+ theme(legend.position = "none")+ggtitle("Indice mensuel\nmétaux et minéraux\n(Base 100, année 2010)")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2016-12-01","2021-12-31")), breaks = "12 months")
g7 <- createSimpleGGPlot(7)+ theme(legend.position = "none")+ggtitle("Dollar américain")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2016-12-01","2021-12-31")), breaks = "12 months")
g8 <- createSimpleGGPlot(8)+ ggtitle("Dollar australien et néo-zélandais")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2016-12-01","2021-12-31")), breaks = "12 months")
g9 <- createSimpleGGPlot(9)+ theme(legend.position = "none")+ggtitle("100 yens")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2016-12-01","2021-12-31")), breaks = "12 months")
g10 <- createSimpleGGPlot(10)+ theme(legend.position = "none")+ggtitle("Dow Jones")+ 
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2020-12-01","2021-12-31")), breaks = "3 months")
g11 <- createSimpleGGPlot(11)+ theme(legend.position = "none")+ggtitle("Nikkei 225")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2020-12-01","2021-12-31")), breaks = "3 months")
g12 <- createSimpleGGPlot(12)+ theme(legend.position = "none")+ggtitle("Eurostoxx")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2020-12-01","2021-12-31")), breaks = "3 months")


g13 <- grid.arrange(g4,g5,g6, ncol=3, nrow = 1)
g14 <- grid.arrange(g7,g8,g9, ncol=3, nrow = 1)
g15 <- grid.arrange(g10,g11,g12, ncol=3, nrow = 1)

saveGrapheFiles(1, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(5, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(6, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(7, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(8, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(9, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(10, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(11, largeurCM = 8, hauteurCM = 5)
saveGrapheFiles(12, largeurCM = 8, hauteurCM = 5)
saveGrapheFiles(13, largeurCM = 18, hauteurCM = 6)
saveGrapheFiles(14, largeurCM = 18, hauteurCM = 6)
saveGrapheFiles(15, largeurCM = 18, hauteurCM = 6)
#saveGrapheFiles(16, largeurCM = 18, hauteurCM = 6)

