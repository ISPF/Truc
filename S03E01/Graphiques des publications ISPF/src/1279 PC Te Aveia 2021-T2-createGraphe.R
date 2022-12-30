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
graphe1 <- melt(graphe1,id.vars="CA")


g1 <- ggplot(graphe1,aes(x = CA,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab(" en %")+  xlab("")+
  theme(legend.position = "bottom", legend.direction = "horizontal")
g1

# colnames(graphe2)[2:3] <- c("Volume", "Prix moyen au gramme")




graphe2 <- readCSVFile(2)
graphe2 <- fread("graphe2.txt", encoding = "UTF-8")
graphe2 <- melt(graphe2,id.vars="Date")

g2 <- ggplot(graphe2) +
  aes(x = Date, y = value, group = variable, color=variable) +
  geom_line(size = 0.5) +
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(0,8))+
  xlab("")+   ylab("Milliards de F.CFP")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g2


graphe3 <- readCSVFile(3)
graphe3 <- melt(graphe3,id.vars="Date")

g3 <- ggplot(graphe3) +
  aes(x = Date, y = value, group = variable, color=variable) +
  geom_line(size = 0.5) +
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(0,300000))+
  xlab("123")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g3


graphe4 <- readCSVFile(4)
graphe4 <- melt(graphe4,id.vars="Trimestre")

g4 <- ggplot(graphe4) +
  aes(x = Trimestre, y = value, group = variable, color=variable) +
  geom_line(size = 0.5) +
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(60000,130000))+
  xlab("123")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g4


g5 <- createSimpleGGPlot(5) + ggtitle("Cours moyen du pétrole")
g6 <- createSimpleGGPlot(6)+ theme(legend.position = "none")+ggtitle("Indice mensuel des matières\npremières alimentaires\n(Base 100, année 2010)")
g7 <- createSimpleGGPlot(7)+ theme(legend.position = "none")+ggtitle("Indice mensuel\nmétaux et minéraux\n(Base 100, année 2010)")
g8 <- createSimpleGGPlot(8)+ theme(legend.position = "none")+ggtitle("Dollar américain")
g9 <- createSimpleGGPlot(9)+ ggtitle("Dollar australien et néo-zélandais")
g10 <- createSimpleGGPlot(10)+ theme(legend.position = "none")+ggtitle("100 yens")
g11 <- createSimpleGGPlot(11)+ theme(legend.position = "none")+ggtitle("Dow Jones")+ 
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2020-06-01","2021-06-30")), breaks = "3 months")
g12 <- createSimpleGGPlot(12)+ theme(legend.position = "none")+ggtitle("Nikkei 225")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2020-06-01","2021-06-30")), breaks = "3 months")
g13 <- createSimpleGGPlot(13)+ theme(legend.position = "none")+ggtitle("Eurostoxx")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2020-06-01","2021-06-30")), breaks = "3 months")


g14 <- grid.arrange(g5,g6,g7, ncol=3, nrow = 1)
g15 <- grid.arrange(g8,g9,g10, ncol=3, nrow = 1)
g16 <- grid.arrange(g11,g12,g13, ncol=3, nrow = 1)

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
saveGrapheFiles(13, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(14, largeurCM = 18, hauteurCM = 6)
saveGrapheFiles(15, largeurCM = 18, hauteurCM = 6)
saveGrapheFiles(16, largeurCM = 18, hauteurCM = 6)

