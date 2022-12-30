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
graphe1[,variable:=factor(variable, levels=c("Construction","Hébergement et restauration","Transports et entreposage","Autres","Commerce"))]
#graphe1[,variable:=stringr::str_wrap(variable,10)]
graphe1[,variablse:=factor(variables, levels=r, ordered=T)]




g1 <- ggplot(graphe1, aes(x = factor(Date), y = value, groupe=variable)) + 
  geom_bar(aes(fill=variable), stat="identity")+
  #geom_text(data=graphe1,aes(x = factor(Année), y = value, label=scales::number(value,accuracy = 1)),  position = position_dodge(width=0), vjust=-2 ,color="black", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(0,205000)) +
  theme_ispf()+
  xlab("")+ylab("")+
  theme(legend.text=element_text(size=rel(0.7)))+
  #guides(col = guide_legend(ncol = 2))
  guides(fill = guide_legend(nrow = 2))

g1



graphe2 <- readCSVFile(2)
graphe2 <- melt(graphe2,id.vars="Année")
graphe2[,variable:=stringr::str_wrap(variable,10)]


g2 <- ggplot(graphe2, aes(x = Année, y = value, color=variable)) + 
  geom_line()+
  theme_ispf()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(85,105)) +
  scale_fill_manual(values=ispfPalette)+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank()) 
  #guides(col = guide_legend(ncol = 2))
g2





graphe3 <- readCSVFile(3)
graphe3 <- melt(graphe3,id.vars="Année")
graphe3[,variable:=stringr::str_wrap(variable,10)]


g3 <- ggplot(graphe3, aes(x = factor(Année), y = value, groupe=variable)) + 
  geom_bar(aes(fill=variable), stat="identity", position = "dodge")+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(0,45000)) +
  theme_ispf()+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank()) + 
  guides(col = guide_legend(ncol = 2))
g3


g4 <- createSimpleGGPlot(4) + ggtitle("Cours moyen du pétrole")
g5 <- createSimpleGGPlot(5)+ theme(legend.position = "none")+ggtitle("Indice mensuel des matières\npremières alimentaires\n(Base 100, année 2010)")
g6 <- createSimpleGGPlot(6)+ theme(legend.position = "none")+ggtitle("Indice mensuel\nmétaux et minéraux\n(Base 100, année 2010)")
g7 <- createSimpleGGPlot(7)+ theme(legend.position = "none")+ggtitle("Dollar américain")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2017-06-30","2022-07-31")), breaks = "1 years")
g8 <- createSimpleGGPlot(8)+ ggtitle("Dollar australien et néo-zélandais")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2017-09-30","2022-07-31")), breaks = "1 years")
g9 <- createSimpleGGPlot(9)+ theme(legend.position = "none")+ggtitle("100 yens")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2017-06-30","2022-07-31")), breaks = "1 years")
g10 <- createSimpleGGPlot(10)+ theme(legend.position = "none")+ggtitle("Dow Jones")+ 
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2017-06-30","2022-07-31")), breaks = "1 years")
g11 <- createSimpleGGPlot(11)+ theme(legend.position = "none")+ggtitle("Nikkei 225")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2017-06-30","2022-07-31")), breaks = "1 years")
g12 <- createSimpleGGPlot(12)+ theme(legend.position = "none")+ggtitle("Eurostoxx")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2017-06-30","2022-07-31")), breaks = "1 years")


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
saveGrapheFiles(11, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(12, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(13, largeurCM = 18, hauteurCM = 6)
saveGrapheFiles(14, largeurCM = 18, hauteurCM = 6)
saveGrapheFiles(15, largeurCM = 18, hauteurCM = 6)


