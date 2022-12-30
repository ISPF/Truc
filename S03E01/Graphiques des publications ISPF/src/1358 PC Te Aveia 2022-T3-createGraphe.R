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
graphe1[,Hébergement:=as.numeric(Hébergement)]
graphe1[,Restauration:=as.numeric(Restauration)]
graphe1[,Transport:=as.numeric(Transport)]
graphe1[,`Commerce ; réparation d'automobiles et de motocycles`:=as.numeric(`Commerce ; réparation d'automobiles et de motocycles`)]
graphe1[,Construction:=as.numeric(Construction)]
graphe1[,Autres:=as.numeric(Autres)]
graphe1 <- melt(graphe1,id.vars="Date")
graphe1[,variable:=stringr::str_wrap(variable,20)]

g1 <- ggplot(graphe1,aes(x = Date,y=value, fill=variable))+
  geom_bar(data=graphe1[variable!="Indice Prix\nConsommation\ntrimestriel moyen"],
           mapping = aes(y = value/10^9),
           stat="identity", width=.8, position = "dodge")+
  geom_line(data=graphe1[variable=="Indice Prix\nConsommation\ntrimestriel moyen"],size=1.1,
            mapping = aes(x = factor(Date), y = ((value-97)/0.1),group=1, colour=variable))+
  scale_y_continuous(name = expression("Milliards de F.CFP"), 
                     labels = scales::number_format(accuracy = 1),
                     sec.axis = sec_axis(~ .*0.1+97  , 
                                         name = "",labels = scales::number_format(accuracy = 1),
                                         breaks = c(97,100,103,106,109)
                                         ))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank()) + 
  guides(col = guide_legend(ncol = 2)) +
  theme(legend.text=element_text(size=rel(0.6)))

g1




graphe2 <- readCSVFile(2)
graphe2 <- melt(graphe2,id.vars="Année")
graphe2[,variable:=stringr::str_wrap(variable,10)]


g2 <- ggplot(graphe2, aes(x = Année, y = value, color=variable, group=variable)) + 
  geom_line()+
  theme_ispf()+
  #scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  scale_fill_manual(values=ispfPalette)+
  xlab("")+ylab("")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #guides(col = guide_legend(ncol = 2))
g2


graphe3 <- readCSVFile(3)
graphe3 <- melt(graphe3,id.vars="Importations")
graphe3[,Importations:=stringr::str_wrap(Importations,15)]


g3 <- ggplot(graphe3, aes(x = factor(Importations), y = value, groupe=variable)) + 
  geom_bar(aes(fill=variable), stat="identity", position = "dodge")+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(-0.25,0.90)) +
  theme_ispf()+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank()) + 
  guides(col = guide_legend(ncol = 2))
g3


g4 <- createSimpleGGPlot(4) + ggtitle("Cours moyen du pétrole")
g5 <- createSimpleGGPlot(5)+ scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(80,190))+ theme(legend.position = "none")+ggtitle("Indice mensuel des matières\npremières alimentaires\n(Base 100, année 2010)")
g6 <- createSimpleGGPlot(6)+ scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(50,150))+theme(legend.position = "none")+ggtitle("Indice mensuel\nmétaux et minéraux\n(Base 100, année 2010)")
g7 <- createSimpleGGPlot(7)+ theme(legend.position = "none")+ggtitle("Dollar américain")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(90,130))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2017-06-30","2022-09-31")), breaks = "1 years")
g8 <- createSimpleGGPlot(8)+ ggtitle("Dollar australien et néo-zélandais")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2017-09-30","2022-09-31")), breaks = "1 years")
g9 <- createSimpleGGPlot(9)+ theme(legend.position = "none")+ggtitle("100 yens")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2017-06-30","2022-09-31")), breaks = "1 years")
g10 <- createSimpleGGPlot(10)+ theme(legend.position = "none")+ggtitle("Dow Jones")+ 
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2017-06-30","2022-09-31")), breaks = "1 years")
g11 <- createSimpleGGPlot(11)+ theme(legend.position = "none")+ggtitle("Nikkei 225")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2017-06-30","2022-12-31")), breaks = "1 years")
g12 <- createSimpleGGPlot(12)+ theme(legend.position = "none")+ggtitle("Eurostoxx")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2017-06-30","2022-09-31")), breaks = "1 years")


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


