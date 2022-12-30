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
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1), limits=c(-1,1.5))+
  theme(legend.position = "bottom", legend.direction = "horizontal")
g1

# colnames(graphe2)[2:3] <- c("Volume", "Prix moyen au gramme")


graphe2 <- readCSVFile(2)
graphe2[,`Recettes à l'export`:=as.integer(`Recettes à l'export`/10^6)]
graphe2 <- melt(graphe2,id.vars="Année")
graphe2[,variable:=stringr::str_wrap(variable,20)]


g2 <- ggplot() + 
  geom_bar(data=graphe2[variable!="Recettes à l'export"], 
           mapping = aes(x = factor(Année), y = value, fill=variable), stat="identity", position="dodge")+ 
  geom_line(data=graphe2[variable=="Recettes à l'export"],size=1.1,
            mapping = aes(x = factor(Année), y = value*500,group=1, colour=variable))+
  scale_y_continuous(name = expression("Prix au kilo"), 
                     labels = scales::number_format(accuracy = 1),
                     limits=c(0,100000),
                     sec.axis = sec_axis(~ ./500  , name = "Milliards de F.CFP",labels = scales::number_format(accuracy = 1),))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank()) + 
  guides(col = guide_legend(ncol = 2))
g2


graphe3 <- readCSVFile(3)
graphe3 <- melt(graphe3,id.vars="Date")
graphe3[, Date:=as.Date(Date)]


g3 <- ggplot(graphe3) +
  aes(x = Date, y = value, group = variable, color=variable) +
  geom_line(size = 1) +
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_x_date(date_labels="%Y", breaks="1 year")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), 
                     limits=c(2000,6000))+
  xlab("123")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g3



g4 <- createSimpleGGPlot(4) + ggtitle("Cours moyen du pétrole")
g5 <- createSimpleGGPlot(5)+ theme(legend.position = "none")+ggtitle("Indice mensuel des matières\npremières alimentaires\n(Base 100, année 2010)")
g6 <- createSimpleGGPlot(6)+ theme(legend.position = "none")+ggtitle("Indice mensuel\nmétaux et minéraux\n(Base 100, année 2010)")
g7 <- createSimpleGGPlot(7)+ theme(legend.position = "none")+ggtitle("Dollar américain")
g8 <- createSimpleGGPlot(8)+ ggtitle("Dollar australien et néo-zélandais")
g9 <- createSimpleGGPlot(9)+ theme(legend.position = "none")+ggtitle("100 yens")
g10 <- createSimpleGGPlot(10)+ theme(legend.position = "none")+ggtitle("Dow Jones")+ 
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2020-06-01","2021-09-30")), breaks = "3 months")
g11 <- createSimpleGGPlot(11)+ theme(legend.position = "none")+ggtitle("Nikkei 225")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2020-06-01","2021-09-30")), breaks = "3 months")
g12 <- createSimpleGGPlot(12)+ theme(legend.position = "none")+ggtitle("Eurostoxx")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2020-06-01","2021-09-30")), breaks = "3 months")


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
saveGrapheFiles(16, largeurCM = 18, hauteurCM = 6)

