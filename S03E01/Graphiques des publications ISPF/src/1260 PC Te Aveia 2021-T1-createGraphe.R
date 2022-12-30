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

# graphe1 <- readCSVFile(1)
# graphe1 <- melt(graphe1, id.vars="Trimestre")
# g1 <- ggplot(data=graphe1) +
#   geom_bar(mapping = aes(x=Trimestre, y=value, fill=variable), stat = "identity", position="dodge" )+
#   scale_fill_manual(values=ispfPalette)+
#   theme_ispf()+
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
#   ylab("")+
#   xlab("")
# 
# g1

g1 <- createGenericGraphe(1, keyList = "Trimestre", 
                          str_wrap_cle = 20, legendPosition = "bottom", legendDirection = "horizontal",
                          ylabelFunction = scales::percent_format(accuracy = 1))+
  geom_bar(mapping = aes(x=Trimestre, y=value, fill=variable), stat = "identity", position="dodge" )

g1

graphe2 <- readCSVFile(2)
colnames(graphe2)[2:3] <- c("Volume", "Prix moyen au gramme")
graphe2 <- melt(graphe2, id.vars="Trimestre")
g2 <- ggplot() + 
  geom_bar(data=graphe2[variable=="Volume"], 
           mapping = aes(x = Trimestre, y = value, fill="Volume"), stat = "identity" )+ 
  geom_line(data=graphe2[variable=="Prix moyen au gramme"],
            mapping = aes(x = Trimestre, y = value*10/600, group="Prix moyen au gramme", colour="Prix moyen au gramme"))+
  scale_y_continuous(name = expression("Volume (Tonne)"), 
                     labels = scales::number_format(accuracy = 1),
                     limits=c(0,17),
                     sec.axis = sec_axis(~ . * 600 / 10 , name = "Prix moyen au gramme (F.CFP)",labels = scales::number_format(accuracy = 1)))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g2

g3 <- createGenericGraphe(3, keyList = "Trimestre", 
                    str_wrap_variable = 30, axis.text.x.angle = 90,
                    legendPosition = "bottom", legendDirection = "horizontal")+
  geom_line(mapping = aes(x=Trimestre, y=value, group=variable, color=variable))

g4 <- createSimpleGGPlot(4) + ggtitle("Cours moyen du pétrole")
g5 <- createSimpleGGPlot(5)+ theme(legend.position = "none")+ggtitle("Indice mensuel des matières\npremières alimentaires\n(Base 100, année 2010)")
g6 <- createSimpleGGPlot(6)+ theme(legend.position = "none")+ggtitle("Indice mensuel\nmétaux et minéraux\n(Base 100, année 2010)")
g7 <- createSimpleGGPlot(7)+ theme(legend.position = "none")+ggtitle("Dollar américain")
g8 <- createSimpleGGPlot(8)+ ggtitle("Dollar australien et néo-zélandais")
g9 <- createSimpleGGPlot(9)+ theme(legend.position = "none")+ggtitle("100 yens")
g10 <- createSimpleGGPlot(10)+ theme(legend.position = "none")+ggtitle("Dow Jones")+ 
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2019-11-01","2020-12-30")), breaks = "2 months")
g11 <- createSimpleGGPlot(11)+ theme(legend.position = "none")+ggtitle("Nikkei 225")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2019-11-01","2020-12-30")), breaks = "2 months")
g12 <- createSimpleGGPlot(12)+ theme(legend.position = "none")+ggtitle("Eurostoxx")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2019-11-01","2020-12-30")), breaks = "2 months")


g13 <- grid.arrange(g4,g5,g6, ncol=3, nrow = 1)
g14 <- grid.arrange(g7,g8,g9, ncol=3, nrow = 1)
g15 <- grid.arrange(g10,g11,g12, ncol=3, nrow = 1)

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

saveGrapheFiles(13, largeurCM = 18, hauteurCM = 6)
saveGrapheFiles(14, largeurCM = 18, hauteurCM = 6)
saveGrapheFiles(15, largeurCM = 18, hauteurCM = 6)

