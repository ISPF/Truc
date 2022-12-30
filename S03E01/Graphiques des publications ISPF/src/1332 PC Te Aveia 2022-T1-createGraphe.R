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




# colnames(graphe2)[2:3] <- c("Volume", "Prix moyen au gramme")

options(scipen=999)

graphe1 <- readCSVFile(1)
##graphe1[,`Chiffre d'affaires autres secteurs`:=trunc(`Chiffre d'affaires autres secteurs`/1000000000)]
#graphe1[,`Chiffre d'affaires Tourisme`:=trunc(`Chiffre d'affaires Tourisme`/1000000000)]
graphe1[,`Indice des prix trimestriel moyen`:=`Indice des prix trimestriel moyen`*25-2400]
graphe1 <- melt(graphe1,id.vars="Année")
graphe1[,variable:=stringr::str_wrap(variable,10)]


g1 <- ggplot() + 
  geom_bar(data=graphe1[variable!="Indice\ndes prix\ntrimestriel\nmoyen"], 
           mapping = aes(x = factor(Année), y = value, fill=variable), stat="identity")+
  geom_text(data=graphe1[variable!="Indice\ndes prix\ntrimestriel\nmoyen"],aes(x = factor(Année), y = value, label=scales::number(value,accuracy = 1)),  position = position_dodge(width=0.8),color="black", size=2)+
  geom_line(data=graphe1[variable=="Indice\ndes prix\ntrimestriel\nmoyen"],size=1,
            mapping = aes(x = factor(Année), y = value, group=1,colour=variable))+
  scale_y_continuous(name = expression("Milliards de F.CFP"), 
                     labels = scales::number_format(accuracy = 1),
                     limits=c(0,200),
                     sec.axis = sec_axis(~ (.+2400)/25, name = "Points",labels = scales::number_format(accuracy = 1),))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank()) + 
  guides(col = guide_legend(ncol = 2))
g1


  options(scipen=999)
  
  graphe2 <- readCSVFile(2)
  #graphe2 <- fread("graphe2.txt", encoding = "UTF-8")
  graphe2 <- melt(graphe2,id.vars="Année")
  graphe2[,variable:=stringr::str_wrap(variable,10)]
  CA <- c("CA\nCommerce\nde\ndétail, à\nl'exception\ndes\nautomobiles\net des\nmotocycles","CA\nCommerce\net\nréparation\nd'automobiles\net de\nmotocycles")
  
  
  g2 <- ggplot() +
    geom_bar(data=graphe2[variable%in%CA], 
             mapping = aes(x = factor(Année), y = value, fill=variable), stat="identity")+ 
    geom_line(data=graphe2[variable=="Valeur\nimportations\nà\ndestination\ndes\nménages" && "Masse\nsalariale"],size=1,
              mapping = aes(x = factor(Année), y = value, colour=variable, group=2))+
    scale_y_continuous(#name = expression(""), 
                       labels = scales::number_format(accuracy = 1),
                       limits=c(0,125),)+
                    
    scale_fill_manual(values=ispfPalette)+
    theme_ispf()+
    xlab("")+ylab("")+
    theme(legend.position = "bottom",legend.direction = "horizontal",
          axis.title.x = element_blank()) + 
    guides(col = guide_legend(ncol = 2))
  
  g2


# graphe 2 initial
graphe2 <- readCSVFile(2)
graphe2 <- fread("graphe2.txt", encoding = "UTF-8")
graphe2 <- melt(graphe2,id.vars="Année")
graphe2[,Année:=stringr::str_wrap(Année,20)]
  

g2 <- ggplot() +
  aes(x = Année, y = value, group = variable, color=variable) +
  geom_line(size = 0.5) +
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(0,200))+
  xlab("")+   ylab("Indice")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g2


g3 <- createSimpleGGPlot(3) + ggtitle("Cours moyen du pétrole")
g4 <- createSimpleGGPlot(4)+ theme(legend.position = "none")+ggtitle("Indice mensuel des matières\npremières alimentaires\n(Base 100, année 2010)")
g5 <- createSimpleGGPlot(5)+ theme(legend.position = "none")+ggtitle("Indice mensuel\nmétaux et minéraux\n(Base 100, année 2010)")
g6 <- createSimpleGGPlot(6)+ theme(legend.position = "none")+ggtitle("Dollar américain")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2018-03-31","2022-03-31")), breaks = "1 years")
g7 <- createSimpleGGPlot(7)+ ggtitle("Dollar australien et néo-zélandais")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2018-03-31","2022-03-31")), breaks = "1 years")
g8 <- createSimpleGGPlot(8)+ theme(legend.position = "none")+ggtitle("100 yens")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2018-03-31","2022-03-31")), breaks = "1 years")
g9 <- createSimpleGGPlot(9)+ theme(legend.position = "none")+ggtitle("Dow Jones")+ 
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2021-03-31","2022-03-31")), breaks = "3 months")
g10 <- createSimpleGGPlot(10)+ theme(legend.position = "none")+ggtitle("Nikkei 225")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2021-03-31","2022-03-31")), breaks = "3 months")
g11 <- createSimpleGGPlot(11)+ theme(legend.position = "none")+ggtitle("Eurostoxx")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_date(date_labels = "%m/%y", limits=as.Date(c("2021-03-31","2022-03-31")), breaks = "3 months")


g12 <- grid.arrange(g3,g4,g5, ncol=3, nrow = 1)
g13 <- grid.arrange(g6,g7,g8, ncol=3, nrow = 1)
g14 <- grid.arrange(g9,g10,g11, ncol=3, nrow = 1)

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
saveGrapheFiles(12, largeurCM = 18, hauteurCM = 6)
saveGrapheFiles(13, largeurCM = 18, hauteurCM = 6)
saveGrapheFiles(14, largeurCM = 18, hauteurCM = 6)


