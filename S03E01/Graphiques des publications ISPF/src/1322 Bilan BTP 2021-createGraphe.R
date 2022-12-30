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
graphe1[,`Chiffre d'affaires`:=as.integer(`Chiffre d'affaires`/10^9)]
graphe1 <- melt(graphe1,id.vars="Année")

g1 <- ggplot() + 
  geom_bar(data=graphe1[variable!="Nombre d'entreprises"], 
           mapping = aes(x = factor(Année), y = value, fill=variable), stat="identity", position="dodge")+ 
  geom_line(data=graphe1[variable=="Nombre d'entreprises"],size=1.1,
            mapping = aes(x = factor(Année), y = value*0.01,group=1, colour=variable))+
  scale_y_continuous(name = expression("Milliards de F.CFP"), 
                     labels = scales::number_format(accuracy = 1),
                     sec.axis = sec_axis(~ ./0.01  , name = "Nombre",labels = scales::number_format(accuracy = 1)))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank())
g1



graphe3 <- readCSVFile(3)
graphe3 <- melt(graphe3,id.vars="Trimestre")

g3 <- ggplot(graphe3,aes(x = Trimestre,y=value, color=variable, group=1)) +
  geom_line()+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1), limits=c(-1.2,1.1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position = "none")
g3



  graphe2 <- readCSVFile(2)
  graphe2 <- melt(graphe2,id.vars="Année")
  graphe2[, Date:=as.Date(Date)]
  
  
  g2 <- ggplot(graphe2) +
    aes(x = Année,y=value, color=variable) +
    geom_line()+
    scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),breaks = seq(2016,2021,1))+
    scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(90,130))+
    scale_fill_manual(values=ispfPalette)+
    scale_colour_manual(values=ispfPalette)+
    theme_ispf()+
    xlab("")+  ylab("")+ theme_ispf()+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
    theme(legend.position = "bottom",legend.direction = "horizontal")+
          theme(axis.title.x = element_blank())
  g2


saveGrapheFiles(1, largeurCM = 9, hauteurCM = 4)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 4)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 5)

