graphe1 <- readCSVFile(1)
graphe1 <- melt(graphe1,id.vars="Année")

g1 <- ggplot(graphe1,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),
                     breaks = seq(1980,2020,5))+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  ylab("Millions de F.CFP")+
  theme(legend.position = "none")
g1

graphe2 <- readCSVFile(2)
graphe2 <- melt(graphe2,id.vars="Année")

g2 <- ggplot(graphe2,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(1980,2020,5))+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1), limits=c(0,4))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none")
g2


graphe3 <- readCSVFile(3)
graphe3 <- melt(graphe3,id.vars="Année")

g3 <- ggplot(graphe3,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(1980,2020,5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(-0.1,0.15))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none")
g3

graphe4 <- readCSVFile(4)
graphe4 <- melt(graphe4,id.vars="Année")
#graphe4[,variable:=factor(variable, levels=c("Paraha peue", "Crevette"))]

g4 <- ggplot(graphe4,aes(x = Année,y=value, color=variable)) +
  geom_line(size=1.1)+
  #geom_bar(mapping=aes(x = Année,y=value, fill=variable), stat="identity", position = "dodge")+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = c(1983,1988,1996,2002,2007,2012,2017))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom") +
  guides(col = guide_legend(ncol = 2))

g4

graphe5 <- readCSVFile(5)
graphe5 <- melt(graphe5,id.vars="Année")
g5 <- ggplot(graphe5,aes(x = Année,y=value, color=variable)) +
  geom_line(size=1.1)+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = c(1983,1988,1996,2002,2007,2012,2017))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),name = expression(""), limits=c(100,250))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")

g5

graphe6 <- readCSVFile(6)
graphe6 <- melt(graphe6,id.vars="Année")

g6 <- ggplot(graphe6,aes(x = Année,y=value, color=variable)) +
  geom_bar(mapping=aes(x = Année,y=value, fill=variable), stat="identity")+
  #scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = c(1983,1988,1996,2002,2007,2012,2017))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=rev(ispfPalette[1:5]))+
  scale_colour_manual(values=rev(ispfPalette[1:5]))+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 2))


g6


graphe7 <- readCSVFile(7)
graphe7 <- melt(graphe7,id.vars="Année")

g7 <- ggplot(graphe7,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(1980,2020,5))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(100,300))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none")
g7

graphe8 <- readCSVFile(8)
graphe8 <- melt(graphe8,id.vars="Année")

g8 <- ggplot(graphe8,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(1980,2020,5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")
g8


graphe9 <- readCSVFile(9)
graphe9 <- melt(graphe9,id.vars="Année")

g9 <- ggplot(graphe9,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(1980,2020,5))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")
g9



graphe10 <- readCSVFile(10)
graphe10 <- melt(graphe10,id.vars="Année")
graphe10[,variable:=stringr::str_wrap(variable, 15)]

g10 <- ggplot() + 
  geom_bar(data=graphe10[variable!="Taux de\ncouverture en\nbiens"], 
           mapping = aes(x = Année, y = value, fill=variable), stat = "identity", position="dodge")+ 
  geom_line(data=graphe10[variable=="Taux de\ncouverture en\nbiens"],
            mapping = aes(x = Année, y = value*10^6, colour=variable))+
  scale_y_continuous(name = expression("Millions de F.CFP"), 
                     labels = scales::number_format(accuracy = 1),
                     limits=c(0,260000),
                     sec.axis = sec_axis(~ . / 10^6 , name = "Taux de couverture",labels = scales::percent_format(accuracy = 1)))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank())
g10

graphe11 <- readCSVFile(11)
graphe11 <- melt(graphe11,id.vars="Année")

g11 <- ggplot(graphe11,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(1980,2020,5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none")
g11


graphe12 <- readCSVFile(12)
graphe12 <- melt(graphe12,id.vars="Année")
#graphe12[,variable:=stringr::str_wrap(variable, 15)]

g12 <- ggplot() + 
  geom_bar(data=graphe12[variable!="Prix moyen au gramme"], 
           mapping = aes(x = Année, y = value, fill=variable), stat = "identity", position="dodge")+ 
  geom_line(data=graphe12[variable=="Prix moyen au gramme"],
            mapping = aes(x = Année, y = value*2, colour=variable))+
  scale_y_continuous(name = expression("Millions de F.CFP"), 
                     labels = scales::number_format(accuracy = 1),
                     limits=c(0,28000),
                     sec.axis = sec_axis(~ ./2  , name = "Prix moyen au gramme",labels = scales::number_format(accuracy = 1)))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank())+
  guides(fill = guide_legend(nrow = 2))
g12


graphe13 <- readCSVFile(13)
graphe13 <- melt(graphe13,id.vars="Année")

g13 <- ggplot(graphe13,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(1980,2020,5))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),name = "Nombre" )+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")
g13


graphe14 <- readCSVFile(14)
graphe14 <- melt(graphe14,id.vars="Année")

g14 <- ggplot() + 
  geom_bar(data=graphe14[variable!="Recette touristique"], 
           mapping = aes(x = Année, y = value, fill=variable), stat = "identity", position="dodge")+ 
  geom_line(data=graphe14[variable=="Recette touristique"],size=1.1,
            mapping = aes(x = Année, y = value*2, colour=variable))+
  scale_y_continuous(name = expression("Nombre"), 
                     labels = scales::number_format(accuracy = 1),
                     sec.axis = sec_axis(~ ./2  , name = "Millions de F.CFP",labels = scales::number_format(accuracy = 1)))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank())
g14

graphe15 <- readCSVFile(15)
graphe15 <- melt(graphe15,id.vars="Année")
g15 <- ggplot() + 
  geom_bar(data=graphe15[variable!="Dépenses touristiques/PIB"], 
           mapping = aes(x = Année, y = value, fill=variable), stat = "identity", position="dodge")+ 
  geom_line(data=graphe15[variable=="Dépenses touristiques/PIB"],size=1.1,
            mapping = aes(x = Année, y = value*20000, colour=variable))+
  scale_y_continuous(name = expression("Nombre"), 
                     labels = scales::number_format(accuracy = 1),
                     sec.axis = sec_axis(~ ./20000, name = "Poids des dépenses touristiques en % ",labels = scales::percent_format(accuracy = 1)))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank())
g15


graphe16 <- readCSVFile(16)
colnames(graphe16) <- c("Année", "France", "Amérique du nord")
graphe16 <- melt(graphe16,id.vars="Année")

g16 <- ggplot(graphe16,aes(x = Année,y=value, color=variable, fill=variable)) +
  geom_area()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(1980,2020,5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  labs(caption = "Domicile permanent après 1995")+
  theme(legend.position = "bottom")
g16


graphe17 <- readCSVFile(17)
graphe17 <- melt(graphe17,id.vars="Année")
g17 <- ggplot() + 
  geom_bar(data=graphe17[variable!="Population"], 
           mapping = aes(x = Année, y = value*300000, fill=variable), stat = "identity")+ 
  geom_line(data=graphe17[variable=="Population"],mapping = aes(x = Année, y = value, colour=variable))+
  scale_y_continuous(name = expression("Nombre"), 
                     labels = scales::number_format(accuracy = 1),limits=c(0,300000),
                     sec.axis = sec_axis(~ ./300000  , name = "Part de la population",labels = scales::percent_format(accuracy = 1)))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank())
g17


graphe18 <- readCSVFile(18)
graphe18 <- melt(graphe18,id.vars="Année")
graphe18[,variable:=stringr::str_wrap(variable, 25)]

g18 <- ggplot(graphe18,aes(x = Année,y=value, color=variable)) +
  #geom_line(size=1.1)+
  geom_bar(mapping=aes(x = Année,y=value, fill=variable), stat="identity", position = "dodge")+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = c(1983,1988,1996,2002,2007,2012,2017))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")

g18



graphe19 <- readCSVFile(19)
graphe19 <- melt(graphe19,id.vars="Année")
g19 <- ggplot(graphe19,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(1985,2020,10))+
  scale_y_continuous(name = ("Années"),labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(ncol = 2))
g19



graphe20 <- readCSVFile(20)
graphe20 <- melt(graphe20,id.vars="Année")

g20 <- ggplot(graphe20,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(1980,2020,5))+
  scale_y_continuous(name = ("Années"),labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")
g20


graphe21 <- readCSVFile(21)
graphe21 <- melt(graphe21,id.vars="Année")

g21 <- ggplot(graphe21,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(1985,2020,10))+
  scale_y_continuous(name = "Taux de mortalité infantile en ???",labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(ncol = 2))
g21



saveGrapheFiles(1, largeurCM = 9, hauteurCM = 4.5)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(5, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(6, largeurCM = 9, hauteurCM = 9)
saveGrapheFiles(7, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(8, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(9, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(10, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(11, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(12, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(13, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(14, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(15, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(16, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(17, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(18, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(19, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(20, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(21, largeurCM = 9, hauteurCM = 6)
