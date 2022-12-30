graphe1 <- readCSVFile(1)
graphe1 <- melt(graphe1,id.vars="Année")
g1 <- ggplot(graphe1,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(2010,2019,2))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0.03,0.15),)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")

g1

graphe2 <- readCSVFile(2)
colnames(graphe2) <- c("Région", "2018", "2019")
graphe2 <- melt(graphe2,id.vars="Région")
graphe2[,Région:=factor(Région, levels=rev(c("Etats-Unis d'Amérique","France","Chine","Corée du Sud","Nouvelle-Zélande")))]


g2 <- ggplot(graphe2,aes(x = Région,y=value, color=variable)) +
  geom_bar(mapping=aes(x = Région,y=value, fill=variable), stat="identity", position="dodge")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")+
  coord_flip()
  
g2


graphe3 <- readCSVFile(3)
graphe3 <- melt(graphe3,id = "Année")
#graphe3[,variable:=stringr::str_wrap(variable, 20)]


g3 <- ggplot() + 
  geom_bar(data=graphe3[variable!="Perles brutes en volume"], 
           mapping = aes(x = Année, y = value, fill=variable), stat = "identity")+ 
   geom_line(data=graphe3[variable=="Perles brutes en volume"],
            mapping = aes(x = Année, y = value*500, colour=variable), size=1)+
  scale_y_continuous(name = expression("Millions de F.CFP"), 
                     labels = scales::number_format(accuracy = 1),
                           sec.axis = sec_axis(~ ./500 , name = "Tonnes",labels = scales::number_format(accuracy = 1)))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank())+
  guides(fill = guide_legend(nrow = 3))
g3
#OK

graphe4 <- readCSVFile(4)
graphe4 <- melt(graphe4,id = "Année")
graphe4[,variable:=stringr::str_wrap(variable, 15)]

g4 <- ggplot(graphe4,aes(x = Année,y=value/1000, fill = variable, group = variable, weight = value, color=variable)) +
  geom_bar(stat = "identity")+
  #scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(2010,2020,2))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(0,2000))+
  scale_fill_manual(values=rev(ispfPalette[1:4]))+
  scale_colour_manual(values=rev(ispfPalette[1:4]))+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")
g4
#OK

graphe5 <- readCSVFile(5)
colnames(graphe5) <- c("Région", "2018","2019")
graphe5 <- melt(graphe5,id = "Région")
graphe5[,Région:=factor(Région, levels=c("Chine","France","Japon","Etats-Unis d'Amérique","Hong-Kong"))]


g5 <- ggplot(graphe5,aes(x = Région,y=value, fill = variable, group = variable, weight = value, color=variable)) +
  geom_bar(stat = "identity", position=position_dodge())+
  geom_text(aes(label=scales::number(value, accuracy = 1)), size=2, hjust=1, color="white", position = position_dodge(0.9))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(0,4500))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+ylab("")+
  coord_flip()+
  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g5
#OK



saveGrapheFiles(1, largeurCM = 9, hauteurCM = 4.5)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(5, largeurCM = 9, hauteurCM = 5)

