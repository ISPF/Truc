graphe1 <- readCSVFile(1)
graphe1[, Année:=as.Date(sprintf("%s-01-01", Année))]
graphe1[, `Exportations (F.CFP)`:=as.integer(`Exportations (F.CFP)`/10^6)]
graphe1[, `Exportations (kilos)`:=as.integer(`Exportations (kilos)`/10^3)]
colnames(graphe1)[3] <- "Exportations (Tonnes)"
grapheImpExp <- melt(graphe1,id.vars="Année")

g1 <- ggplot() + 
  geom_bar(data=grapheImpExp[variable=="Exportations (F.CFP)"], 
           mapping = aes(x = Année, y = value, fill="Exportations (F.CFP)"), stat = "identity" )+ 
  geom_line(data=grapheImpExp[variable=="Exportations (Tonnes)"],
            mapping = aes(x = Année, y = value, colour="Exportations (Tonnes)"))+
  scale_x_date(date_labels = "%Y",
               breaks = seq(min(grapheImpExp$Année), max(grapheImpExp$Année),by = "1 year"))+
  
  scale_y_continuous(name = "Millions de F.CFP", labels = scales::number_format(accuracy = 1), limits=c(0,2000),
                     sec.axis = sec_axis(~., name = "Tonnes",
                                         labels = scales::number_format(accuracy = 1)))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank())
g1


graphe2 <- readCSVFile(2)
graphe2 <- melt(graphe2,id.vars="Année")
graphe2[, variable:=factor(stringr::str_wrap(variable,18))]


g2 <- ggplot(graphe2,aes(x = Année,y=value, color=variable)) +
  geom_bar(mapping=aes(x = Année,y=value, fill=variable), stat="identity")+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),
                     breaks = seq(2002,2020,2))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")
g2


graphe3 <- readCSVFile(3)
graphe3[,`Valeurs`:=as.integer(`Valeurs`/10^6)]
graphe3 <- melt(graphe3,id.vars="Année")
graphe3[,variable:=stringr::str_wrap(variable,20)]


g3 <- ggplot() + 
  geom_bar(data=graphe3[variable!="Valeurs"], 
           mapping = aes(x = factor(Année), y = value, fill=variable), stat="identity", position="dodge")+ 
  geom_line(data=graphe3[variable=="Valeurs"],size=1.1,
            mapping = aes(x = factor(Année), y = value*1000,group=1, colour=variable))+
  scale_y_continuous(name = expression("Poids"), 
                     labels = scales::number_format(accuracy = 1),
                     limits=c(0,60000),
                     sec.axis = sec_axis(~ ./1000  , name = "Milliards de F.CFP",labels = scales::number_format(accuracy = 1),))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank()) + 
  guides(col = guide_legend(ncol = 2))
g3







###graphe3 <- readCSVFile(3)
graphe3 <- melt(graphe3,id.vars="Année")
graphe3[, variable:=factor(stringr::str_wrap(variable,18))]

g3 <- ggplot(graphe3) +
  geom_bar(mapping=aes(x = Année,y=value, fill=variable), stat="identity")+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),
                     breaks = seq(2011,2020,2))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     breaks=seq(0,8000,20000000),
                     limits=c(0,80000000))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")
g3

#graphe4 <- readCSVFile(4)
#graphe4 <- melt(graphe4,id.vars="Année")
#graphe4[,variable:=factor(variable, levels=c("Paraha peue", "Crevette"))]


#g4 <- ggplot(graphe4) +
  #geom_bar(mapping=aes(x = Année,y=value, fill=variable), stat="identity")+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),
                     breaks = c(2008:2020))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     breaks=seq(0,160,20))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")
g4

saveGrapheFiles(1, largeurCM = 18, hauteurCM = 6)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 7)
#saveGrapheFiles(4, largeurCM = 9, hauteurCM = 7)
